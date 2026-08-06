#' Merge Exclusively Mapped Observation Variables
#'
#' @description
#' Merges two complementary field variables into a single continuous target variable.
#' 
#' @details
#' By default, enforces a strict Mutual Exclusivity rule: A single simulation cannot have 
#' values for both source variables on the same date. If `prior_var` is NULL, collisions 
#' crash the pipeline. If `prior_var` is set to "var_1" or "var_2", collisions are resolved 
#' by keeping the specified variable, and a warning is issued instead of a fatal crash.
#'
#' @param df_obs Dataframe. The wide observation dataset.
#' @param var_final Character. The final target column name to create or overwrite.
#' @param var_1 Character. The first source variable.
#' @param var_2 Character. The second source variable.
#' @param del_vars1_2 Logical. If TRUE, deletes the source variables after merging. Defaults to FALSE.
#' @param prior_var Character or NULL. Resolves collisions. Accepts "var_1", "var_2", or NULL (default).
#'
#' @return A dataframe with the merged target column.
#' @export
merge_obs_variables <- function(df_obs, var_final, var_1, var_2, del_vars1_2 = FALSE, prior_var = NULL) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # ------------------------------------------------------------------
  # 1. DEFENSIVE SETUP
  # ------------------------------------------------------------------
  if (!"SimulationName" %in% names(df_obs)) stop("CRITICAL: Missing 'SimulationName'.")
  
  date_col <- intersect(c("Clock.Today", "Date"), names(df_obs))[1]
  if (is.na(date_col)) stop("CRITICAL: Missing Date/Clock.Today column.")
  
  # Validate prior_var argument
  if (!is.null(prior_var)) {
    # Allow loose typing (e.g., "var1" instead of "var_1")
    if (prior_var == "var1") prior_var <- "var_1"
    if (prior_var == "var2") prior_var <- "var_2"
    if (!prior_var %in% c("var_1", "var_2")) {
      stop("CRITICAL: 'prior_var' must be NULL, 'var_1', or 'var_2'.")
    }
  }
  
  # If the source columns don't exist at all, create them as pure NA to prevent crashes
  if (!var_1 %in% names(df_obs)) df_obs[[var_1]] <- NA_real_
  if (!var_2 %in% names(df_obs)) df_obs[[var_2]] <- NA_real_
  
  # ------------------------------------------------------------------
  # 2. THE COLLISION GUARD & PRIORITY RESOLUTION
  # ------------------------------------------------------------------
  # Find any rows where BOTH variables have a recorded numeric value
  collisions <- df_obs %>%
    dplyr::filter(!is.na(.data[[var_1]]) & !is.na(.data[[var_2]])) %>%
    dplyr::select(dplyr::all_of(c("SimulationName", date_col, var_1, var_2)))
  
  if (nrow(collisions) > 0) {
    fail_log <- capture.output(print(collisions))
    
    if (is.null(prior_var)) {
      # FATAL CRASH (Default Behavior)
      crash_msg <- c(
        "",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        " \U0001F6A8 CRITICAL ALARM: VARIABLE COLLISION DETECTED \U0001F6A8",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        sprintf(" RULE VIOLATION: '%s' and '%s' both contain", var_1, var_2),
        "                 data on the exact same Date for the same Simulation.",
        " ACTION        : Pipeline halted to prevent data double-counting.",
        " SOLUTION      : Use 'prior_var' to resolve automatically, or fix raw data.",
        "----------------------------------------------------------------------",
        " FAILURE LOCATIONS:",
        fail_log,
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        ""
      )
      stop(paste(crash_msg, collapse = "\n"), call. = FALSE)
      
    } else {
      # WARNING & OVERRIDE (Priority Resolution Behavior)
      priority_col_name <- ifelse(prior_var == "var_1", var_1, var_2)
      warn_msg <- c(
        "",
        "======================================================================",
        " \u26A0\uFE0F COLLISION DETECTED & AUTOMATICALLY RESOLVED \u26A0\uFE0F ",
        "======================================================================",
        sprintf(" -> Conflict     : '%s' and '%s' overlapped on %d row(s).", var_1, var_2, nrow(collisions)),
        sprintf(" -> Resolution   : Overwrote tie using '%s' (based on prior_var).", priority_col_name),
        "======================================================================",
        ""
      )
      message(paste(warn_msg, collapse = "\n"))
      
      tryCatch({
        log_qflag(
          severity = "WARN", 
          category = "DATA MERGED", 
          message = sprintf("Resolved %d timeline collisions by prioritizing '%s'.", nrow(collisions), priority_col_name)
        )
      }, error = function(e) {})
      
      warning("Variable collision resolved via 'prior_var' override. See console for details.", call. = FALSE)
    }
  }
  
  # ------------------------------------------------------------------
  # 3. THE MERGE (DYNAMIC COALESCE)
  # ------------------------------------------------------------------
  df_out <- df_obs %>%
    dplyr::mutate(
      !!var_final := dplyr::case_when(
        # If prior_var is var_2, evaluate var_2 first
        !is.null(prior_var) && prior_var == "var_2" ~ dplyr::coalesce(.data[[var_2]], .data[[var_1]]),
        
        # Default behavior (and prior_var == "var_1"): evaluate var_1 first
        TRUE ~ dplyr::coalesce(.data[[var_1]], .data[[var_2]])
      )
    )
  
  # ------------------------------------------------------------------
  # 4. CLEANUP (Optional)
  # ------------------------------------------------------------------
  if (del_vars1_2) {
    cols_to_drop <- setdiff(c(var_1, var_2), var_final)
    if (length(cols_to_drop) > 0) {
      df_out <- df_out %>% dplyr::select(-dplyr::all_of(cols_to_drop))
    }
  }
  
  # ------------------------------------------------------------------
  # 5. AUDIT TRAIL LOGGING (Only if it didn't crash)
  # ------------------------------------------------------------------
  log_msg <- c(
    "",
    "======================================================================",
    " \U0001F527 VARIABLES MERGED: EXCLUSIVE TIMELINES ZIPPED \U0001F527",
    "======================================================================",
    sprintf(" -> Source 1     : '%s'", var_1),
    sprintf(" -> Source 2     : '%s'", var_2),
    sprintf(" -> Target Asset : '%s'", var_final),
    sprintf(" -> Priority Set : %s", ifelse(is.null(prior_var), "NONE (Strict Exclusivity)", prior_var)),
    sprintf(" -> Cleanup      : Source columns %s", ifelse(del_vars1_2, "DROPPED", "RETAINED")),
    " -> Status       : Timelines successfully merged.",
    "======================================================================",
    ""
  )
  message(paste(log_msg, collapse = "\n"))
  
  # Log standard info if no collision happened (otherwise the collision WARN flag covers it)
  if (nrow(collisions) == 0) {
    tryCatch({
      log_qflag(
        severity = "INFO", 
        category = "DATA MERGED", 
        message = sprintf("Zipped '%s' and '%s' into '%s' cleanly.", var_1, var_2, var_final)
      )
    }, error = function(e) {})
  }
  
  return(df_out)
}