#' Merge Exclusively Mapped Spike Variables
#'
#' @description
#' Merges two complementary field variables (e.g., in-season Spike and harvest Chaff)
#' into a single continuous APSIM target variable. 
#' 
#' @details
#' Enforces a strict Mutual Exclusivity rule: A single simulation cannot have 
#' values for both source variables on the same date. If a collision is detected, 
#' the pipeline halts immediately.
#'
#' @param df_obs Dataframe. The wide observation dataset.
#' @param spike1_var Character. The first source variable (e.g., "Wheat.Spike.Wt").
#' @param spike2_var Character. The second source variable (e.g., "Wheat.Chaff.Wt").
#' @param spike_apsim_var Character. The final target column name. Defaults to "Wheat.Spike.Wt".
#'
#' @return A dataframe with the merged target column.
#' @export
merge_spike_variables <- function(df_obs, spike1_var, spike2_var, spike_apsim_var = "Wheat.Spike.Wt") {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # ------------------------------------------------------------------
  # 1. DEFENSIVE SETUP
  # ------------------------------------------------------------------
  if (!"SimulationName" %in% names(df_obs)) stop("CRITICAL: Missing 'SimulationName'.")
  
  date_col <- intersect(c("Clock.Today", "Date"), names(df_obs))[1]
  if (is.na(date_col)) stop("CRITICAL: Missing Date/Clock.Today column.")
  
  # If the source columns don't exist at all, create them as pure NA to prevent crashes
  if (!spike1_var %in% names(df_obs)) df_obs[[spike1_var]] <- NA_real_
  if (!spike2_var %in% names(df_obs)) df_obs[[spike2_var]] <- NA_real_
  
  # ------------------------------------------------------------------
  # 2. THE COLLISION GUARD (MUTUAL EXCLUSIVITY CHECK)
  # ------------------------------------------------------------------
  # Find any rows where BOTH variables have a recorded numeric value
  collisions <- df_obs %>%
    dplyr::filter(!is.na(.data[[spike1_var]]) & !is.na(.data[[spike2_var]])) %>%
    dplyr::select(dplyr::all_of(c("SimulationName", date_col, spike1_var, spike2_var)))
  
  if (nrow(collisions) > 0) {
    
    # Format the exact location of the failures for the user
    fail_log <- capture.output(print(collisions))
    
    crash_msg <- c(
      "",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " \U0001F6A8 CRITICAL ALARM: SPIKE VARIABLE COLLISION DETECTED \U0001F6A8",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      sprintf(" RULE VIOLATION: '%s' and '%s' both contain", spike1_var, spike2_var),
      "                 data on the exact same Date for the same Simulation.",
      " ACTION        : Pipeline halted to prevent data double-counting.",
      " SOLUTION      : Check your raw data and remove the redundant overlap.",
      "----------------------------------------------------------------------",
      " FAILURE LOCATIONS:",
      fail_log,
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ""
    )
    
    stop(paste(crash_msg, collapse = "\n"), call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 3. THE MERGE (COALESCE)
  # ------------------------------------------------------------------
  df_out <- df_obs %>%
    dplyr::mutate(
      # Coalesce takes the first non-NA value it finds between the two columns
      !!spike_apsim_var := dplyr::coalesce(.data[[spike1_var]], .data[[spike2_var]])
    )
  
  # ------------------------------------------------------------------
  # 4. CLEANUP (Drop the old columns if they aren't the target)
  # ------------------------------------------------------------------
  cols_to_drop <- setdiff(c(spike1_var, spike2_var), spike_apsim_var)
  if (length(cols_to_drop) > 0) {
    df_out <- df_out %>% dplyr::select(-dplyr::all_of(cols_to_drop))
  }
  
  # ------------------------------------------------------------------
  # 5. AUDIT TRAIL LOGGING
  # ------------------------------------------------------------------
  log_msg <- c(
    "",
    "======================================================================",
    " \U0001F527 VARIABLES MERGED: EXCLUSIVE TIMELINES ZIPPED \U0001F527",
    "======================================================================",
    sprintf(" -> Source 1     : '%s'", spike1_var),
    sprintf(" -> Source 2     : '%s'", spike2_var),
    sprintf(" -> Target Asset : '%s'", spike_apsim_var),
    " -> Status       : Mutual exclusivity confirmed. Timelines merged.",
    "======================================================================",
    ""
  )
  message(paste(log_msg, collapse = "\n"))
  
  tryCatch({
    log_qflag(
      severity = "INFO", 
      category = "DATA MERGED", 
      message = sprintf("Zipped '%s' and '%s' into '%s'.", spike1_var, spike2_var, spike_apsim_var)
    )
  }, error = function(e) {})
  
  return(df_out)
}