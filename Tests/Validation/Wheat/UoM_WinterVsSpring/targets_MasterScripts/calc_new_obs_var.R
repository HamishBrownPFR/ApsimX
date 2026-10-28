library(dplyr)
library(rlang)
library(cli)
library(purrr)

#' Calculate a new observed variable with optional range validation and fuzzy date matching
#'
#' @description
#' Calculates mathematical relationships between two observation variables. Includes a 
#' state-of-the-art fuzzy date matcher: if dates between Var A and Var B are misaligned, 
#' it can fetch missing complementary values within a specified `tolerance_days` threshold.
#'
#' @param df_obs Dataframe containing the wide observed data.
#' @param col_name_A String. Name of the first column ('A').
#' @param col_name_B String. Name of the second column ('B').
#' @param oprt String. Mathematical operation to perform (e.g., "A/B").
#' @param col_name_Result String. Name of the new column.
#' @param val_range Optional. A numeric vector `c(min, max)` defining the acceptable boundaries.
#' @param tolerance_days Numeric. Maximum allowable day difference to fuzzily match variables A and B. Defaults to 0.
#' @param anchor_var Character. "A", "B", or "BOTH". If dates are misaligned, which variable's date should the result anchor to? Defaults to "A".
#' 
#' @return A dataframe with the newly calculated column.
#' @export
calc_new_obs_var <- function(df_obs, col_name_A, col_name_B, oprt, col_name_Result, val_range = NULL, tolerance_days = 0, anchor_var = "A") {
  
  # ====================================================================
  # 1. INPUT VALIDATION & SAFETY CHECKS
  # ====================================================================
  essential_cols <- c("SimulationName", "Clock.Today", col_name_A, col_name_B)
  missing_cols <- setdiff(essential_cols, names(df_obs))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing required columns in the dataset.",
      "i" = "Please ensure the following columns exist: {.var {missing_cols}}"
    ))
  }
  
  if (!is.character(df_obs$SimulationName) && !is.factor(df_obs$SimulationName)) cli::cli_abort("{.var SimulationName} must be a character or factor.")
  if (!inherits(df_obs$Clock.Today, c("Date", "POSIXt", "character"))) cli::cli_abort("{.var Clock.Today} must be a Date, POSIXt, or character format.")
  if (!is.numeric(df_obs[[col_name_A]])) cli::cli_abort("Column A ({.var {col_name_A}}) must be numeric.")
  if (!is.numeric(df_obs[[col_name_B]])) cli::cli_abort("Column B ({.var {col_name_B}}) must be numeric.")
  if (!anchor_var %in% c("A", "B", "BOTH")) cli::cli_abort("{.arg anchor_var} must be 'A', 'B', or 'BOTH'.")
  
  # Ensure Clock.Today is strict Date format to allow mathematical day differences
  df_obs$Clock.Today <- as.Date(df_obs$Clock.Today)
  
  if (!is.null(val_range)) {
    if (is.character(val_range) && length(val_range) == 1) val_range <- as.numeric(trimws(unlist(strsplit(val_range, ","))))
    if (!is.numeric(val_range) || length(val_range) != 2 || any(is.na(val_range))) cli::cli_abort("{.arg val_range} must be a numeric vector of length 2.")
    if (val_range[1] >= val_range[2]) cli::cli_abort("Minimum value must be strictly less than the maximum.")
  }
  
  # ====================================================================
  # 2. FUZZY DATE MATCHING ENGINE
  # ====================================================================
  approx_logs <- c()
  
  if (tolerance_days > 0) {
    df_obs <- df_obs %>%
      dplyr::mutate(
        .temp_A = .data[[col_name_A]],
        .temp_B = .data[[col_name_B]]
      )
    
    # Process group by group via split-map pattern
    df_obs <- df_obs %>%
      dplyr::group_split(SimulationName) %>%
      purrr::map_dfr(function(sim_df) {
        dates <- sim_df$Clock.Today
        
        for (i in seq_len(nrow(sim_df))) {
          # Condition 1: Anchor to A (Has A, but missing B) -> Fetch closest B
          if (anchor_var %in% c("A", "BOTH") && !is.na(sim_df$.temp_A[i]) && is.na(sim_df$.temp_B[i])) {
            valid_b <- which(!is.na(sim_df[[col_name_B]]))
            if (length(valid_b) > 0) {
              dists <- abs(as.numeric(difftime(dates[i], dates[valid_b], units = "days")))
              min_dist <- min(dists)
              if (min_dist > 0 && min_dist <= tolerance_days) {
                best_idx <- valid_b[which.min(dists)]
                sim_df$.temp_B[i] <- sim_df[[col_name_B]][best_idx] # Bring B into the row
                approx_logs <<- c(approx_logs, sprintf("     -> Sim: '%s' | Calc Date: %s | Fetched '%s' from %s (%d days off)", 
                                                       sim_df$SimulationName[1], format(dates[i], "%Y-%m-%d"), 
                                                       col_name_B, format(dates[best_idx], "%Y-%m-%d"), min_dist))
              }
            }
          }
          
          # Condition 2: Anchor to B (Has B, but missing A) -> Fetch closest A
          if (anchor_var %in% c("B", "BOTH") && !is.na(sim_df$.temp_B[i]) && is.na(sim_df$.temp_A[i])) {
            valid_a <- which(!is.na(sim_df[[col_name_A]]))
            if (length(valid_a) > 0) {
              dists <- abs(as.numeric(difftime(dates[i], dates[valid_a], units = "days")))
              min_dist <- min(dists)
              if (min_dist > 0 && min_dist <= tolerance_days) {
                best_idx <- valid_a[which.min(dists)]
                sim_df$.temp_A[i] <- sim_df[[col_name_A]][best_idx] # Bring A into the row
                approx_logs <<- c(approx_logs, sprintf("     -> Sim: '%s' | Calc Date: %s | Fetched '%s' from %s (%d days off)", 
                                                       sim_df$SimulationName[1], format(dates[i], "%Y-%m-%d"), 
                                                       col_name_A, format(dates[best_idx], "%Y-%m-%d"), min_dist))
              }
            }
          }
        }
        return(sim_df)
      })
  }
  
  # ====================================================================
  # 3. MATHEMATICAL OPERATION
  # ====================================================================
  expr_str <- gsub("sqr", "sqrt", oprt)
  
  if (tolerance_days > 0) {
    # If fuzzing, calculate using the temporary aligned variables
    expr_str <- gsub("\\bA\\b", "`.temp_A`", expr_str)
    expr_str <- gsub("\\bB\\b", "`.temp_B`", expr_str)
  } else {
    # Standard exact-date matching
    expr_str <- gsub("\\bA\\b", paste0("`", col_name_A, "`"), expr_str)
    expr_str <- gsub("\\bB\\b", paste0("`", col_name_B, "`"), expr_str)
  }
  
  tryCatch({
    parsed_expr <- rlang::parse_expr(expr_str)
    
    df_obs_out <- df_obs %>%
      dplyr::mutate("{col_name_Result}" := !!parsed_expr)
    
    # Drop temp columns cleanly so they don't pollute the final dataset
    if (tolerance_days > 0) df_obs_out <- df_obs_out %>% dplyr::select(-.temp_A, -.temp_B)
    
  }, error = function(e) {
    cli::cli_abort(c("x" = "Failed to evaluate the mathematical operation.", "i" = "Attempted to parse: {.code {expr_str}}", "!" = "Original R error: {e$message}"))
  })
  
  # ====================================================================
  # 4. REPORTING & QUARTO LOGGING
  # ====================================================================
  
  # A. Print Fuzzy Match Logs
  if (length(approx_logs) > 0) {
    cat(paste0(
      "\n============================================================\n",
      " \U0001F504 FUZZY DATE MATCHING APPLIED (Tolerance: ", tolerance_days, " days) \U0001F504 \n",
      "============================================================\n",
      paste(approx_logs, collapse = "\n"),
      "\n------------------------------------------------------------\n"
    ))
    
    tryCatch({
      log_qflag(severity = "INFO", category = "CALCULATIONS", 
                message = sprintf("Fuzzy date matching applied in '%s': Approximated %d calculation(s) within %d day(s) tolerance.", col_name_Result, length(approx_logs), tolerance_days))
    }, error = function(e) {})
  }
  
  # B. Range Check Validation
  if (!is.null(val_range)) {
    calculated_vals <- df_obs_out[[col_name_Result]]
    valid_vals <- calculated_vals[!is.na(calculated_vals)]
    
    if (length(valid_vals) == 0) {
      cli::cli_alert_info("Range check skipped: All calculated values for {.var {col_name_Result}} evaluated to {.val NA}.")
    } else {
      n_below <- sum(valid_vals < val_range[1])
      n_above <- sum(valid_vals > val_range[2])
      
      if (n_below == 0 && n_above == 0) {
        cat(paste0("\n============================================================\n", " \u2705  RANGE CHECK PASSED: ", toupper(col_name_Result), " \u2705 \n", "============================================================\n", " -> LIMITS       : ", val_range[1], " to ", val_range[2], "\n", " -> STATUS       : All calculated values are within boundaries.\n", "------------------------------------------------------------\n"))
      } else {
        msg <- paste0("\n============================================================\n", " \u26A0\uFE0F  RANGE VIOLATION DETECTED: ", toupper(col_name_Result), " \u26A0\uFE0F \n", "============================================================\n", " -> LIMITS       : ", val_range[1], " to ", val_range[2], "\n")
        if (n_below > 0) msg <- paste0(msg, " -> BELOW MIN    : ", n_below, " value(s) fell below ", val_range[1], "\n")
        if (n_above > 0) msg <- paste0(msg, " -> ABOVE MAX    : ", n_above, " value(s) exceeded ", val_range[2], "\n")
        cat(paste0(msg, "------------------------------------------------------------\n"))
        
        tryCatch({ log_qflag(severity = "WARN", category = "TARGETS", message = sprintf("Range Violation in %s: %d value(s) fell outside acceptable limits [%g, %g].", col_name_Result, n_below + n_above, val_range[1], val_range[2])) }, error = function(e) {})
      }
    }
  } else {
    cli::cli_alert_success("Successfully created column {.var {col_name_Result}}.")
  }
  
  return(df_obs_out)
}