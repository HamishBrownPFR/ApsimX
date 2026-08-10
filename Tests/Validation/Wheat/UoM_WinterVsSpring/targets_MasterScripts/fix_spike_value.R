#' Fix Spike Value (Chaff Swap) - Row-wise Edition
#'
#' @description
#' Resolves terminology inconsistencies between field data and APSIM. 
#' Validates that Chaff is strictly less than Spike when both exist.
#' If valid, for every SimulationName and Date combination where both values 
#' exist (and Chaff > 0), Spike is overwritten with the Chaff value.
#'
#' @param df_obs_wide Dataframe. The wide observation dataset.
#' @param spike_var Character string. The name of the APSIM Spike variable.
#' @param chaff_var Character string. The name of the raw Chaff variable.
#'
#' @return The corrected dataframe.
#' @export
fix_spike_value <- function(df_obs_wide, spike_var, chaff_var) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # 1. Defensive Checks
  if (!"SimulationName" %in% names(df_obs_wide)) stop("CRITICAL: Missing 'SimulationName'.")
  if (!spike_var %in% names(df_obs_wide)) stop(sprintf("CRITICAL: '%s' not found.", spike_var))
  if (!chaff_var %in% names(df_obs_wide)) stop(sprintf("CRITICAL: '%s' not found.", chaff_var))
  
  date_col <- intersect(c("Clock.Today", "Date"), names(df_obs_wide))[1]
  if (is.na(date_col)) stop("CRITICAL: Missing Date/Clock.Today column.")
  
  # 2. Validation: Chaff must be < Spike when both exist
  # "Exist" means Chaff is not NA and > 0, and Spike is not NA.
  violations <- df_obs_wide %>%
    dplyr::filter(
      !is.na(.data[[chaff_var]]) & .data[[chaff_var]] > 0,
      !is.na(.data[[spike_var]]),
      .data[[chaff_var]] >= .data[[spike_var]] # The failure condition
    )
  
  # If any violations exist, stop the script and report exactly where they are
  if (nrow(violations) > 0) {
    max_print <- min(5, nrow(violations))
    
    issue_details <- paste(
      paste0(" -> Sim: ", violations$SimulationName[1:max_print], 
             " | Date: ", violations[[date_col]][1:max_print], 
             " | ", chaff_var, ": ", violations[[chaff_var]][1:max_print], 
             " | ", spike_var, ": ", violations[[spike_var]][1:max_print]),
      collapse = "\n"
    )
    
    stop_msg <- sprintf(
      "CRITICAL DATA LOGIC FAILURE: '%s' must be strictly less than '%s'.\nFound %d violations. Showing first %d:\n%s",
      chaff_var, spike_var, nrow(violations), max_print, issue_details
    )
    stop(stop_msg)
  }
  
  # 3. The Row-wise Swap
  # Evaluates every row: if both exist (and chaff > 0), Spike becomes Chaff.
  df_out <- df_obs_wide %>%
    dplyr::mutate(
      !!spike_var := dplyr::if_else(
        !is.na(.data[[chaff_var]]) & .data[[chaff_var]] > 0 & !is.na(.data[[spike_var]]),
        .data[[chaff_var]],
        .data[[spike_var]]
      )
    )
  
  # 4. Audit Trail
  log_msg <- c(
    "",
    "======================================================================",
    " \U0001F527 TERMINOLOGY PATCH: SPIKE OVERWRITTEN BY CHAFF \U0001F527",
    "======================================================================",
    sprintf(" -> Target Spike Var : '%s'", spike_var),
    sprintf(" -> Source Chaff Var : '%s'", chaff_var),
    " -> Logic Applied    : Chaff < Spike validation passed.",
    "                       For every Sim/Date combination where both exist,",
    "                       Spike was successfully overwritten with Chaff.",
    "======================================================================",
    ""
  )
  message(paste(log_msg, collapse = "\n"))
  
  return(df_out)
}