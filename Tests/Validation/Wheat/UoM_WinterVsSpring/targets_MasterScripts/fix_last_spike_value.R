#' Fix Last Spike Value (Chaff Swap)
#'
#' @description
#' Resolves terminology inconsistencies between field data and APSIM. During the season, 
#' 'Spike' is measured, but at harvest, 'Chaff' is measured. This function finds the 
#' absolute last measurement date for the spike variable per simulation and forcefully 
#' overwrites it with the corresponding chaff variable value.
#'
#' @param df_obs_wide Dataframe. The wide observation dataset.
#' @param spike_var Character string. The name of the APSIM Spike variable (e.g., "Wheat.Spike.Wt").
#' @param chaff_var Character string. The name of the raw Chaff variable to swap in.
#'
#' @return The corrected dataframe with harmonized harvest spike values.
#' @export
fix_last_spike_value <- function(df_obs_wide, spike_var, chaff_var) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # 1. Defensive Checks
  if (!"SimulationName" %in% names(df_obs_wide)) stop("CRITICAL [fix_last_spike_value]: Missing 'SimulationName'.")
  if (!spike_var %in% names(df_obs_wide)) stop(sprintf("CRITICAL [fix_last_spike_value]: '%s' not found.", spike_var))
  if (!chaff_var %in% names(df_obs_wide)) stop(sprintf("CRITICAL [fix_last_spike_value]: '%s' not found.", chaff_var))
  
  date_col <- intersect(c("Clock.Today", "Date"), names(df_obs_wide))[1]
  if (is.na(date_col)) stop("CRITICAL [fix_last_spike_value]: Missing Date/Clock.Today column.")
  
  # 2. The Surgical Swap
  df_out <- df_obs_wide %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::mutate(
      # Safely find the max date where spike data actually exists for this sim
      .last_spike_date = suppressWarnings(max(.data[[date_col]][!is.na(.data[[spike_var]])], na.rm = TRUE)),
      
      # If it's the last date, overwrite Spike with Chaff. Otherwise, leave it alone.
      !!spike_var := dplyr::if_else(
        !is.infinite(.last_spike_date) & .data[[date_col]] == .last_spike_date,
        .data[[chaff_var]],
        .data[[spike_var]]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.last_spike_date) # Clean up the temporary column
  
  # 3. 🚨 THE AUDIT TRAIL 🚨
  log_msg <- c(
    "",
    "======================================================================",
    " \U0001F527 TERMINOLOGY PATCH: FINAL SPIKE MASS OVERWRITTEN BY CHAFF \U0001F527",
    "======================================================================",
    sprintf(" -> Target Spike Var : '%s'", spike_var),
    sprintf(" -> Source Chaff Var : '%s'", chaff_var),
    " -> Root Cause       : Model vs. Field naming consistency.",
    "                       (Spikes are threshed at harvest, leaving Chaff).",
    " -> Action Taken     : Identified the final measurement date per simulation",
    "                       and injected the Chaff value into the Spike column.",
    "======================================================================",
    ""
  )
  message(paste(log_msg, collapse = "\n"))
  
  return(df_out)
}