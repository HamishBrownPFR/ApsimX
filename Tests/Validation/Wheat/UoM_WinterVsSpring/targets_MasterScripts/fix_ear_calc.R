#' Fix Ear Calculations (Phase 1: Pass-Through)
#'
#' @description
#' A targeted function to handle ear mass calculations. Currently acts as a pass-through 
#' that copies the original ear variable into a new target variable column.
#'
#' @param df_obs_wide Dataframe. The wide observation dataset containing the raw data.
#' @param ear_orig_var_name Character string. The exact name of the source column (e.g., "Wheat.Ear.Wt").
#' @param ear_new_var_name Character string. The name for the newly generated column.
#'
#' @return The dataframe with the new column appended.
#' @export
fix_ear_calc <- function(df_obs_wide, ear_orig_var_name, ear_new_var_name) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # 1. Defensive Check: Ensure the original column actually exists
  if (!ear_orig_var_name %in% names(df_obs_wide)) {
    stop(sprintf("\n\U0001F6A8 CRITICAL ERROR [fix_ear_calc]: The source column '%s' was not found in the dataframe.", ear_orig_var_name), call. = FALSE)
  }
  
  # 2. Duplicate the column using tidy evaluation
  df_out <- df_obs_wide %>%
    dplyr::mutate(
      !!ear_new_var_name := .data[[ear_orig_var_name]]
    )
  
  # 3. 🚨 THE AUDIT TRAIL 🚨
  log_msg <- c(
    "",
    "======================================================================",
    " \u2699\ufe0f PIPELINE ACTION: EAR MASS VARIABLE INITIALIZED \u2699\ufe0f",
    "======================================================================",
    sprintf(" -> Source Column : '%s'", ear_orig_var_name),
    sprintf(" -> Target Column : '%s'", ear_new_var_name),
    " -> Action Taken  : Direct 1:1 copy (Phase 1 Pass-through).",
    " -> Status        : Ready for future mathematical enhancements.",
    "======================================================================",
    ""
  )
  message(paste(log_msg, collapse = "\n"))
  
  return(df_out)
}