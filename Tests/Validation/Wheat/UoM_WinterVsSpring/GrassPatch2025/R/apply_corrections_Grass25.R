#' Apply Specific Name and Data Corrections for GrassPatch2025
#'
#' @param df_obs Dataframe containing the observed data
#' @return Corrected dataframe
#' @export
apply_corrections_Grass25 <- function(df_obs) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # ------------------------------------------------------------------
  # 1. SIMULATION NAME OVERRIDE
  # ------------------------------------------------------------------
  if (!"SimulationName" %in% names(df_obs)) {
    stop("CRITICAL: 'SimulationName' column not found in the observed dataframe.")
  }
  
  target_name <- "FAR WAE W25-49"
  new_name    <- "GrassPatch2025"
  
  affected_rows <- sum(df_obs$SimulationName == target_name, na.rm = TRUE)
  
  if (affected_rows > 0) {
    message("\n", strrep("=", 60))
    message(" ⚠️  DATA CORRECTION APPLIED: SIMULATION NAME OVERRIDE ⚠️ ")
    message(strrep("=", 60))
    message(sprintf(" -> TARGET FOUND : '%s'", target_name))
    message(sprintf(" -> ACTION       : Renamed to '%s'", new_name))
    message(sprintf(" -> IMPACT       : %d rows updated", affected_rows))
    message(" -> NOTE         : This is a hardcoded fix for GrassPatch2025.")
    message(strrep("-", 60), "\n")
    
    df_obs <- df_obs %>%
      dplyr::mutate(
        SimulationName = ifelse(SimulationName == target_name, new_name, SimulationName)
      )
    
    # ---> NEW: Machine-readable Q-Flag for Simulation Name Override (Grass25)
    log_qflag(
      severity = "WARN", 
      category = "DATA MODIFIED", 
      message = sprintf("Simulation name override (Grass25): renamed '%s' to '%s' (%d rows updated).", target_name, new_name, affected_rows)
    )
  }
  
  # ------------------------------------------------------------------
  # 2. NUTRIENT CONCENTRATION FIX (Percentage to Fractional)
  # ------------------------------------------------------------------
  # Find all columns containing "NConc" or "WSC"
  target_cols <- grep("NConc|WSC", names(df_obs), value = TRUE)
  adjusted_cols <- character(0)
  
  for (col in target_cols) {
    if (is.numeric(df_obs[[col]])) {
      # If any value is > 1, we assume it's a percentage (0-100) instead of a fraction (0-1)
      if (any(df_obs[[col]] > 1, na.rm = TRUE)) {
        df_obs[[col]] <- df_obs[[col]] / 100
        adjusted_cols <- c(adjusted_cols, col)
      }
    }
  }
  
  # Sound the alarm if any nutrient columns had to be converted
  if (length(adjusted_cols) > 0) {
    message("\n", strrep("=", 60))
    message(" ⚠️  DATA CORRECTION APPLIED: NUTRIENT CONVERSION ⚠️ ")
    message(strrep("=", 60))
    message(" -> TRIGGER      : Values > 1 detected (assumed percentage format).")
    message(" -> ACTION       : Divided by 100 to force fractional format (g/g).")
    message(sprintf(" -> IMPACT       : %d columns dynamically adjusted:", length(adjusted_cols)))
    for (col in adjusted_cols) {
      message(sprintf("      - %s", col))
    }
    message(strrep("-", 60), "\n")
    
    # ---> NEW: Machine-readable Q-Flag for Nutrient Concentration Conversion (Grass25)
    log_qflag(
      severity = "WARN", 
      category = "DATA MODIFIED", 
      message = sprintf("Nutrient conversion (Grass25): converted %d column(s) from percentage to fraction space: [%s]", length(adjusted_cols), paste(adjusted_cols, collapse = ", "))
    )
  }
  
  return(df_obs)
}