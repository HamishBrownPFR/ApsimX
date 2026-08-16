#' Apply Specific Name and Data Corrections for GrassPatch2025
#'
#' @description
#' Applies specific structural corrections to the finalized wide observation dataframe.
#' Currently, this function exclusively handles a hardcoded simulation name override 
#' to map the raw data tag 'FAR WAE W25-49' to the APSIM-X target 'GrassPatch2025'.
#'
#' @param df_obs Dataframe containing the observed data
#' @return Corrected dataframe
#' @export
apply_corrections_Grass25 <- function(df_obs) {
  
  if (!is.data.frame(df_obs)) stop("CRITICAL: Input to apply_corrections_Grass25 is not a dataframe.")
  
  # ------------------------------------------------------------------
  # SIMULATION NAME OVERRIDE (Base R)
  # ------------------------------------------------------------------
  if (!"SimulationName" %in% names(df_obs)) {
    stop("CRITICAL: 'SimulationName' column not found in the observed dataframe.")
  }
  
  target_name <- "FAR WAE W25-49"
  new_name    <- "GrassPatch2025"
  
  # Create a safe boolean mask (treating any NAs as FALSE so it doesn't crash the sum)
  mask <- df_obs$SimulationName == target_name
  mask[is.na(mask)] <- FALSE
  affected_rows <- sum(mask)
  
  if (affected_rows > 0) {
    message("\n======================================================================")
    message(" \u26A0\uFE0F  DATA CORRECTION APPLIED: SIMULATION NAME OVERRIDE \u26A0\uFE0F ")
    message("======================================================================")
    message(sprintf(" -> TARGET FOUND : '%s'", target_name))
    message(sprintf(" -> ACTION       : Renamed to '%s'", new_name))
    message(sprintf(" -> IMPACT       : %d rows updated", affected_rows))
    message(" -> NOTE         : This is a hardcoded fix for GrassPatch2025.")
    message("----------------------------------------------------------------------\n")
    
    # Base R assignment completely bypasses dplyr
    df_obs$SimulationName[mask] <- new_name
    
    # ---> Machine-readable Q-Flag
    tryCatch({
      log_qflag(
        severity = "WARN", 
        category = "DATA MODIFIED", 
        message = sprintf("Simulation name override (Grass25): renamed '%s' to '%s' (%d rows updated).", target_name, new_name, affected_rows)
      )
    }, error = function(e) warning("log_qflag function not found, skipping Q-Flag logging."))
  }
  
  return(df_obs)
}