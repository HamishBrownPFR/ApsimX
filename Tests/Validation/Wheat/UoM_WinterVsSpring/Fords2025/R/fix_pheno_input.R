#' Fix Specific Phenology Input Errors (Manual Overrides)
#'
#' @description
#' Applies targeted, hard-coded fixes to the phenology input matrix before it is saved.
#' This is specifically used to rescue simulations with known raw-data biological impossibilities
#' (e.g., emergence dates recorded prior to sowing dates due to weeds or typos).
#'
#' @param pheno_wide Dataframe. The formatted APSIM phenology parameter matrix.
#' @return The corrected dataframe.
#' @export
fix_pheno_input <- function(pheno_wide) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  target_sim <- "Fords2025SowMay_Sunmaster"
  target_col <- "[Wheat].Phenology.Emerging.DateToProgress"
  
  # 1. Ensure the target simulation and column actually exist in this dataset
  if (target_sim %in% pheno_wide$SimulationName && target_col %in% names(pheno_wide)) {
    
    # 2. Isolate the row
    row_idx <- which(pheno_wide$SimulationName == target_sim)
    
    # 3. Extract the old value for the audit log
    old_val <- pheno_wide[[target_col]][row_idx]
    new_val <- "20-May-2025" # Formatted for APSIM standard dd-MMM-yyyy
    
    # 4. Only trigger the fix and the alarm if it hasn't already been fixed
    if (is.na(old_val) || old_val != new_val) {
      
      # Overwrite the data
      pheno_wide[[target_col]][row_idx] <- new_val
      
      # 🚨 THE SURGICAL STRIKE ALARM 🚨
      log_msg <- c(
        "",
        "======================================================================",
        " \U0001F6A8 MANUAL DATA OVERRIDE: BIOLOGICAL IMPOSSIBILITY FIXED \U0001F6A8",
        "======================================================================",
        sprintf(" -> Target Simulation : '%s'", target_sim),
        sprintf(" -> Target Parameter  : '%s'", target_col),
        " -> Root Cause        : Raw data indicated emergence occurred before sowing",
        "                        (likely a stray volunteer plant or data typo).",
        sprintf(" -> Old Value         : %s", ifelse(is.na(old_val), "NA", old_val)),
        sprintf(" -> New Value         : %s", new_val),
        " -> Action            : Force-synced emergence to the sowing date.",
        "======================================================================",
        ""
      )
      message(paste(log_msg, collapse = "\n"))
    }
  }
  
  return(pheno_wide)
}