#' Apply Tailored Corrections to Wide Observation Data
#'
#' @description
#' Applies specific structural corrections to the finalized wide observation dataframe.
#' Currently, this function exclusively enforces APSIM-X simulation naming conventions,
#' resolving the legacy `Sow[Month]Cv` vs `CvSow[Month]` conflict.
#'
#' @param df A data.frame containing the wide-format observation data.
#' @return The corrected data.frame.
#' @export
apply_corrections_Gna25 <- function(df) {
  
  if (!is.data.frame(df)) stop("CRITICAL: Input to apply_corrections_Gna25 is not a dataframe.")
  
  # ------------------------------------------------------------------
  # CORRECTION: Align Simulation Names with APSIM-X Interface
  # ------------------------------------------------------------------
  if ("SimulationName" %in% names(df)) {
    
    # Ensure the column is explicitly a character vector to prevent factor-coercion bugs
    original_names <- as.character(df$SimulationName)
    
    # Safely swap "Sow[Month]Cv" to "CvSow[Month]" (e.g., SowAprCv -> CvSowApr)
    # Using [A-Za-z]+ makes it robust to both 3-letter (Apr) and full-word (April) month entries
    modified_names <- gsub("Sow([A-Za-z]+)Cv", "CvSow\\1", original_names)
    
    # If a change actually occurred, apply the fix and throw the massive alarm!
    if (!identical(original_names, modified_names)) {
      
      df$SimulationName <- modified_names
      
      apsim_warning <- c(
        "",
        "======================================================================",
        " \U0001F6A8 CRITICAL ALARM: APSIM-X SIMULATION NAME MISMATCH DETECTED \U0001F6A8",
        "======================================================================",
        " ISSUE   : The raw data uses 'Sow[Month]Cv' (e.g., SowAprCv), but the",
        "           APSIM-X .apsimx file expects 'CvSow[Month]' (e.g., CvSowApr).",
        " ACTION  : The pipeline has automatically forcibly renamed all",
        "           simulations to match the APSIM-X interface.",
        " WARNING : If you ever update the .apsimx file to match the lab data,",
        "           this forced regex correction MUST be removed!",
        "======================================================================",
        ""
      )
      
      message(paste(apsim_warning, collapse = "\n"))
      
      # ---> Machine-readable Q-Flag for Name Mismatch
      tryCatch({
        log_qflag(
          severity = "CRITICAL", 
          category = "NAMES", 
          message = "Simulation name mismatch between raw data and .apsimx. Pipeline forcibly renamed them."
        )
      }, error = function(e) warning("log_qflag function not found, skipping Q-Flag logging."))
      
      # Trigger a native R warning so {targets} flags it in tar_meta()
      warning("APSIM-X Simulation name forced regex corrections were applied. See console for details.", call. = FALSE)
    }
  }
  
  return(df)
}