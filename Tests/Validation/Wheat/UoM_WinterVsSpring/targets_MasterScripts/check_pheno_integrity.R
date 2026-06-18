#' Universal Phenology Integrity Gatekeeper (Upgraded)
#'
#' @description
#' Scans the final APSIM phenology parameter matrix for missing dates (NAs) AND missing rows.
#' If any missing values or missing simulations are found, it halts the pipeline and prints 
#' a detailed report to prevent APSIM crashes.
#'
#' @param df_pheno The wide dataframe containing SimulationName and DateToProgress columns.
#' @param expected_sims A dataframe containing all expected 'SimulationName's (e.g., your mapping table).
#' @return The original dataframe (if it passes) or throws a fatal error (if it fails).
#' @export
check_pheno_integrity <- function(df_pheno, expected_sims) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required.")
  
  # 0. Extract Expected Simulations
  if (is.data.frame(expected_sims)) {
    expected_list <- unique(as.character(expected_sims$SimulationName))
  } else {
    expected_list <- expected_sims
  }
  
  error_msg <- c(
    "",
    "======================================================================",
    " \U0001F6A8 FATAL ERROR: PHENOLOGY INTEGRITY CHECK FAILED \U0001F6A8 ",
    "======================================================================",
    " APSIM will crash if 'DateToProgress' parameters are missing.",
    ""
  )
  
  has_fatal_error <- FALSE
  
  # 1. CHECK FOR DROPPED ROWS (Completely missing simulations)
  missing_sims <- setdiff(expected_list, df_pheno$SimulationName)
  if (length(missing_sims) > 0) {
    has_fatal_error <- TRUE
    error_msg <- c(error_msg, " [!] MISSING ROWS: The following simulations were completely dropped:")
    for (sim in missing_sims) {
      error_msg <- c(error_msg, sprintf("  -> Simulation: '%s'", sim))
    }
    error_msg <- c(error_msg, "")
  }
  
  # 2. CHECK FOR MISSING CELLS (NAs inside existing rows)
  if (any(is.na(df_pheno))) {
    has_fatal_error <- TRUE
    error_msg <- c(error_msg, " [!] MISSING VALUES: The following simulations have NA dates:")
    
    missing_report <- df_pheno %>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), is.na)) %>%
      tidyr::pivot_longer(
        cols = -SimulationName, 
        names_to = "Parameter", 
        values_to = "Value"
      ) %>%
      dplyr::filter(is.na(Value)) %>%
      dplyr::select(SimulationName, Parameter)
    
    for (i in 1:nrow(missing_report)) {
      error_msg <- c(
        error_msg, 
        sprintf("  -> Simulation: '%s' | Missing: '%s'", 
                missing_report$SimulationName[i], missing_report$Parameter[i])
      )
    }
  }
  
  # 3. RESOLUTION
  if (!has_fatal_error) {
    message("\n\U0001F7E2 SUCCESS: Phenology Input Matrix passed integrity check. No missing rows or dates.")
    return(df_pheno)
  } else {
    error_msg <- c(
      error_msg,
      "======================================================================",
      " ACTION REQUIRED: Fix the raw data, adjust upstream drop_na() filters, ",
      " or apply an imputation function.",
      "======================================================================",
      ""
    )
    message(paste(error_msg, collapse = "\n"))
    stop("Pipeline Halted: Phenology Matrix Integrity Check Failed.", call. = FALSE)
  }
}