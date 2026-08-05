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
  
  # Ensure necessary data manipulation packages are available before proceeding
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required.")
  
  # ---- 0. EXTRACT EXPECTED SIMULATIONS ----
  # Determine if the input is a mapping dataframe or a raw character vector
  if (is.data.frame(expected_sims)) {
    # If it is a dataframe, extract the 'SimulationName' column and remove duplicates
    expected_list <- unique(as.character(expected_sims$SimulationName))
  } else {
    # If it is already a vector of names, assign it directly
    expected_list <- expected_sims
  }
  
  # Initialize the foundational error message block
  error_msg <- c(
    "",
    "======================================================================",
    " \U0001F6A8 FATAL ERROR: PHENOLOGY INTEGRITY CHECK FAILED \U0001F6A8 ",
    "======================================================================",
    " APSIM will crash if 'DateToProgress' parameters are missing.",
    ""
  )
  
  # Establish a logical flag to track if any integrity checks fail
  has_fatal_error <- FALSE
  
  # ---- 1. CHECK FOR DROPPED ROWS (Completely missing simulations) ----
  # Compare the expected list against the actual list in the final dataframe
  # setdiff() isolates simulation names that exist in 'expected_list' but NOT in 'df_pheno'
  missing_sims <- setdiff(expected_list, df_pheno$SimulationName)
  
  if (length(missing_sims) > 0) {
    has_fatal_error <- TRUE # Trigger the failure flag
    error_msg <- c(error_msg, " [!] MISSING ROWS: The following simulations were completely dropped:")
    
    # Loop through every missing simulation and append its name to the error report
    for (sim in missing_sims) {
      error_msg <- c(error_msg, sprintf("  -> Simulation: '%s'", sim))
    }
    error_msg <- c(error_msg, "")
  }
  
  # ---- 2. CHECK FOR MISSING CELLS (NAs inside existing rows) ----
  # Quickly scan the entire dataframe to see if any cell contains an NA
  if (any(is.na(df_pheno))) {
    has_fatal_error <- TRUE # Trigger the failure flag
    error_msg <- c(error_msg, " [!] MISSING VALUES: The following simulations have NA dates:")
    
    # Pinpoint exactly which simulation and parameter column combination contains the NA
    missing_report <- df_pheno %>%
      # Keep only the rows (simulations) that have at least one NA in any column
      dplyr::filter(dplyr::if_any(dplyr::everything(), is.na)) %>%
      # Pivot the data into a long format so we can inspect every single cell individually
      tidyr::pivot_longer(
        cols = -SimulationName, 
        names_to = "Parameter", 
        values_to = "Value"
      ) %>%
      # Filter down to only the cells that are specifically NA
      dplyr::filter(is.na(Value)) %>%
      # Retain only the simulation name and the specific APSIM parameter that is missing data
      dplyr::select(SimulationName, Parameter)
    
    # Loop through the pinpointed list and append exactly which stage is missing for which simulation
    for (i in 1:nrow(missing_report)) {
      error_msg <- c(
        error_msg, 
        sprintf("  -> Simulation: '%s' | Missing: '%s'", 
                missing_report$SimulationName[i], missing_report$Parameter[i])
      )
    }
  }
  
  # ---- 3. RESOLUTION AND PIPELINE CONTROL ----
  if (!has_fatal_error) {
    # If the flag was never triggered, the data is perfect. Return the dataframe safely.
    message("\n\U0001F7E2 SUCCESS: Phenology Input Matrix passed integrity check. No missing rows or dates.")
    return(df_pheno)
  } else {
    # If the flag was triggered, append instructions on how to fix the issue
    error_msg <- c(
      error_msg,
      "======================================================================",
      " ACTION REQUIRED: Fix the raw data, adjust upstream drop_na() filters, ",
      " or apply an imputation function.",
      "======================================================================",
      ""
    )
    
    # Print the aggregated, detailed error report to the console
    message(paste(error_msg, collapse = "\n"))
    
    # Log a machine-readable Q-Flag for the Quarto dashboard so the crash is recorded
    log_qflag(
      severity = "FATAL", 
      category = "PHENOLOGY", 
      message = sprintf("Phenology integrity gatekeeper failed: %d missing row(s) and/or missing date value(s) detected.", length(missing_sims))
    )
    
    # Force the R script to stop completely. This prevents the bad data from being sent to APSIM.
    stop("Pipeline Halted: Phenology Matrix Integrity Check Failed.", call. = FALSE)
  }
}