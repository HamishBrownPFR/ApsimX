#' Transform and Format Phenology Timelines for APSIM-X Injection (Step 5)
#'
#' @description
#' The final phenology pipeline component. It ingests the unified, quality-checked 
#' long-format phenology data, runs a fail-safe chronological sequence check, translates 
#' numeric stages into explicit APSIM-X bracketed parameter strings, pivots the dataset 
#' wide, and strictly formats dates to locale-safe characters ("dd-MMM-yyyy").
#' 
#' Empty Column Pruning: Automatically detects and removes any phenology stage columns 
#' that contain 100% NA values across all simulations, preventing downstream APSIM crashes.
#'
#' @param df_pheno_final Data frame. The unified 3-column output from Step 4.
#'
#' @return A wide data frame strictly formatted for APSIM-X parameter input.
#' @export
format_apsim_pheno_params <- function(df_pheno_final) {
  
  # ---- 1. DEFENSIVE INTEGRITY CHECKS ----
  # Verify that the input dataframe is provided and not empty
  if (missing(df_pheno_final) || is.null(df_pheno_final) || nrow(df_pheno_final) == 0) {
    stop("Error [format_apsim_pheno_params]: Unified phenology data frame is missing or empty.")
  }
  
  # Ensure the input dataframe conforms strictly to the 3-column interface expected from Step 4
  req_cols <- c("SimulationName", "Clock.Today", "Wheat.Phenology.Stage")
  missing_cols <- setdiff(req_cols, names(df_pheno_final))
  if (length(missing_cols) > 0) {
    stop(paste("Error [format_apsim_pheno_params]: Input does not match standard schema. Missing:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # ---- 2. ULTIMATE FAIL-SAFE CHRONOLOGY CHECK ----
  # Double-check that no timeline inversions slipped through the pipeline
  chrono_check <- df_pheno_final %>%
    # Sort chronologically and by developmental stage per simulation
    dplyr::arrange(SimulationName, Wheat.Phenology.Stage) %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    # Calculate the time difference between sequential rows; flag if any difference is negative (moving backward in time)
    dplyr::summarise(
      Is_Bad = any(as.numeric(diff(Clock.Today)) < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Isolate only the simulations that failed the chronology check
    dplyr::filter(Is_Bad == TRUE)
  
  # If timeline inversions exist, throw a critical warning before formatting
  if (nrow(chrono_check) > 0) {
    bad_sims <- chrono_check$SimulationName
    warning_box <- c(
      "",
      "======================================================================",
      "   ⚠️  CRITICAL: FINAL EXPORT CHRONOLOGY ERROR DETECTED ⚠️ ",
      "======================================================================",
      " The following SimulationNames have non-sequential dates in the final",
      " export buffer (a later stage is dated before an earlier stage):",
      paste("    -", bad_sims),
      "======================================================================",
      " Action Required: Review Step 4 (merge_and_qc_pheno) execution logs.",
      ""
    )
    message(paste(warning_box, collapse = "\n"))
    
    # Machine-readable Q-Flag for Chronology Error
    log_qflag(
      severity = "CRITICAL", 
      category = "PHENOLOGY", 
      message = sprintf("Final export chronology error detected in %d simulation(s): non-sequential dates found.", length(bad_sims))
    )
    
    warning("Export buffer contains chronological inversions. See console.", call. = FALSE)
  }
  
  # ---- 3. APSIM STRING MAPPING ----
  df_mapped <- df_pheno_final %>%
    dplyr::mutate(
      # Translate simple numeric codes (3, 4, 5) into APSIM's internal parameter-naming convention
      Stage_Name = dplyr::case_when(
        Wheat.Phenology.Stage == 3  ~ "Emerging",
        Wheat.Phenology.Stage == 4  ~ "LeavesInitiating",
        Wheat.Phenology.Stage == 5  ~ "SpikeletsDifferentiating",
        Wheat.Phenology.Stage == 6  ~ "StemElongating",
        Wheat.Phenology.Stage == 7  ~ "Heading",
        Wheat.Phenology.Stage == 8  ~ "Flowering",
        Wheat.Phenology.Stage == 10 ~ "GrainFilling",
        TRUE                        ~ NA_character_
      )
    ) %>%
    # Drop rows with unrecognized stage codes
    dplyr::filter(!is.na(Stage_Name)) %>%
    dplyr::mutate(
      # Concatenate the exact bracketed parameter string APSIM expects for injection
      Apsim_Param = paste0("[Wheat].Phenology.", Stage_Name, ".DateToProgress")
    )
  
  # ---- 4. PIVOT WIDE & ORDER COLUMNS ----
  # Define the strict chronological sequence of columns for the final file
  ordered_cols <- c(
    "SimulationName",
    "[Wheat].Phenology.Emerging.DateToProgress",
    "[Wheat].Phenology.LeavesInitiating.DateToProgress",
    "[Wheat].Phenology.SpikeletsDifferentiating.DateToProgress",
    "[Wheat].Phenology.StemElongating.DateToProgress",
    "[Wheat].Phenology.Heading.DateToProgress",
    "[Wheat].Phenology.Flowering.DateToProgress",
    "[Wheat].Phenology.GrainFilling.DateToProgress"
  )
  
  # Reshape data from a long vertical list to a wide format (one row per simulation)
  df_wide <- df_mapped %>%
    dplyr::select(SimulationName, Apsim_Param, Clock.Today) %>%
    tidyr::pivot_wider(
      names_from = Apsim_Param,
      values_from = Clock.Today
    )
  
  # Initialize missing stage columns dynamically with NA to maintain schema consistency
  missing_ap_cols <- setdiff(ordered_cols, names(df_wide))
  for (col in missing_ap_cols) {
    df_wide[[col]] <- as.Date(NA)
  }
  
  # ---- 4.5 EMPTY COLUMN PRUNING ----
  # Identify any phenology stages (columns) that contain zero valid dates across the entire dataset
  empty_cols <- names(df_wide)[purrr::map_lgl(df_wide, ~all(is.na(.x)))]
  empty_cols <- setdiff(empty_cols, "SimulationName") # Shield the primary key column from accidental pruning
  
  # Remove fully empty columns to prevent APSIM injection parsing errors
  if (length(empty_cols) > 0) {
    df_wide <- df_wide %>% dplyr::select(-dplyr::all_of(empty_cols))
    
    log_box <- c(
      "",
      "----------------------------------------------------------------------",
      " 🧹 PIPELINE ACTION: EMPTY STAGE COLUMNS PRUNED 🧹",
      "----------------------------------------------------------------------",
      " The following phenology stages contained no data (100% NA) across all",
      " simulations and were safely removed from the final APSIM output:",
      paste("    -", empty_cols),
      "----------------------------------------------------------------------",
      ""
    )
    message(paste(log_box, collapse = "\n"))
    
    # Machine-readable Q-Flag for Pruned Empty Columns
    log_qflag(
      severity = "INFO", 
      category = "PHENOLOGY", 
      message = sprintf("Pruned %d empty phenology stage column(s) (100%% NA): [%s]", length(empty_cols), paste(empty_cols, collapse = ", "))
    )
  }
  
  # Reorder the remaining valid columns back into the strict physiological order
  remaining_ordered_cols <- intersect(ordered_cols, names(df_wide))
  df_wide <- df_wide %>% dplyr::select(dplyr::all_of(remaining_ordered_cols))
  
  # ---- 5. LOCALE-SAFE TEXT STRING FORMATTING ----
  # Hardcode standard English abbreviations to prevent international locale bugs during formatting
  eng_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Internal function to convert standard Date objects into APSIM's required text layout
  format_apsim_date <- function(dates) {
    dplyr::if_else(
      is.na(dates), 
      NA_character_, 
      # Format as "dd-MMM-yyyy" (e.g., 05-Aug-2026) using the hardcoded month array
      sprintf("%02d-%s-%04d", 
              as.numeric(format(dates, "%d")), 
              eng_months[as.numeric(format(dates, "%m"))], 
              as.numeric(format(dates, "%Y")))
    )
  }
  
  # Apply the string formatter to all columns except SimulationName
  df_export <- df_wide %>%
    dplyr::mutate(
      dplyr::across(-SimulationName, format_apsim_date)
    )
  
  # ---- 6. PIPELINE COMPLETION NOTIFICATION ----
  message(sprintf("Success [format_apsim_pheno_params]: Translated %d simulations into wide APSIM parameter format.", 
                  nrow(df_export)))
  
  # Machine-readable Q-Flag for Successful Wide Format Translation
  log_qflag(
    severity = "INFO", 
    category = "PHENOLOGY", 
    message = sprintf("Translated %d simulations into wide APSIM parameter format.", nrow(df_export))
  )
  
  return(df_export)
}