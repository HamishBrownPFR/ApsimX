#' Apply Manual Data Corrections to Raw Observations (Wagga Wagga 2024 Context)
#'
#' @description
#' Performs targeted manual corrections and data patches on a compiled, nested observation tibble.
#' Specifically, it repairs a known calendar year-offset issue inside raw NDVI records and back-fills
#' missing collection dates for physical biomass and tissue chemical cuts at Stage 6 and Stage 8.
#'
#' @details
#' **Decoupled Architecture:** This function explicitly requires the finalized, universal 
#' phenology timeline (`df_pheno_final`) to build its synchronization dictionary. 
#' 
#' **Audit Logging:** Automatically reports which dataframes were rescued, how many orphaned rows 
#' were patched, and the exact chronological date range injected from the phenology records.
#'
#' @param df_tbl A nested tibble structure containing the raw \code{df_name} and \code{data}.
#' @param df_pheno_final Data frame. The long-format universal phenology output from Step 4.
#' @param ref_date Character string. A reference date (e.g., "1/1/2024") used to define the base year 
#'   for correcting sensor year-rollover bugs.
#'
#' @return The corrected nested tibble with normalized calendar records and patched matrices.
#' @export
apply_corrections_Wagga24 <- function(df_tbl, df_pheno_final, ref_date) {
  
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' required.")
  
  # ---- 0. ROBUST REFERENCE DATE PARSING ----
  rd <- lubridate::parse_date_time(ref_date, orders = c("dmy", "ymd", "mdy"))
  if (is.na(rd)) {
    stop(sprintf("\n\U0001F6A8 CRITICAL ERROR: Could not parse 'ref_date' (%s). Please use a standard format like '1/1/2024' or '2024-01-01'.", ref_date), call. = FALSE)
  }
  ry <- lubridate::year(rd)
  
  if (missing(df_tbl) || !all(c("df_name", "data") %in% names(df_tbl))) {
    stop("Error [apply_corrections_Wagga24]: Input target 'df_tbl' must be a valid nested tibble.")
  }
  if (missing(df_pheno_final) || nrow(df_pheno_final) == 0) {
    stop("Error [apply_corrections_Wagga24]: Must provide 'df_pheno_final' to back-fill dates.")
  }
  
  # ---- 1. GENERATE THE DICTIONARY MATRIX FROM UNIVERSAL PHENO ----
  date_lookup <- df_pheno_final %>%
    dplyr::filter(Wheat.Phenology.Stage %in% c(6, 8)) %>%
    dplyr::select(SimulationName, Clock.Today, Wheat.Phenology.Stage) %>%
    dplyr::mutate(PhenoDate = paste0("PhenoDate_", Wheat.Phenology.Stage)) %>%
    dplyr::select(-Wheat.Phenology.Stage) %>%
    tidyr::pivot_wider(names_from = PhenoDate, values_from = Clock.Today)
  
  # Safety bounds: Ensure columns exist even if some stages were missing
  if (!"PhenoDate_6" %in% names(date_lookup)) date_lookup$PhenoDate_6 <- as.Date(NA)
  if (!"PhenoDate_8" %in% names(date_lookup)) date_lookup$PhenoDate_8 <- as.Date(NA)
  
  # ---- 2. ITERATE METADATA CORRECTIONS PIPELINE ----
  df_tbl_corrected <- df_tbl %>%
    dplyr::mutate(
      data = purrr::map2(
        .x = data,
        .y = df_name,
        .f = function(df, nm) {
          
          # Fix A: Repair NDVI sensor year rollover offsets using Dynamic Reference Year
          if (nm == "ndvi_raw") {
            if (!"Date" %in% names(df)) return(df)
            
            # Count how many bugs exist before fixing
            bug_count <- sum(lubridate::year(df$Date) != ry, na.rm = TRUE)
            
            df <- df %>%
              dplyr::mutate(
                Date = as.Date(Date),
                Date = dplyr::if_else(
                  lubridate::year(Date) != ry,
                  Date %m+% lubridate::years(ry - lubridate::year(Date)), # Safe math difference
                  Date
                )
              )
            
            if (bug_count > 0) {
              message(sprintf("\n [\U0001F527 NDVI REPAIR] Synchronized %d misaligned sensor readings to the reference year (%d).", bug_count, ry))
            }
          }
          
          # Fix B: Patch collection dates for physiological components at Stage 6
          target_dfs_6 <- c(
            "stemYield_6_raw", "spikeYield_6_raw", "senescLeafYield_6_raw", 
            "totalAboveGround_6_raw", "par_6_raw", "greenLeaf_6_raw",
            "leafDead_6_Nconc", "leafDead_6_WSCc",
            "leaf_6_Nconc", "leaf_6_WSCc",
            "stem_6_Nconc","stem_6_WSCc","spike_6_Nconc","spike_6_WSCc"
          )
          
          if (nm %in% target_dfs_6) {
            if (!"SimulationName" %in% names(df)) stop(sprintf("Error: Table %s lacks 'SimulationName'.", nm))
            if (!"Date" %in% names(df)) df$Date <- as.Date(NA)
            
            orphans_before <- sum(is.na(df$Date))
            
            df <- df %>%
              dplyr::left_join(date_lookup, by = "SimulationName") %>%
              dplyr::mutate(Date = as.Date(.data$PhenoDate_6))
            
            valid_dates <- df$Date[!is.na(df$Date)]
            min_d <- if(length(valid_dates) > 0) as.character(min(valid_dates)) else "N/A"
            max_d <- if(length(valid_dates) > 0) as.character(max(valid_dates)) else "N/A"
            
            df <- df %>%
              dplyr::select(-dplyr::any_of(c("PhenoDate_6", "PhenoDate_8"))) %>%
              dplyr::filter(!is.na(Date)) 
            
            if (orphans_before > 0 && length(valid_dates) > 0) {
              message("\n", strrep("=", 70))
              message(" \U0001F489  DATA RESCUE: EXTERNAL PHENOLOGY TIMELINE INJECTED \U0001F489")
              message(strrep("=", 70))
              message(sprintf(" -> Target Data   : '%s'", nm))
              message(" -> Target Stage  : Stage 6 (Anthesis/Flowering)")
              message(sprintf(" -> Action Taken  : Overwrote %d previously orphaned/NA dates.", orphans_before))
              message(sprintf(" -> Date Timeline : %s to %s", min_d, max_d))
              message(strrep("-", 70), "\n")
            }
          }
          
          # Fix C: Patch collection dates for physiological components at Stage 8
          target_dfs_8 <- c(
            "stemYield_8_raw", "spikeYield_8_raw", "senescLeafYield_8_raw", 
            "totalAboveGround_8_raw", "par_8_raw", "greenLeaf_8_raw",
            "leafDead_8_Nconc", "leafDead_8_WSCc",
            "leaf_8_Nconc", "leaf_8_WSCc",
            "stem_8_Nconc","stem_8_WSCc","spike_8_Nconc","spike_8_WSCc"
          )
          
          if (nm %in% target_dfs_8) {
            if (!"SimulationName" %in% names(df)) stop(sprintf("Error: Table %s lacks 'SimulationName'.", nm))
            if (!"Date" %in% names(df)) df$Date <- as.Date(NA) 
            
            orphans_before <- sum(is.na(df$Date))
            
            df <- df %>%
              dplyr::left_join(date_lookup, by = "SimulationName") %>%
              dplyr::mutate(Date = as.Date(.data$PhenoDate_8))
            
            valid_dates <- df$Date[!is.na(df$Date)]
            min_d <- if(length(valid_dates) > 0) as.character(min(valid_dates)) else "N/A"
            max_d <- if(length(valid_dates) > 0) as.character(max(valid_dates)) else "N/A"
            
            df <- df %>%
              dplyr::select(-dplyr::any_of(c("PhenoDate_6", "PhenoDate_8"))) %>%
              dplyr::filter(!is.na(Date)) 
            
            if (orphans_before > 0 && length(valid_dates) > 0) {
              message("\n", strrep("=", 70))
              message(" \U0001F489  DATA RESCUE: EXTERNAL PHENOLOGY TIMELINE INJECTED \U0001F489")
              message(strrep("=", 70))
              message(sprintf(" -> Target Data   : '%s'", nm))
              message(" -> Target Stage  : Stage 8 (Maturity)")
              message(sprintf(" -> Action Taken  : Overwrote %d previously orphaned/NA dates.", orphans_before))
              message(sprintf(" -> Date Timeline : %s to %s", min_d, max_d))
              message(strrep("-", 70), "\n")
            }
          }
          
          return(df)
        }
      )
    )
  
  message("\n\U0001F7E2 Success [apply_corrections_Wagga24]: Phenology timelines applied and matrices patched.\n")
  return(df_tbl_corrected)
}