#' Apply Project-Specific Data Fixes (Turretfield)
#' 
#' Intercepts the raw compiled observations, patches missing dates using metadata,
#' enforces strict year-matching against a reference date to catch Excel typos,
#' and linearly scales Haun stage data. Strictly flags any dates occurring before 
#' the specified sowing dates for manual review.
#'
#' @param compiled_obs The nested tibble from compile_all_observed().
#' @param df_obs_info The metadata dataframe containing 'SampleDateApprox'.
#' @param ref_date Character. A reference date (e.g., "15/05/2024") used to define 
#'   the definitive project year. Any dates with mismatched years are corrected.
#' @param max_haun Numeric. The target maximum value to scale raw Haun data to (Default = 12).
#' @param min_spr_sow_date Character. The spring sowing marker (Format: dd-mmm, e.g., "16-Apr").
#' @param min_wint_sow_date Character. The winter sowing marker (Format: dd-mmm, e.g., "15-May").
#' @return The corrected nested tibble
#' @export
apply_local_fixes <- function(compiled_obs, df_obs_info, ref_date, max_haun=12, 
                              min_spr_sow_date="16-Apr", min_wint_sow_date="15-May") {
  
  require(dplyr)
  require(purrr)
  require(lubridate)
  
  # ====================================================================
  # 0. THE FLEXIBLE DATE PARSER
  # ====================================================================
  parse_flexible_date <- function(date_string) {
    x <- trimws(as.character(date_string))
    nums <- suppressWarnings(as.numeric(x))
    if (!is.na(nums)) return(as.Date(nums, origin = "1899-12-30"))
    
    formats_to_try <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%Y/%m/%d", "%d/%m/%y", "%m/%d/%Y")
    for (fmt in formats_to_try) {
      d <- suppressWarnings(as.Date(x, format = fmt))
      if (!is.na(d)) return(d)
    }
    return(as.Date(NA))
  }
  
  # ====================================================================
  # 1. DEFENSIVE CHECKS & REFERENCE EXTRACTION
  # ====================================================================
  if (!"SampleDateApprox" %in% names(df_obs_info)) {
    warning("No 'SampleDateApprox' column found in metadata. Missing dates will not be imputed.", call. = FALSE)
  }
  
  parsed_ref_date <- parse_flexible_date(ref_date)
  if (is.na(parsed_ref_date)) {
    stop(sprintf("CRITICAL: Could not parse ref_date '%s'. Please provide a valid date.", ref_date))
  }
  target_year <- lubridate::year(parsed_ref_date)
  
  parsed_spr_sow_date <- suppressWarnings(as.Date(paste(min_spr_sow_date, target_year, sep="-"), format="%d-%b-%Y"))
  if (is.na(parsed_spr_sow_date)) {
    stop(sprintf("CRITICAL: Could not parse min_spr_sow_date '%s'. Ensure format is dd-mmm (e.g., '16-Apr').", min_spr_sow_date))
  }
  
  parsed_wint_sow_date <- suppressWarnings(as.Date(paste(min_wint_sow_date, target_year, sep="-"), format="%d-%b-%Y"))
  if (is.na(parsed_wint_sow_date)) {
    stop(sprintf("CRITICAL: Could not parse min_wint_sow_date '%s'. Ensure format is dd-mmm (e.g., '15-May').", min_wint_sow_date))
  }
  
  # Setup trackers for our audit logs
  patched_missing_vars <- c()
  year_correction_logs <- c()
  haun_scaling_logs    <- c() 
  fatal_date_logs      <- c() # NEW: Tracker for impossible dates
  
  # ====================================================================
  # 2. JOIN METADATA & PROCESS
  # ====================================================================
  final_obs <- compiled_obs %>%
    dplyr::left_join(
      df_obs_info %>% dplyr::select(df_name, column_name, dplyr::any_of("SampleDateApprox")), 
      by = "df_name"
    ) %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(data, column_name, if("SampleDateApprox" %in% names(.)) SampleDateApprox else NA), 
        function(df, var_name, approx_date_raw) {
          
          if (is.null(df) || nrow(df) == 0) return(df)
          
          if ("Date" %in% names(df)) {
            
            # -------------------------------------------------------------
            # ACTION A: Patch Missing Dates (NAs)
            # -------------------------------------------------------------
            if (any(is.na(df$Date)) && !is.na(approx_date_raw) && trimws(as.character(approx_date_raw)) != "") {
              approx_date <- parse_flexible_date(approx_date_raw)
              if (!is.na(approx_date)) {
                df <- df %>% dplyr::mutate(Date = dplyr::if_else(is.na(Date), approx_date, Date))
                patched_missing_vars <<- c(patched_missing_vars, var_name)
              } else {
                warning(sprintf("Could not parse SampleDateApprox '%s' for '%s'.", approx_date_raw, var_name), call. = FALSE)
              }
            }
            
            # -------------------------------------------------------------
            # ACTION B: Correct Bad Years
            # -------------------------------------------------------------
            bad_year_idx <- which(!is.na(df$Date) & lubridate::year(df$Date) != target_year)
            
            if (length(bad_year_idx) > 0) {
              old_dates_str <- format(df$Date[bad_year_idx], "%d/%m/%Y")
              lubridate::year(df$Date[bad_year_idx]) <- target_year
              new_dates_str <- format(df$Date[bad_year_idx], "%d/%m/%Y")
              
              unique_changes <- unique(paste("incorrect year", old_dates_str, "was forced to", new_dates_str))
              for (change in unique_changes) {
                year_correction_logs <<- c(year_correction_logs, sprintf("Variable '%s' - %s", var_name, change))
              }
            }
            
            # -------------------------------------------------------------
            # ACTION D: Flag Impossible Pre-Sowing Dates (No Imputation)
            # -------------------------------------------------------------
            if ("SimulationName" %in% names(df)) {
              row_min_sow_dates <- dplyr::case_when(
                grepl(min_wint_sow_date, df$SimulationName, ignore.case = TRUE) ~ parsed_wint_sow_date,
                grepl(min_spr_sow_date, df$SimulationName, ignore.case = TRUE) ~ parsed_spr_sow_date,
                TRUE ~ parsed_spr_sow_date 
              )
            } else {
              row_min_sow_dates <- rep(parsed_spr_sow_date, nrow(df))
            }
            
            pre_sow_idx <- which(!is.na(df$Date) & df$Date < row_min_sow_dates)
            
            if (length(pre_sow_idx) > 0) {
              for (idx in pre_sow_idx) {
                bad_date_str <- format(df$Date[idx], "%d/%m/%Y")
                sim_name_str <- if("SimulationName" %in% names(df)) df$SimulationName[idx] else "Unknown Simulation"
                target_min_sow <- format(row_min_sow_dates[idx], "%d/%m/%Y")
                
                # Append to the fatal log tracker
                fatal_date_logs <<- c(
                  fatal_date_logs,
                  sprintf(" -> [!] Variable: '%s' | Sim: '%s' | Invalid Date: %s (Sown on: %s)",
                          var_name, sim_name_str, bad_date_str, target_min_sow)
                )
              }
            }
          }
          
          # -------------------------------------------------------------
          # ACTION C: Scale Haun Stages
          # -------------------------------------------------------------
          haun_cols <- grep("haun", names(df), ignore.case = TRUE, value = TRUE)
          
          for (target_col in haun_cols) {
            if (is.numeric(df[[target_col]])) {
              raw_max <- max(df[[target_col]], na.rm = TRUE)
              
              if (!is.infinite(raw_max) && raw_max > 0) {
                scaling_factor <- max_haun / raw_max
                df[[target_col]] <- df[[target_col]] * scaling_factor
                
                haun_scaling_logs <<- c(
                  haun_scaling_logs, 
                  sprintf("Col '%s': scaled by factor %.3f (Raw Max: %.1f -> New Max: %d)", 
                          target_col, scaling_factor, raw_max, max_haun)
                )
              }
            }
          }
          
          return(df)
        }
      )
    ) %>%
    dplyr::select(df_name, data)
  
  # ====================================================================
  # 3. THE CONSOLIDATED AUDIT WARNINGS
  # ====================================================================
  if (length(patched_missing_vars) > 0) {
    message(paste(c(
      "",
      "======================================================================",
      " \U000026A0\U0000FE0F APPROXIMATE SAMPLE DATES APPLIED \U000026A0\U0000FE0F",
      "======================================================================",
      " Missing dates were patched using the 'SampleDateApprox' column.",
      " Variables Affected:",
      paste("   ->", unique(patched_missing_vars)),
      "======================================================================",
      ""
    ), collapse = "\n"))
  }
  
  if (length(year_correction_logs) > 0) {
    message(paste(c(
      "",
      "======================================================================",
      sprintf(" \U000026A0\U0000FE0F YEAR MISMATCHES CORRECTED (Target Year: %s) \U000026A0\U0000FE0F", target_year),
      "======================================================================",
      " Dates with mismatched years were found and forced to the target year:",
      paste("   ->", year_correction_logs),
      "======================================================================",
      ""
    ), collapse = "\n"))
  }
  
  if (length(fatal_date_logs) > 0) {
    warning_msg <- paste(c(
      "\n",
      "======================================================================",
      " \U0001F6A8 CRITICAL DATA ERROR: IMPOSSIBLE PRE-SOWING DATES DETECTED \U0001F6A8",
      "======================================================================",
      " The following records contain sampling dates that occur BEFORE the crop",
      " was physically planted. These must be manually corrected in the raw data.",
      "\n",
      paste(unique(fatal_date_logs), collapse = "\n"),
      "\n",
      " ACTION REQUIRED: Check the raw Excel file for dd/mm formatting typos.",
      "======================================================================",
      "\n"
    ), collapse = "\n")
    
    warning(warning_msg, call. = FALSE, immediate. = TRUE)
  }
  
  if (length(haun_scaling_logs) > 0) {
    warning_msg <- paste(c(
      "\n",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      "!!!                        HUGE WARNING                            !!!",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      sprintf(" \U000026A0\U0000FE0F HAUN STAGE DATA HAS BEEN LINEARLY SCALED (Target Max: %d) \U000026A0\U0000FE0F", max_haun),
      "----------------------------------------------------------------------",
      " ASSUMPTION: Raw Haun measurements were collected on a non-standard scale.",
      " FIX: All values were linearly scaled based on the global observed maximum.",
      " TRACEABILITY LOGS:",
      paste("   ->", unique(haun_scaling_logs)),
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      "\n"
    ), collapse = "\n")
    
    warning(warning_msg, call. = FALSE, immediate. = TRUE)
  }
  
  return(final_obs)
}