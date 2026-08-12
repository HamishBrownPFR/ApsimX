#' Apply Date Imputation and Year Standardization (Step 1)
#'
#' @description
#' Scans a nested tibble of observation dataframes for missing or NA values in the 'Date' column.
#' If missing dates are found, it looks up the dataframe name (`df_name`) in the provided metadata 
#' table and injects the corresponding fallback date. It then validates all dates against a definitive
#' reference year (`ref_date`), automatically correcting any mismatched years, and generates detailed 
#' console logs and Quarto Q-Flags.
#'
#' @param list_df_obs A nested tibble containing `df_name` and a `data` list-column.
#' @param df_meta A dataframe containing metadata for the observations.
#' @param df_meta_date_col Character. The name of the column in `df_meta` containing the fallback dates.
#' @param ref_date Character. A reference date (e.g., "15/05/2024") used to define the definitive project year.
#'
#' @return The nested tibble with imputed and year-standardized dates.
#' @export
Turret24_apply_fixes <- function(list_df_obs, df_meta, df_meta_date_col, ref_date) {
  
  require(dplyr)
  require(purrr)
  require(lubridate)
  
  # ====================================================================
  # 0. STATE-OF-THE-ART INPUT VALIDATION & DATE ANCHORING
  # ====================================================================
  if (!is.data.frame(list_df_obs) || !"data" %in% names(list_df_obs) || !"df_name" %in% names(list_df_obs)) {
    stop("CRITICAL: 'list_df_obs' must be a nested tibble containing 'df_name' and 'data' columns.")
  }
  if (!is.data.frame(df_meta) || !"df_name" %in% names(df_meta)) {
    stop("CRITICAL: 'df_meta' must be a dataframe containing a 'df_name' column for joining.")
  }
  if (!df_meta_date_col %in% names(df_meta)) {
    warning(sprintf("Metadata column '%s' not found in df_meta. Skipping date imputation.", df_meta_date_col), call. = FALSE)
    return(list_df_obs)
  }
  
  # Helper: Robust Date Parser
  parse_flexible_date <- function(date_string) {
    if (is.na(date_string)) return(as.Date(NA))
    x <- trimws(as.character(date_string))
    if (x == "") return(as.Date(NA))
    
    # Try numeric Excel dates first
    nums <- suppressWarnings(as.numeric(x))
    if (!is.na(nums)) return(as.Date(nums, origin = "1899-12-30"))
    
    # Try standard string formats
    formats_to_try <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%Y/%m/%d", "%d/%m/%y", "%m/%d/%Y")
    for (fmt in formats_to_try) {
      d <- suppressWarnings(as.Date(x, format = fmt))
      if (!is.na(d)) return(d)
    }
    return(as.Date(NA))
  }
  
  # Parse reference date and extract target year
  parsed_ref_date <- parse_flexible_date(ref_date)
  if (is.na(parsed_ref_date)) {
    stop(sprintf("CRITICAL: Could not parse ref_date '%s'. Please provide a valid date format (e.g., '15/05/2024').", ref_date))
  }
  target_year <- lubridate::year(parsed_ref_date)
  
  # Audit Trackers
  patched_logs         <- c()
  failed_logs          <- c()
  year_correction_logs <- c()
  
  # ====================================================================
  # 1. METADATA JOIN & PROCESSING LOOP
  # ====================================================================
  res <- list_df_obs %>%
    dplyr::left_join(
      df_meta %>% 
        dplyr::select(df_name, fallback_date_raw = dplyr::all_of(df_meta_date_col)),
      by = "df_name"
    ) %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(data, df_name, fallback_date_raw),
        function(df, name, approx_date_raw) {
          
          # Skip empty dataframes or those without a Date column
          if (is.null(df) || nrow(df) == 0 || !"Date" %in% names(df)) return(df)
          
          # Enforce Date class on the target column to prevent type-cast crashes
          if (!inherits(df$Date, "Date")) {
            parsed_existing <- suppressWarnings(lubridate::parse_date_time(df$Date, orders = c("dmy", "ymd", "Ymd", "mdy")))
            df$Date <- as.Date(parsed_existing)
          }
          
          # -------------------------------------------------------------
          # ACTION A: Patch Missing Dates (NAs)
          # -------------------------------------------------------------
          na_idx <- which(is.na(df$Date) | trimws(as.character(df$Date)) == "")
          
          if (length(na_idx) > 0) {
            if (is.na(approx_date_raw) || trimws(as.character(approx_date_raw)) == "") {
              failed_logs <<- c(failed_logs, sprintf(" -> [!] '%s' has %d missing date(s), but metadata '%s' is empty.", name, length(na_idx), df_meta_date_col))
            } else {
              parsed_date <- parse_flexible_date(approx_date_raw)
              if (is.na(parsed_date)) {
                failed_logs <<- c(failed_logs, sprintf(" -> [!] '%s' metadata date '%s' could not be parsed into a valid Date.", name, approx_date_raw))
              } else {
                df$Date[na_idx] <- parsed_date
                patched_logs <<- c(patched_logs, sprintf(" -> [\u2713] '%s' : Imputed %d row(s) with %s", name, length(na_idx), format(parsed_date, "%d-%b-%Y")))
              }
            }
          }
          
          # -------------------------------------------------------------
          # ACTION B: Standardize Years (Match against target_year)
          # -------------------------------------------------------------
          bad_year_idx <- which(!is.na(df$Date) & lubridate::year(df$Date) != target_year)
          
          if (length(bad_year_idx) > 0) {
            old_dates_str <- format(df$Date[bad_year_idx], "%d/%m/%Y")
            
            # Forcibly update the year to target_year
            lubridate::year(df$Date[bad_year_idx]) <- target_year
            
            new_dates_str <- format(df$Date[bad_year_idx], "%d/%m/%Y")
            
            unique_changes <- unique(sprintf("incorrect year %s was forced to %s", old_dates_str, new_dates_str))
            for (change in unique_changes) {
              year_correction_logs <<- c(year_correction_logs, sprintf("DataFrame '%s' - %s", name, change))
            }
          }
          
          return(df)
        }
      )
    ) %>%
    dplyr::select(-fallback_date_raw) # Clean up temporary join column
  
  # ====================================================================
  # 2. REPORTING & QUARTO Q-FLAGS
  # ====================================================================
  if (length(patched_logs) > 0 || length(failed_logs) > 0 || length(year_correction_logs) > 0) {
    
    message("\n======================================================================")
    message(" \U0001F4CB DATE IMPUTATION & YEAR STANDARDIZATION SUMMARY \U0001F4CB")
    message("======================================================================")
    
    # 1. Handle Successful Imputations
    if (length(patched_logs) > 0) {
      message("\n [SUCCESS] Missing Dates Patched from Metadata:")
      message(paste(patched_logs, collapse = "\n"))
      
      tryCatch({
        log_qflag(
          severity = "INFO", 
          category = "DATES", 
          message = sprintf("Metadata Imputation: Successfully patched missing dates in %d observation dataframe(s) using '%s'.", length(patched_logs), df_meta_date_col)
        )
      }, error = function(e) {})
    }
    
    # 2. Handle Year Corrections
    if (length(year_correction_logs) > 0) {
      year_msg <- paste(
        "",
        "----------------------------------------------------------------------",
        sprintf(" \U000026A0\U0000FE0F YEAR MISMATCHES CORRECTED (Target Year: %d) \U000026A0\U0000FE0F", target_year),
        "----------------------------------------------------------------------",
        " Dates with mismatched years were identified and forced to target year:",
        paste("   ->", unique(year_correction_logs), collapse = "\n"),
        "----------------------------------------------------------------------",
        sep = "\n"
      )
      
      message(year_msg)
      
      tryCatch({
        log_qflag(
          severity = "WARN", 
          category = "DATES", 
          message = sprintf("Year Standardization: Corrected year mismatches across %d dataset record(s) to align with reference year %d.", length(year_correction_logs), target_year)
        )
      }, error = function(e) {})
      
      warning(sprintf("Year mismatches corrected: Force-synced dates to target year %d. See logs for details.", target_year), 
              call. = FALSE, immediate. = TRUE)
    }
    
    # 3. Handle Failures (Missing Metadata for Missing Dates)
    if (length(failed_logs) > 0) {
      fail_msg <- paste(
        "",
        "----------------------------------------------------------------------",
        " \U000026A0\U0000FE0F ORPHANED DATA WARNING: MISSING METADATA DATES \U000026A0\U0000FE0F",
        "----------------------------------------------------------------------",
        " The following dataframes contain missing dates, but no valid fallback",
        sprintf(" date was found in the metadata column: '%s'", df_meta_date_col),
        "",
        paste(failed_logs, collapse = "\n"),
        "----------------------------------------------------------------------",
        sep = "\n"
      )
      
      message(fail_msg)
      
      tryCatch({
        log_qflag(
          severity = "WARN", 
          category = "DATES", 
          message = sprintf("Orphaned Data: %d dataframe(s) have missing dates but lack valid fallback data in metadata.", length(failed_logs))
        )
      }, error = function(e) {})
      
      warning(sprintf("Orphaned data detected: %d dataframe(s) missing dates could not be imputed via metadata.", length(failed_logs)), 
              call. = FALSE, immediate. = TRUE)
    }
    
    message("\n======================================================================\n")
  }
  
  return(res)
}