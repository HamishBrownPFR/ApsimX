#' Impute or Exclude Missing Phenology Dates (Chronologically Safe)
#'
#' @description
#' Scans the final wide APSIM phenology input matrix for missing dates (NAs).
#' - Tier 1: Drops columns entirely missing.
#' - Tier 2: Imputes gaps using group averages.
#' - Tier 3: If group average violates chronology, forces a mathematical mid-point and warns user.
#'
#' @param df Wide dataframe of APSIM phenology inputs (SimulationName + DateToProgress).
#' @param group_keys Character vector of strings to group by.
#' @return The dataframe formatted safely for APSIM.
#' @export
do_averages_for_missing_pheno <- function(df, group_keys) {
  
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' required.")
  
  stage_cols <- setdiff(names(df), "SimulationName")
  imputation_logs <- c()
  dropped_logs <- c()
  tier3_logs <- c() # Track the forced mid-points
  
  for (col_idx in seq_along(stage_cols)) {
    col <- stage_cols[col_idx]
    actual_col_idx <- which(names(df) == col) 
    
    # Safe check that prevents strict type-cast errors on Date columns
    na_idx <- which(is.na(df[[col]]) | as.character(df[[col]]) == "")
    
    # ==========================================================
    # TIER 1: ENTIRE COLUMN MISSING 
    # ==========================================================
    if (length(na_idx) == nrow(df)) {
      df[[col]] <- NULL
      dropped_logs <- c(dropped_logs, sprintf(" -> EXCLUDED: '%s'", col))
      
      # ==========================================================
      # TIER 2: PARTIALLY MISSING (With Tier 3 Safety Rails)
      # ==========================================================
    } else if (length(na_idx) > 0) {
      
      for (i in na_idx) {
        current_sim <- df$SimulationName[i]
        
        # 1. Match Group
        matched_group <- NULL
        for (key in group_keys) {
          if (grepl(key, current_sim, ignore.case = TRUE)) {
            matched_group <- key
            break
          }
        }
        
        # 2. Get Group Rows
        if (!is.null(matched_group)) {
          group_rows <- grep(matched_group, df$SimulationName, ignore.case = TRUE)
        } else {
          group_rows <- 1:nrow(df) 
        }
        
        # 3. Calculate Initial Average
        raw_dates <- df[[col]][group_rows]
        valid_dates_str <- raw_dates[!is.na(raw_dates) & as.character(raw_dates) != ""]
        
        if (length(valid_dates_str) > 0) {
          valid_dates <- suppressWarnings(lubridate::parse_date_time(valid_dates_str, orders = c("dmy", "ymd", "Ymd")))
          valid_dates <- as.Date(valid_dates[!is.na(valid_dates)])
          
          if (length(valid_dates) > 0) {
            avg_date <- as.Date(round(mean(as.numeric(valid_dates))), origin = "1970-01-01")
            
            # ==========================================================
            # 4. CHRONOLOGICAL INTEGRITY CHECK & TIER 3 INTERVENTION
            # ==========================================================
            prev_date <- NA
            next_date <- NA
            
            # Search backward safely (using seq to count backwards properly)
            if (actual_col_idx > 2) { 
              for (p in seq(actual_col_idx - 1, 2, by = -1)) {
                val <- df[[p]][i]
                if (!is.na(val) && as.character(val) != "") {
                  parsed_p <- suppressWarnings(lubridate::parse_date_time(val, orders = c("dmy", "ymd", "Ymd")))
                  if (!is.na(parsed_p)) { prev_date <- as.Date(parsed_p); break }
                }
              }
            }
            
            # Search forward safely (STRICTLY LESS THAN ncol to avoid 11:10 quirk)
            if (actual_col_idx < ncol(df)) {
              for (n in seq(actual_col_idx + 1, ncol(df), by = 1)) {
                val <- df[[n]][i]
                if (!is.na(val) && as.character(val) != "") {
                  parsed_n <- suppressWarnings(lubridate::parse_date_time(val, orders = c("dmy", "ymd", "Ymd")))
                  if (!is.na(parsed_n)) { next_date <- as.Date(parsed_n); break }
                }
              }
            }
            
            # Validate Timeline
            is_valid <- TRUE
            if (!is.na(prev_date) && avg_date < prev_date) is_valid <- FALSE
            if (!is.na(next_date) && avg_date > next_date) is_valid <- FALSE
            
            # TIER 3: MID-POINT CLAMPING (If timeline is invalid)
            if (!is_valid) {
              original_avg <- format(avg_date, "%d-%b-%Y") # Changed to dd-MMM-yyyy
              
              if (!is.na(prev_date) && !is.na(next_date)) {
                mid_num <- as.numeric(prev_date) + (as.numeric(next_date) - as.numeric(prev_date)) / 2
                avg_date <- as.Date(round(mid_num), origin = "1970-01-01")
              } else if (!is.na(prev_date)) {
                avg_date <- prev_date + 1
              } else if (!is.na(next_date)) {
                avg_date <- next_date - 1
              }
              
              tier3_logs <- c(
                tier3_logs,
                sprintf(
                  " -> [!] %s | Stage: %s \n      Invalid Avg: %s | FORCED MID-POINT: %s \n      (Bounds: Prev= %s, Next= %s)",
                  current_sim, col, original_avg, format(avg_date, "%d-%b-%Y"), 
                  ifelse(is.na(prev_date), "None", format(prev_date, "%d-%b-%Y")), 
                  ifelse(is.na(next_date), "None", format(next_date, "%d-%b-%Y"))  
                )
              )
            }
            
            # 5. Final Injection
            formatted_avg <- format(avg_date, "%d-%b-%Y") 
            df[[col]][i] <- formatted_avg
            
            if (is_valid) {
              group_label <- ifelse(is.null(matched_group), "GLOBAL FALLBACK", matched_group)
              imputation_logs <- c(
                imputation_logs, 
                sprintf(" -> IMPUTED (Clean): [%s] filled '%s' with %s (Group: %s)", current_sim, col, formatted_avg, group_label)
              )
            }
          }
        }
      }
    }
  }
  
  # ==========================================================
  # REPORTING (With Quarto Q-Flags and Console Warnings)
  # ==========================================================
  if (length(dropped_logs) > 0 || length(imputation_logs) > 0 || length(tier3_logs) > 0) {
    
    # 1. Print General Adjustments
    message("\n======================================================================")
    message(" \U0001F527 PHENOLOGY MATRIX ADJUSTMENTS (TEMPORARY FIXES APPLIED) \U0001F527")
    message("======================================================================")
    
    if (length(dropped_logs) > 0) {
      message("\n [TIER 1] PARAMETERS FULLY EXCLUDED (Lacking Data):")
      message(paste(dropped_logs, collapse = "\n"))
      
      # ---> Quarto Q-Flag: Tier 1
      tryCatch({
        log_qflag(
          severity = "INFO", 
          category = "PHENOLOGY", 
          message = sprintf("Tier 1 Data Rescue: Excluded %d phenology stage column(s) entirely lacking data.", length(dropped_logs))
        )
      }, error = function(e) {})
    }
    
    if (length(imputation_logs) > 0) {
      message("\n [TIER 2] MISSING DATES IMPUTED (Group Averaged & Valid):")
      message(paste(imputation_logs, collapse = "\n"))
      
      # ---> Quarto Q-Flag: Tier 2
      tryCatch({
        log_qflag(
          severity = "WARN", 
          category = "PHENOLOGY", 
          message = sprintf("Tier 2 Data Rescue: Safely imputed %d missing phenology date(s) using chronological group averages.", length(imputation_logs))
        )
      }, error = function(e) {})
    }
    
    # 2. Print Massive Tier 3 Alarm & Trigger Quarto Warning
    if (length(tier3_logs) > 0) {
      
      # Build the massive console block
      tier3_msg <- paste(
        "",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        " \U0001F6A8 CRITICAL ALARM: CHRONOLOGY FORCED (TIER 3 INTERVENTION) \U0001F6A8",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        " The following group averages violated biological chronological logic.",
        " Dates were artificially forced to the mid-point of available bounds.",
        " ",
        " ACTION REQUIRED: YOU MUST REVIEW THE RAW DATA FOR THESE SIMULATIONS!",
        "----------------------------------------------------------------------",
        paste(tier3_logs, collapse = "\n\n"),
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
        "",
        sep = "\n"
      )
      
      message(tier3_msg)
      
      # ---> Quarto Q-Flag: Tier 3
      tryCatch({
        log_qflag(
          severity = "FATAL", # Elevated severity for Tier 3
          category = "PHENOLOGY", 
          message = sprintf("Tier 3 Data Rescue (Chronology Forced): %d date(s) violated biological timeline and were mathematically forced to mid-points. MANUAL REVIEW REQUIRED.", length(tier3_logs))
        )
      }, error = function(e) {})
      
      # Trigger standard R warning for terminal orchestrator
      warning("Phenology Chronology Forced (Tier 3 Intervention). Review raw data immediately. See logs for details.", 
              call. = FALSE, immediate. = TRUE)
    } else {
      message("\n======================================================================\n")
    }
  }
  
  return(df)
}