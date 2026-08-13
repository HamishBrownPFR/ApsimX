#' Apply Specific Corrections and Metadata Date Imputation (For25)
#'
#' @param df_tbl The compiled list of observed dataframes (output of Phase C)
#' @param folder_path The directory where the correction file should be stored
#' @param ref_date A reference date to enforce the correct year
#' @param file_name_newDates The name of the CSV file to generate/read (Defaults to "dates_to_correct.csv")
#' @param first_sow_date The absolute earliest sowing date. Any data recorded before this date is excluded.
#' @export
apply_corrections_For25 <- function(df_tbl, folder_path, ref_date, 
                                    file_name_newDates = "dates_to_correct.csv", first_sow_date) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' required.")
  
  cat("\n======================================================================\n")
  cat(" 🛠️  PHASE C.2: APPLYING FOR25-SPECIFIC CORRECTIONS \n")
  cat("======================================================================\n")
  
  # Externalized file path
  csv_path <- file.path(folder_path, file_name_newDates)
  
  # ------------------------------------------------------------------
  # 0. THE SWISS CHEESE DATE PARSER (Global to this function)
  # ------------------------------------------------------------------
  parse_any_date <- function(x) {
    if (is.null(x)) return(as.Date(NA))
    final_dates <- as.Date(rep(NA_character_, length(x)))
    
    nums <- suppressWarnings(as.numeric(x))
    num_idx <- which(!is.na(nums))
    if (length(num_idx) > 0) {
      final_dates[num_idx] <- as.Date(nums[num_idx], origin = "1899-12-30")
    }
    
    rem_idx <- which(is.na(final_dates) & !is.na(x) & trimws(as.character(x)) != "")
    if (length(rem_idx) > 0) {
      x_rem <- as.character(x[rem_idx])
      for (fmt in c("%d/%m/%Y", "%d-%m-%Y", "%d/%m/%y", "%d-%m-%y", "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y")) {
        temp_dates <- suppressWarnings(as.Date(x_rem, format = fmt))
        success_idx <- which(!is.na(temp_dates))
        if (length(success_idx) > 0) {
          final_dates[rem_idx[success_idx]] <- temp_dates[success_idx]
          x_rem <- x_rem[-success_idx]        
          rem_idx <- rem_idx[-success_idx]    
        }
        if (length(rem_idx) == 0) break
      }
    }
    return(final_dates)
  }
  
  # ------------------------------------------------------------------
  # 0.5 SAFE TARGET YEAR & SOW DATE EXTRACTION
  # ------------------------------------------------------------------
  if (suppressWarnings(!is.na(as.numeric(ref_date))) && nchar(trimws(as.character(ref_date))) == 4) {
    target_year <- as.numeric(ref_date)
  } else {
    safe_ref <- parse_any_date(as.character(ref_date))
    if (any(is.na(safe_ref))) {
      stop(sprintf("CRITICAL ERROR: 'ref_date' (%s) could not be parsed into a valid date.", ref_date))
    }
    target_year <- as.numeric(format(safe_ref[1], "%Y"))
  }
  
  safe_sow_date <- parse_any_date(as.character(first_sow_date))
  if (any(is.na(safe_sow_date))) {
    stop(sprintf("CRITICAL ERROR: 'first_sow_date' (%s) could not be parsed into a valid date.", first_sow_date))
  }
  sow_date_val <- safe_sow_date[1]
  
  cat(sprintf("   [📅 REFERENCE] Target Year securely locked as: %d\n", target_year))
  cat(sprintf("   [⏳ THRESHOLD] Earliest Sowing Date locked as: %s\n", sow_date_val))
  
  # ------------------------------------------------------------------
  # 1. THE PRE-AUDIT: Who is actually missing dates?
  # ------------------------------------------------------------------
  missing_summary <- df_tbl %>%
    dplyr::mutate(missing_count = purrr::map_int(data, ~sum(is.na(.x$Date)))) %>%
    dplyr::filter(missing_count > 0)
  
  # ------------------------------------------------------------------
  # 1.5 THE AUDIT LOG (Tier i: Informing user about details)
  # ------------------------------------------------------------------
  if (nrow(missing_summary) > 0) {
    purrr::walk2(missing_summary$df_name, missing_summary$data, function(name_val, raw_df) {
      if (is.null(raw_df) || nrow(raw_df) == 0) return()
      missing_count <- sum(is.na(raw_df$Date))
      
      target_vars <- setdiff(names(raw_df), c("SimulationName", "Date", "Plot", "Exp_key_name"))
      warning_box <- c(
        "",
        "----------------------------------------------------------------------",
        sprintf(" ⚠️  MISSING DATE ALARM: '%s' ⚠️", name_val),
        "----------------------------------------------------------------------",
        sprintf(" -> Variable(s)    : [%s]", paste(target_vars, collapse = ", ")),
        sprintf(" -> Missing Dates  : %d rows found without a valid Date!", missing_count),
        "----------------------------------------------------------------------"
      )
      cat(paste(warning_box, collapse = "\n"), "\n")
      
      # Quarto Q-Flag for missing date discovery
      tryCatch({
        log_qflag(
          severity = "WARN",
          category = "DATES",
          message = sprintf("Missing date alarm (For25): %d row(s) in '%s' found without a valid Date.", missing_count, name_val)
        )
      }, error = function(e) {})
    })
  }
  
  # ------------------------------------------------------------------
  # 2. TEMPLATE GENERATION & INITIAL HALT (If template doesn't exist)
  # ------------------------------------------------------------------
  if (nrow(missing_summary) > 0 && !file.exists(csv_path)) {
    template <- data.frame(df_name = missing_summary$df_name, SampleDateApprox = "")
    write.csv(template, csv_path, row.names = FALSE)
    
    stop_msg <- c(
      "",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " ⚠️ FATAL ALARM: PIPELINE HALTED FOR MISSING DATES ⚠️",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " The pipeline cannot proceed because the datasets listed above lack valid dates.",
      "",
      sprintf(" ACTION REQUIRED:", csv_path),
      sprintf(" Either ensure dates are properly structured in your raw data files,", csv_path),
      sprintf(" OR provide a temporary solution via 'SampleDateApprox' in file:", csv_path),
      sprintf(" -> %s", csv_path),
      " Fill in the 'SampleDateApprox' column, save, and run targets::tar_make() again.",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ""
    )
    
    tryCatch({
      log_qflag(
        severity = "FATAL",
        category = "DATES",
        message = sprintf("Pipeline halted (For25): Missing dates detected. Template generated at '%s'.", file_name_newDates)
      )
    }, error = function(e) {})
    
    stop(paste(stop_msg, collapse = "\n"), call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 3. READ & VALIDATE METADATA CORRECTIONS (Tier ii: SampleDateApprox Lookup)
  # ------------------------------------------------------------------
  corrections <- data.frame(df_name = character(), parsed_date = as.Date(character()))
  
  if (file.exists(csv_path)) {
    user_csv <- read.csv(csv_path, stringsAsFactors = FALSE)
    
    # Check for expected column structures (supporting both old 'new_date' and new 'SampleDateApprox')
    date_col_used <- NULL
    if ("SampleDateApprox" %in% names(user_csv)) {
      date_col_used <- "SampleDateApprox"
    } else if ("new_date" %in% names(user_csv)) {
      date_col_used <- "new_date"
    }
    
    if (is.null(date_col_used)) {
      stop(sprintf("\n🚨 CRITICAL ERROR: '%s' must contain a 'SampleDateApprox' column.", file_name_newDates), call. = FALSE)
    }
    
    # Extract rows that have actual metadata fallback entries
    valid_entries <- user_csv %>% 
      dplyr::filter(trimws(df_name) != "" & !is.na(.data[[date_col_used]]) & trimws(as.character(.data[[date_col_used]])) != "")
    
    if (nrow(valid_entries) > 0) {
      corrections <- data.frame(
        df_name = valid_entries$df_name,
        parsed_date = parse_any_date(valid_entries[[date_col_used]])
      )
    }
  }
  
  # ------------------------------------------------------------------
  # 4. SURGICAL INJECTION & (Tier iii) HARD STOP IF UNRESOLVED
  # ------------------------------------------------------------------
  unresolved_dfs <- c()
  
  df_corrected <- df_tbl %>%
    dplyr::mutate(
      data = purrr::pmap(list(df_name, data), function(name_val, raw_df) {
        if (is.null(raw_df) || nrow(raw_df) == 0) return(raw_df)
        
        na_count <- sum(is.na(raw_df$Date))
        
        if (na_count > 0) {
          # Check if a fallback date exists in our corrections table
          match_row <- corrections %>% dplyr::filter(df_name == name_val)
          
          if (nrow(match_row) > 0 && !is.na(match_row$parsed_date[1])) {
            fix_date <- match_row$parsed_date[1]
            raw_df <- raw_df %>% dplyr::mutate(Date = dplyr::if_else(is.na(Date), fix_date, Date))
            
            cat(sprintf("   [✔️ FIXED NAs] '%s' | Original: NA (%d rows) -> Corrected via SampleDateApprox: %s\n", 
                        name_val, na_count, fix_date))
            
            tryCatch({
              log_qflag(
                severity = "WARN", 
                category = "DATES", 
                message = sprintf("Surgical date injection (For25): patched %d missing NA date(s) for '%s' using SampleDateApprox metadata with %s.", na_count, name_val, as.character(fix_date))
              )
            }, error = function(e) {})
            
          } else {
            # Tier (iii): If not found in SampleDateApprox, track as unresolved
            unresolved_dfs <<- c(unresolved_dfs, name_val)
          }
        }
        return(raw_df)
      })
    )
  
  # If any missing dates remain unpatched because they lacked a SampleDateApprox entry -> hard stop()
  if (length(unresolved_dfs) > 0) {
    unresolved_list_str <- paste(unresolved_dfs, collapse = "\n -> ")
    
    fatal_msg <- c(
      "",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " 🚨 CRITICAL ERROR: UNRESOLVED MISSING DATES DETECTED 🚨",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " The following datasets still contain missing dates and no valid fallback",
      " date was found in 'SampleDateApprox':",
      sprintf(" -> %s", unresolved_list_str),
      "",
      sprintf(" ACTION REQUIRED:", csv_path),
      sprintf(" You must either ensure dates are present in the raw data files,", csv_path),
      sprintf(" OR provide a temporary date solution via the 'SampleDateApprox'", csv_path),
      sprintf(" column in file: %s", csv_path),
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ""
    )
    
    tryCatch({
      log_qflag(
        severity = "FATAL",
        category = "DATES",
        message = sprintf("Post-audit firewall failed (For25): %d dataset(s) still missing dates lacking SampleDateApprox entries.", length(unresolved_dfs))
      )
    }, error = function(e) {})
    
    stop(paste(fatal_msg, collapse = "\n"), call. = FALSE)
  }
  
  # ------------------------------------------------------------------
  # 4.5 BULLETPROOF YEAR SWAP RESCUE
  # ------------------------------------------------------------------
  df_corrected <- df_corrected %>%
    dplyr::mutate(
      data = purrr::pmap(list(df_name, data), function(name_val, raw_df) {
        if (is.null(raw_df) || nrow(raw_df) == 0) return(raw_df)
        
        current_years <- as.numeric(format(raw_df$Date, "%Y"))
        mismatch_idx <- which(!is.na(current_years) & current_years != target_year)
        
        if (length(mismatch_idx) > 0) {
          affected_sims <- unique(raw_df$SimulationName[mismatch_idx])
          original_dates_disp <- paste(unique(as.character(raw_df$Date[mismatch_idx])), collapse = ", ")
          
          new_dates_str <- paste(target_year, format(raw_df$Date[mismatch_idx], "%m-%d"), sep="-")
          parsed_new_dates <- as.Date(new_dates_str)
          
          if (any(is.na(parsed_new_dates))) {
            parsed_new_dates[is.na(parsed_new_dates)] <- as.Date(paste(target_year, "02-28", sep="-"))
          }
          
          raw_df$Date[mismatch_idx] <- parsed_new_dates
          corrected_dates_disp <- paste(unique(as.character(raw_df$Date[mismatch_idx])), collapse = ", ")
          
          cat(sprintf("   [🔄 YEAR SWAP] '%s' | Original: [%s] -> Corrected: [%s]\n     -> Affected Sims: %s\n", 
                      name_val, original_dates_disp, corrected_dates_disp, 
                      paste(head(affected_sims, 5), collapse = ", ")))
          
          tryCatch({
            log_qflag(
              severity = "WARN", 
              category = "DATES", 
              message = sprintf("Year swap rescue (For25): synchronized %d row(s) in '%s' to target year %d.", length(mismatch_idx), name_val, target_year)
            )
          }, error = function(e) {})
        }
        return(raw_df)
      })
    )
  
  # ------------------------------------------------------------------
  # 4.7 PRE-SOWING DATA PRUNING
  # ------------------------------------------------------------------
  df_corrected <- df_corrected %>%
    dplyr::mutate(
      data = purrr::pmap(list(df_name, data), function(name_val, raw_df) {
        if (is.null(raw_df) || nrow(raw_df) == 0) return(raw_df)
        
        early_idx <- which(!is.na(raw_df$Date) & raw_df$Date < sow_date_val)
        
        if (length(early_idx) > 0) {
          dropped_sims <- unique(raw_df$SimulationName[early_idx])
          dropped_count <- length(early_idx)
          
          cat(sprintf("   [✂️  PRUNED] '%s' | Removed %d row(s) recorded before %s\n     -> Affected Sims: %s", 
                      name_val, dropped_count, as.character(sow_date_val), 
                      paste(head(dropped_sims, 5), collapse = ", ")))
          if (length(dropped_sims) > 5) {
            cat(sprintf(" (...and %d more)\n", length(dropped_sims) - 5))
          } else {
            cat("\n")
          }
          
          raw_df <- raw_df[-early_idx, ]
          
          tryCatch({
            log_qflag(
              severity = "WARN", 
              category = "DATA MODIFIED", 
              message = sprintf("Pre-sowing pruning (For25): removed %d pre-sowing row(s) from '%s' prior to %s.", dropped_count, name_val, as.character(sow_date_val))
            )
          }, error = function(e) {})
        }
        return(raw_df)
      })
    )
  
  cat("======================================================================\n\n")
  return(df_corrected)
}