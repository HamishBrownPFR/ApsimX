#' Compile and Format All Observed Data (Explicit Single-Key Version)
#'
#' @description
#' A robust data-ingestion engine that reads raw field observations from multiple Excel files, 
#' extracts specific variables based on a metadata dictionary, and perfectly aligns them to 
#' APSIM-X `SimulationName`s using a strictly defined unique key (e.g., "Cultivar" or "Plot").
#'
#' @details
#' **Replicate Aggregation:** If the raw data contains multiple replicates for the same 
#' `SimulationName` on the same `Date`, this function automatically groups them and calculates 
#' the mathematical mean, stripping out `NA`s, to provide a single, clean daily value for APSIM.
#' 
#' **Duplicate Key Defense:** Before attempting any joins, the function aggressively scans 
#' the mapping dictionary (`df_simNames`). If it detects that a single unique key maps to 
#' multiple different SimulationNames, it will trigger a fatal alarm to prevent silent data duplication.
#' 
#' **Date Auto-Repair:** Automatically detects and fixes 2-digit Excel year parsing bugs (e.g., Year 0024 to 2024).
#' 
#' **Missing Date Extraction:** Safely extracts data even if the Date column is missing or blank, 
#' assigning `NA` to the date so downstream scripts can attempt to patch it.
#'
#' @export
compile_all_obs_by_one_key <- function(folder, excel_files, df_obs_info, df_simNames, unique_key, exp_keys = NULL) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' required.")
  
  # ------------------------------------------------------------------
  # 1. DEFENSIVE CHECKS: STRICT COLUMN VALIDATION
  # ------------------------------------------------------------------
  req_cols <- c("df_name", "sheet_name", "column_name", "apsim_var_name", "corr_fact")
  missing_cols <- setdiff(req_cols, names(df_obs_info))
  if (length(missing_cols) > 0) {
    stop(sprintf("\n🚨 CRITICAL ERROR: 'df_obs_info' missing required columns: [%s]", 
                 paste(missing_cols, collapse=", ")), call. = FALSE)
  }
  
  if (!unique_key %in% names(df_simNames)) {
    stop(sprintf("\n🚨 CRITICAL ERROR: The unique_key '%s' was not found in your mapping CSV ('df_simNames').", 
                 unique_key), call. = FALSE)
  }
  
  if (!"SimulationName" %in% names(df_simNames)) {
    stop("\n🚨 CRITICAL ERROR: 'SimulationName' is missing from your mapping CSV ('df_simNames').", call. = FALSE)
  }
  
  # Setup Join Keys
  use_keys <- !is.null(exp_keys)
  join_keys <- unique_key
  
  if (use_keys) {
    if (length(excel_files) != length(exp_keys)) {
      stop("\n🚨 CRITICAL ERROR: The number of 'excel_files' must exactly match the number of 'exp_keys'.", call. = FALSE)
    }
    if (!"Exp_key_name" %in% names(df_simNames)) {
      stop("\n🚨 CRITICAL ERROR: You provided 'exp_keys' but 'Exp_key_name' is missing from the mapping CSV.", call. = FALSE)
    }
    join_keys <- c("Exp_key_name", unique_key)
  }
  
  # ------------------------------------------------------------------
  # 2. THE DUPLICATE KEY DEFENDER (Pre-scan mapping table)
  # ------------------------------------------------------------------
  dup_check <- df_simNames %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    dup_str <- paste(capture.output(print(dup_check)), collapse = "\n")
    stop_msg <- c(
      "",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      " \U0001F6A8 FATAL ALARM: DUPLICATE LOOKUP KEYS IN MAPPING TABLE \U0001F6A8 ",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      sprintf(" -> Join Keys Tested: [%s]", paste(join_keys, collapse = ", ")),
      "",
      " -> DUPLICATES FOUND:",
      dup_str,
      "",
      " ACTION: Fix the mapping CSV so each key combination only appears once!",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ""
    )
    stop(paste(stop_msg, collapse = "\n"), call. = FALSE)
  }
  
  clean_mapping_df <- df_simNames %>% 
    dplyr::select(dplyr::all_of(c(join_keys, "SimulationName"))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(join_keys), as.character))
  
  full_paths <- file.path(folder, excel_files)
  iter_keys <- if (use_keys) exp_keys else rep(NA, length(full_paths))
  
  # ------------------------------------------------------------------
  # 3. READ, STACK, AND INJECT
  # ------------------------------------------------------------------
  final_tibble <- df_obs_info %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(df_name, sheet_name, column_name, apsim_var_name, corr_fact),
        function(name_val, sh, col, new_col, corr) {
          
          # Force evaluation of variable name to prevent tidyverse scoping bugs
          target_var <- as.character(new_col)
          
          raw_df <- purrr::map2_dfr(full_paths, iter_keys, function(path, key) {
            
            temp_df <- tryCatch({
              read_observed_one_key(
                file_path   = path,
                SheetName   = as.character(sh),
                VarName     = as.character(col),
                NewVarName  = target_var,
                UnitCorrect = as.numeric(corr),
                unique_key  = unique_key         
              )
            }, error = function(e) {
              message(sprintf("\n \u26A0\uFE0F SKIPPING EXTRACTION: '%s' in sheet '%s'. Error: %s", col, sh, e$message))
              return(NULL)
            })
            
            if (is.null(temp_df) || !is.data.frame(temp_df) || nrow(temp_df) == 0) {
              return(dplyr::tibble())
            }
            
            if (use_keys) temp_df <- temp_df %>% dplyr::mutate(Exp_key_name = key)
            
            return(temp_df)
          })
          
          if (nrow(raw_df) == 0) return(dplyr::tibble())
          
          # ---------------------------------------------------------
          # THE STRICT JOIN
          # ---------------------------------------------------------
          if (!"SimulationName" %in% names(raw_df)) {
            
            if (!unique_key %in% names(raw_df)) {
              message(sprintf("   -> ❌ ERROR in '%s': Raw data does not contain the unique key '%s'. Skipping join.", 
                              name_val, unique_key))
              return(dplyr::tibble())
            }
            
            raw_df <- raw_df %>%
              dplyr::left_join(clean_mapping_df, by = join_keys, relationship = "many-to-one") %>%
              dplyr::relocate(SimulationName, .before = 1)
          }
          
          # ---------------------------------------------------------
          # 3.5. BULLETPROOF DATE CORRECTION (The "Year 24" Fix)
          # ---------------------------------------------------------
          if ("Date" %in% names(raw_df)) {
            raw_df$Date <- as.Date(raw_df$Date) # Ensure it's a date object
            
            # Extract years and find bugs safely ignoring NAs
            yrs <- suppressWarnings(lubridate::year(raw_df$Date))
            bad_idx <- which(!is.na(yrs) & yrs < 100)
            
            if (length(bad_idx) > 0) {
              # If year < 50, assume 20xx. If >= 50, assume 19xx.
              fixed_years <- ifelse(yrs[bad_idx] < 50, yrs[bad_idx] + 2000, yrs[bad_idx] + 1900)
              lubridate::year(raw_df$Date)[bad_idx] <- fixed_years
              
              message(sprintf("   -> 🔧 DATE REPAIR: Auto-corrected %d '2-digit year' bugs (e.g., 0024 -> 2024) for '%s'.", length(bad_idx), target_var))
              
              # ---> NEW: Machine-readable Q-Flag for Date Auto-Repair
              log_qflag(
                severity = "WARN", 
                category = "DATES", 
                message = sprintf("Date auto-repair in '%s': fixed %d '2-digit year' bug(s).", target_var, length(bad_idx))
              )
            }
          }
          
          # ---------------------------------------------------------
          # 3.75. MISSING DATE ALARMS (DATA SURVIVES AS NA)
          # ---------------------------------------------------------
          if (!"Date" %in% names(raw_df)) {
            message("\n", strrep("=", 60))
            message(" 🚨  DATE WARNING: NO DATE COLUMN FOUND 🚨 ")
            message(strrep("=", 60))
            message(sprintf(" -> df_name        : '%s'", name_val))
            message(sprintf(" -> Target Var     : '%s'", target_var))
            message(" -> ISSUE          : The 'Date' column is completely missing from the raw data.")
            message(" -> ACTION         : Data is extracted, but dates are set to NA. Will require downstream patching.")
            message(strrep("-", 60), "\n")
            
            raw_df$Date <- as.Date(NA) # Inject an NA date column so the pipeline doesn't crash
            
            # ---> NEW: Machine-readable Q-Flag for Completely Missing Date Column
            log_qflag(
              severity = "WARN", 
              category = "DATES", 
              message = sprintf("Missing date column in '%s' ('%s'): injected NA dates for downstream patching.", name_val, target_var)
            )
            
          } else {
            na_dates <- sum(is.na(raw_df$Date))
            if (na_dates > 0) {
              message("\n", strrep("=", 60))
              message(" 🚨  MISSING DATE ALARM: ORPHANED DATA 🚨 ")
              message(strrep("=", 60))
              message(sprintf(" -> df_name        : '%s'", name_val))
              message(sprintf(" -> Target Var     : '%s'", target_var))
              message(sprintf(" -> ISSUE          : %d row(s) have 'NA' or blank values in the Date column.", na_dates))
              message(" -> ACTION         : Data is extracted but dates remain NA. Will require downstream patching.")
              message(strrep("-", 60), "\n")
              
              # ---> NEW: Machine-readable Q-Flag for Orphaned Data (NA Dates)
              log_qflag(
                severity = "WARN", 
                category = "DATES", 
                message = sprintf("Orphaned data in '%s' ('%s'): %d row(s) have NA/blank dates requiring patching.", name_val, target_var, na_dates)
              )
            }
          }
          
          # ---------------------------------------------------------
          # 4. REPLICATE AGGREGATION & NA PURGE
          # ---------------------------------------------------------
          # This averages all replicate plots that share the same SimulationName and Date
          raw_df <- raw_df %>%
            dplyr::select(dplyr::any_of(c("SimulationName", "Date", target_var))) %>%
            dplyr::filter(!is.na(SimulationName)) %>%
            dplyr::group_by(SimulationName, Date) %>%
            dplyr::summarise(
              dplyr::across(
                dplyr::all_of(target_var),
                ~ replace(mean(.x, na.rm = TRUE), is.nan(mean(.x, na.rm = TRUE)), NA)
              ),
              .groups = "drop"
            ) %>%
            tidyr::drop_na(dplyr::all_of(target_var))
          
          return(raw_df)
        }
      )
    ) %>%
    dplyr::select(df_name, data)
  
  return(final_tibble)
}