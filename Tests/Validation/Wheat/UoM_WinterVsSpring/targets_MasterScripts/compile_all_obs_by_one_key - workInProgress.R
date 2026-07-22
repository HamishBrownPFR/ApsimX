#' Compile and Format All Observed Data (Multi-Key Strict Version with Shield Alarms)
#'
#' @description
#' A robust data-ingestion engine that reads raw field observations from multiple Excel files, 
#' extracts specific variables based on a metadata dictionary, and perfectly aligns them to 
#' APSIM-X `SimulationName`s using a strictly defined unique key (e.g., "Cultivar" or "Plot").
#'
#' @export
compile_all_obs_by_one_keyOLD <- function(folder, excel_files, df_obs_info, df_simNames, unique_key, exp_keys = NULL) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required.")
  
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
  # 1.5. MULTI-KEY EXPANSION (The '|' Delimiter Handler)
  # ------------------------------------------------------------------
  n_orig_mapping <- nrow(df_simNames)
  
  df_simNames <- df_simNames %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(unique_key), as.character)) %>%
    tidyr::separate_longer_delim(cols = dplyr::all_of(unique_key), delim = "\\s*\\|\\s*") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(unique_key), trimws)) %>%
    dplyr::filter(!is.na(.data[[unique_key]]), .data[[unique_key]] != "")
  
  n_expanded <- nrow(df_simNames)
  
  if (n_expanded > n_orig_mapping) {
    message(sprintf(" \U0001F500 MULTI-KEY DETECTED: Safely expanded %d mapped rows into %d individual 1:1 connections using the '|' separator.", n_orig_mapping, n_expanded))
  }
  
  # ------------------------------------------------------------------
  # 2. THE DUPLICATE KEY DEFENDER (Post-expansion scan)
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
          
          # Force evaluation of the target variable name immediately to prevent scoping bugs
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
              # 🚨 THE SHIELD IS BACK: EXTRACTION ALARM 🚨
              message("\n", strrep("=", 60))
              message(" \u26A0\uFE0F  MISSING DATA ALARM: SKIPPING EXTRACTION \u26A0\uFE0F ")
              message(strrep("=", 60))
              message(sprintf(" -> APSIM VAR   : '%s'", name_val))
              message(sprintf(" -> TARGET FILE : %s", basename(path)))
              message(sprintf(" -> TARGET SHEET: '%s'", sh))
              message(sprintf(" -> TARGET COL  : '%s'", col))
              message(sprintf(" -> R ERROR     : %s", e$message))
              message(" -> ACTION      : Pipeline bypassed error and returned empty data.")
              message(strrep("-", 60), "\n")
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
              # 🚨 THE SHIELD IS BACK: MISSING KEY ALARM 🚨
              message("\n", strrep("=", 60))
              message(" \u274C  FATAL JOIN ALARM: MISSING UNIQUE KEY \u274C ")
              message(strrep("=", 60))
              message(sprintf(" -> df_name     : '%s'", name_val))
              message(sprintf(" -> TARGET SHEET: '%s'", sh))
              message(sprintf(" -> MISSING KEY : '%s'", unique_key))
              message(" -> R ERROR     : The extracted data does not contain the required key.")
              message(" -> ACTION      : Skipping mapping join. Returning empty dataframe.")
              message(strrep("-", 60), "\n")
              
              return(dplyr::tibble())
            }
            
            raw_df <- raw_df %>%
              dplyr::mutate(dplyr::across(dplyr::any_of(join_keys), as.character)) %>%
              dplyr::left_join(clean_mapping_df, by = join_keys, relationship = "many-to-one") %>%
              dplyr::relocate(SimulationName, .before = 1)
          }
          
          # ---------------------------------------------------------
          # 4. REPLICATE AGGREGATION & NA PURGE (WITH DATA LOSS ALARMS)
          # ---------------------------------------------------------
          
          if (!"Date" %in% names(raw_df)) {
            raw_df$Date <- as.Date(NA)
          }
          
          if (!target_var %in% names(raw_df)) {
            raw_df[[target_var]] <- NA_real_
          }
          
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
            )
          
          # 🚨 NEW: PARTIAL DATA LOSS ALARM (Bulletproof Base R) 🚨
          missing_dates <- unique(raw_df$SimulationName[is.na(raw_df$Date)])
          missing_vals  <- unique(raw_df$SimulationName[is.na(raw_df[[target_var]])])
          
          if (length(missing_dates) > 0 || length(missing_vals) > 0) {
            message("\n", strrep("=", 60))
            message(" \U0001F6A8  DATA LOSS ALARM: MISSING DATES OR VALUES DETECTED \U0001F6A8 ")
            message(strrep("=", 60))
            message(sprintf(" -> df_name       : '%s'", name_val))
            
            if (length(missing_dates) > 0) {
              message(" -> MISSING DATA  : Date")
              message(" -> AFFECTED SIMS :")
              message(paste("      -", head(missing_dates, 10), collapse = "\n"))
              if (length(missing_dates) > 10) message(sprintf("      ... and %d more.", length(missing_dates) - 10))
            }
            
            if (length(missing_vals) > 0) {
              message(sprintf(" -> MISSING DATA  : Variable ('%s')", target_var))
              message(" -> AFFECTED SIMS :")
              message(paste("      -", head(missing_vals, 10), collapse = "\n"))
              if (length(missing_vals) > 10) message(sprintf("      ... and %d more.", length(missing_vals) - 10))
            }
            message(" -> ACTION        : These incomplete rows will be dropped from APSIM output.")
            message(strrep("-", 60), "\n")
          }
          
          # Strictly drop the rows with missing target values (Base R to avoid tidyverse scoping bugs)
          raw_df <- raw_df[!is.na(raw_df[[target_var]]), ]
          
          # 🚨 NEW: COMPLETE DATA LOSS ALARM 🚨
          if (nrow(raw_df) == 0) {
            message("\n", strrep("=", 60))
            message(" \U0001F480  COMPLETE FAILURE ALARM: DATAFRAME IS 100% EMPTY \U0001F480 ")
            message(strrep("=", 60))
            message(sprintf(" -> df_name       : '%s'", name_val))
            message(" -> R ERROR       : After processing and dropping NAs, 0 rows remain.")
            message(" -> ACTION        : Returning <data.frame[0 x 3]>.")
            message(strrep("-", 60), "\n")
          }
          
          return(raw_df)
        }
      )
    ) %>%
    dplyr::select(df_name, data)
  
  return(final_tibble)
}