#' Audit and Scrutinize Compiled Observation Data (Independent Target Version)
#'
#' @description
#' Performs a comprehensive health check on a nested tibble of compiled observations.
#' Scans for missing values, impossible dates (e.g., Year 0024), missing simulations, 
#' and duplicate entries. Prints a consolidated warning report to the console.
#'
#' @param df_tbl A nested tibble containing `df_name` and `data`.
#' @param expected_sims A character vector OR a data frame. If a data frame is provided, 
#'   the function automatically extracts unique values from its 'SimulationName' column.
#' @param min_year Numeric. The earliest plausible year (default 1990).
#' @param max_year Numeric. The latest plausible year (default 2100).
#'
#' @return A logical `TRUE` if perfectly clean, or `FALSE` if alarms were triggered. 
#'   (Prevents `{targets}` from caching duplicate heavy dataframes).
#' @export
audit_observed_data_tibble_list <- function(df_tbl, expected_sims, min_year = 1990, max_year = 2100) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' required.")
  
  # ---- DYNAMIC EXPECTED SIMS EXTRACTION ----
  if (is.data.frame(expected_sims)) {
    if (!"SimulationName" %in% names(expected_sims)) {
      stop("\n\U0001F6A8 CRITICAL ERROR: The reference dataframe passed to 'expected_sims' does not contain a 'SimulationName' column.", call. = FALSE)
    }
    expected_sims <- unique(as.character(expected_sims$SimulationName))
  } else if (!is.character(expected_sims)) {
    stop("\n\U0001F6A8 CRITICAL ERROR: 'expected_sims' must be either a character vector or a dataframe.", call. = FALSE)
  }
  
  master_alarms <- c()
  total_dfs <- nrow(df_tbl)
  failed_dfs <- 0
  
  for (i in 1:total_dfs) {
    var_name <- df_tbl$df_name[i]
    df       <- df_tbl$data[[i]]
    
    df_alarms <- c()
    
    # Check 0: Completely Empty
    if (nrow(df) == 0) {
      master_alarms <- c(master_alarms, sprintf(" [ %s ] \U0001F480 CRITICAL: Dataframe is completely empty (0 rows)!", var_name))
      failed_dfs <- failed_dfs + 1
      next
    }
    
    # Identify the target variable column
    meta_cols <- c("SimulationName", "Date", "Exp_key_name", "Cultivar", "Plot")
    target_vars <- setdiff(names(df), meta_cols)
    
    # Check 1: Missing SimulationNames
    if (!"SimulationName" %in% names(df)) {
      df_alarms <- c(df_alarms, "  -> \u274C Missing 'SimulationName' column entirely.")
    } else {
      missing_sims <- setdiff(expected_sims, df$SimulationName)
      if (length(missing_sims) > 0) {
        preview <- paste(head(missing_sims, 3), collapse = ", ")
        if (length(missing_sims) > 3) preview <- paste0(preview, ", ... (+", length(missing_sims) - 3, " more)")
        df_alarms <- c(df_alarms, sprintf("  -> \u26A0\uFE0F Missing %d expected SimulationName(s) [e.g., %s]", length(missing_sims), preview))
      }
      
      na_sims <- sum(is.na(df$SimulationName))
      if (na_sims > 0) df_alarms <- c(df_alarms, sprintf("  -> \u274C %d row(s) have NA for SimulationName.", na_sims))
    }
    
    # Check 2 & 3: Date Missing & Date Plausibility (The '0024' Bug)
    if (!"Date" %in% names(df)) {
      df_alarms <- c(df_alarms, "  -> \u274C Missing 'Date' column entirely.")
    } else {
      na_dates <- sum(is.na(df$Date))
      if (na_dates > 0) df_alarms <- c(df_alarms, sprintf("  -> \u274C %d row(s) have NA for Date (Orphaned Data).", na_dates))
      
      valid_dates <- df$Date[!is.na(df$Date)]
      if (length(valid_dates) > 0) {
        years <- lubridate::year(valid_dates)
        bad_years <- sum(years < min_year | years > max_year, na.rm = TRUE)
        if (bad_years > 0) {
          df_alarms <- c(df_alarms, sprintf("  -> \U0001F6A8 %d row(s) have IMPOSSIBLE years (e.g., < %d or > %d). Check for 2-digit year Excel bugs!", bad_years, min_year, max_year))
        }
      }
    }
    
    # Check 4: Missing Target Variable Data
    if (length(target_vars) > 0) {
      for (tv in target_vars) {
        na_vals <- sum(is.na(df[[tv]]))
        if (na_vals > 0) {
          df_alarms <- c(df_alarms, sprintf("  -> \u274C %d row(s) have NA for the actual variable ('%s').", na_vals, tv))
        }
      }
    } else {
      df_alarms <- c(df_alarms, "  -> \u274C No target variable column found to test!")
    }
    
    # Check 5: Duplicate Rows (Same Simulation + Same Date)
    if ("SimulationName" %in% names(df) && "Date" %in% names(df)) {
      dups <- df %>% dplyr::group_by(SimulationName, Date) %>% dplyr::tally() %>% dplyr::filter(n > 1)
      if (nrow(dups) > 0) {
        df_alarms <- c(df_alarms, sprintf("  -> \u26A0\uFE0F %d instance(s) of DUPLICATE rows (Same Simulation + Same Date).", nrow(dups)))
      }
    }
    
    # Compile alarms for this dataframe
    if (length(df_alarms) > 0) {
      master_alarms <- c(master_alarms, sprintf(" [ %s ]", var_name), df_alarms, "")
      failed_dfs <- failed_dfs + 1
    }
  }
  
  # Print the massive report
  if (length(master_alarms) > 0) {
    report_box <- c(
      "\n",
      strrep("=", 80),
      " \U0001F6A8  OBSERVATION DATA HEALTH REPORT (OMNI-SCANNER) \U0001F6A8",
      strrep("=", 80),
      sprintf(" Scanned %d dataframes. Found issues in %d of them.", total_dfs, failed_dfs),
      " Please review the following structural warnings:",
      strrep("-", 80),
      master_alarms,
      strrep("=", 80),
      "\n"
    )
    warning(paste(report_box, collapse = "\n"), call. = FALSE)
    return(invisible(FALSE)) # Returns FALSE to indicate issues were found
  } else {
    message(sprintf("\n\U0001F7E2 SUCCESS: Scanned %d dataframes. Data is completely pristine! No missing values, missing sims, or bad dates detected.\n", total_dfs))
    return(invisible(TRUE)) # Returns TRUE to indicate perfect health
  }
}