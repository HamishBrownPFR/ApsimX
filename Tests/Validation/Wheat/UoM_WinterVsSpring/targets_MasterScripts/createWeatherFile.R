#' Read and Process Weather Data from Excel with QA/QC and Hard-Stop Time Checks
#'
#' @param thisFolder Character. Directory containing the raw Excel file.
#' @param thisExcelFile Character. Name of the Excel file.
#' @param thisSheet Character. Name of the weather sheet.
#' @return A list containing `data`, `tav`, `amp`, `mapped_cols`, and `missing_vars`.
#' @export
createWeatherFile <- function(thisFolder, thisExcelFile, thisSheet) {
  
  require(dplyr)
  require(lubridate)
  require(readxl)
  require(rlang)
  
  file_path <- file.path(thisFolder, thisExcelFile)
  if (!file.exists(file_path)) stop("CRITICAL ERROR: Weather file not found: ", file_path)
  
  # ------------------------------------------------------------------
  # 1. SMART HEADER DETECTION
  # ------------------------------------------------------------------
  temp_col <- readxl::read_excel(file_path, sheet = thisSheet, col_names = FALSE, n_max = 50)
  header_row_idx <- which(grepl("(?i)date|time", temp_col[[1]]))[1]
  
  if (is.na(header_row_idx)) stop(sprintf("CRITICAL ERROR: Could not locate a 'Date' column in %s.", thisExcelFile))
  
  Weather_raw <- readxl::read_excel(file_path, sheet = thisSheet, skip = header_row_idx - 1, col_types = "text")
  raw_cols <- names(Weather_raw)
  
  # ------------------------------------------------------------------
  # 2. DYNAMIC COLUMN MAPPING
  # ------------------------------------------------------------------
  patterns <- list(
    radn = "(?i)rad",
    maxt = "(?i)max.*t|t.*max|maximum",
    mint = "(?i)min.*t|t.*min|minimum",
    rain = "(?i)rain|precip",
    vp   = "(?i)\\bvp\\b|\\bvapour|\\bvapor", # Added word boundaries (\b)
    et   = "(?i)\\bet\\b|evapotranspiration"  # Added word boundaries to 'et' as well
  )
  
  col_map <- list()
  missing_vars <- c()
  
  # Map essentials
  for (var in c("radn", "maxt", "mint", "rain")) {
    match <- grep(patterns[[var]], raw_cols, value = TRUE)[1]
    if (is.na(match)) stop(sprintf("CRITICAL ERROR: Could not find essential '%s' in %s.", var, thisExcelFile))
    col_map[[var]] <- match
  }
  
  # Map facultatives
  for (var in c("vp", "et")) {
    match <- grep(patterns[[var]], raw_cols, value = TRUE)[1]
    if (!is.na(match)) {
      col_map[[var]] <- match
    } else {
      missing_vars <- c(missing_vars, var)
    }
  }
  
  # ------------------------------------------------------------------
  # 3. WEATHER DATA CREATION LOG (Standard Output)
  # ------------------------------------------------------------------
  log_msg <- c(
    "",
    "=======================================================",
    sprintf(" WEATHER DATA CREATION LOG: %s", thisExcelFile),
    "======================================================="
  )
  for (var in names(col_map)) {
    log_msg <- c(log_msg, sprintf("  [\u2713] %-5s -> '%s'", toupper(var), col_map[[var]]))
  }
  if (length(missing_vars) > 0) {
    log_msg <- c(log_msg, sprintf("  [X] %-5s -> NOT FOUND (Optional - Skipped)", toupper(missing_vars)))
  }
  log_msg <- c(log_msg, "=======================================================", "")
  
  # Print the dedicated log block directly to targets console output
  cat(paste(log_msg, collapse = "\n"))
  
  # ------------------------------------------------------------------
  # 4. DATA CLEANING, COERCION & DATE BULLETPROOFING
  # ------------------------------------------------------------------
  Weather_worked <- Weather_raw
  
  # Bulletproof Date Parsing (Handles both Excel Serials and dd/mm/yyyy strings)
  raw_dates <- suppressWarnings(as.numeric(Weather_worked[[1]]))
  if (all(is.na(raw_dates) | is.null(raw_dates))) {
    Clock.Today <- lubridate::dmy(Weather_worked[[1]])
  } else {
    Clock.Today <- as.Date(lubridate::ymd("1899-12-30") + raw_dates)
  }
  
  Weather_worked <- Weather_worked %>%
    dplyr::mutate(
      Clock.Today = Clock.Today,
      year = lubridate::year(Clock.Today),
      day = lubridate::yday(Clock.Today)
    )
  
  for (var in names(col_map)) {
    orig_name <- col_map[[var]]
    Weather_worked[[var]] <- as.numeric(as.character(Weather_worked[[orig_name]]))
  }
  
  Weather_worked <- Weather_worked %>%
    dplyr::mutate(tav_daily = (maxt + mint) / 2, amp_daily = maxt - mint)
  
  stats_average <- Weather_worked %>%
    dplyr::summarise(tav = round(mean(tav_daily, na.rm = TRUE), 1), amp = round(mean(amp_daily, na.rm = TRUE), 1))
  
  target_cols <- c("Clock.Today", "year", "day", names(col_map))
  met_selected <- Weather_worked %>% dplyr::select(dplyr::all_of(target_cols))
  
  met_out <- met_selected %>%
    dplyr::filter(!is.na(year) & !is.na(day)) %>%
    dplyr::filter(!(is.na(radn) & is.na(maxt) & is.na(mint) & is.na(rain)))
  
  # ------------------------------------------------------------------
  # 5. TIME-SERIES CHECKS 
  # ------------------------------------------------------------------
  if (any(is.na(met_out$Clock.Today))) stop("CRITICAL ERROR: Unparseable dates detected.")
  if (any(duplicated(met_out$Clock.Today))) stop("CRITICAL ERROR: Duplicate dates detected.")
  expected_dates <- seq.Date(from = min(met_out$Clock.Today), to = max(met_out$Clock.Today), by = "day")
  if (length(expected_dates) != nrow(met_out)) stop("CRITICAL ERROR: Missing days in time-series.")
  
  met_out <- met_out %>% dplyr::select(-Clock.Today)
  
  # Return EVERYTHING we need for the next step to document the file properly
  return(list(
    data         = met_out,
    tav          = stats_average$tav,
    amp          = stats_average$amp,
    mapped_cols  = col_map,        
    missing_vars = missing_vars    
  ))
}