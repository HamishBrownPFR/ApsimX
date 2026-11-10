#' Fix Biological Inconsistencies in Forced Emergence Dates (Turretfield 2024)
#'
#' @param filepath Character. The absolute or relative path to the Turretfield2024_PhenoDatesInput.csv
#' @return The filepath of the modified CSV, allowing _targets to track it.
#' @export
fix_emerg_date_Turret24 <- function(filepath) {
  
 # debug filepath <- "C:/github/ApsimX/Tests/Validation/Wheat/UoM_WinterVsSpring/Inputs/Turretfield2024_PhenoDatesInput.csv"
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' is required.")
  if (!requireNamespace("cli", quietly = TRUE)) stop("Package 'cli' is required.")
  
  if (!file.exists(filepath)) {
    stop(sprintf("CRITICAL: File not found at %s", filepath))
  }
  
  # 1. Load Data
  df <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
  
  target_col <- "[Wheat].Phenology.Emerging.DateToProgress"
  
  if (!"SimulationName" %in% names(df) || !target_col %in% names(df)) {
    warning("Missing required columns in the CSV. Returning original file.", call. = FALSE)
    return(filepath)
  }
  
  # Helper: Calculate Statistical Mode (Excluding Impossible Dates!)
  get_mode <- function(dates_vector, sowing_date) {
    valid_dates <- dates_vector[!is.na(dates_vector) & dates_vector > sowing_date]
    uniqv <- unique(na.omit(valid_dates))
    if (length(uniqv) == 0) return(as.Date(NA))
    uniqv[which.max(tabulate(match(valid_dates, uniqv)))]
  }
  
  # 2. Extract Sowing Date from SimulationName and calculate Groups
  # Force all dates to be strict Base R Date objects to prevent lubridate comparison quirks
  df <- df %>%
    dplyr::mutate(
      Parsed_Emergence = as.Date(lubridate::dmy(.data[[target_col]])),
      Group = dplyr::case_when(
        grepl("15-May", SimulationName) ~ "Spring",
        grepl("16-Apr", SimulationName) ~ "Winter",
        TRUE ~ "Unknown"
      ),
      Sowing_Date = dplyr::case_when(
        Group == "Spring" ~ as.Date(lubridate::dmy("15-May-2024")),
        Group == "Winter" ~ as.Date(lubridate::dmy("16-Apr-2024")),
        TRUE ~ as.Date(NA)
      )
    )
  
  #df %>% filter(SimulationName =="Turretfield2024CvScepterSow15-May")
  
  # 3. Calculate the Valid Mode Emergence Date per Group
  modes <- df %>%
    dplyr::group_by(Group, Sowing_Date) %>%
    dplyr::summarise(
      Mode_Emergence = get_mode(Parsed_Emergence, unique(Sowing_Date)),
      .groups = "drop"
    )
  
  # 4. Apply the Fix
  patched_logs <- c()
  
  for (i in seq_len(nrow(df))) {
    if (df$Group[i] == "Unknown" || is.na(df$Parsed_Emergence[i]) || is.na(df$Sowing_Date[i])) next
    
    # Check for biological impossibility: Emergence <= Sowing
    if (as.numeric(df$Parsed_Emergence[i]) <= as.numeric(df$Sowing_Date[i])) {
      
      replacement_date <- modes$Mode_Emergence[modes$Group == df$Group[i]]
      
      if (!is.na(replacement_date)) {
        new_date_str <- format(replacement_date, "%d-%b-%Y")
        old_date_str <- df[[target_col]][i]
        
        df[[target_col]][i] <- new_date_str
        
        patched_logs <- c(patched_logs, 
                          sprintf("   -> %s: Overwrote impossible Emergence date (%s) with valid %s mode value of %s", 
                                  df$SimulationName[i], old_date_str, df$Group[i], new_date_str))
      }
    }
  }
  
  # 5. Save and Clean up
  df <- df %>% dplyr::select(-Parsed_Emergence, -Group, -Sowing_Date)
  
  
  # === DEBUG: FORCE PRINT THE DATAFRAME TO CONSOLE ===
  message("\n--- DEBUG: CURRENT DATAFRAME STATE ---")
  
  # Capture the printed dataframe as a vector of strings
  df_print_output <- capture.output(print(df, row.names = FALSE))
  
  # Use message() to force it through the targets stdout suppression
  message(paste(df_print_output, collapse = "\n"))
  message("--------------------------------------\n")
  # ===================================================
  
  
  
  write.csv(df, filepath, row.names = FALSE, quote = FALSE)
  
  # 6. Reporting & Quarto Q-Flags
  cat("\n=======================================================\n")
  cat(" \U0001F527 FIXING BIOLOGICAL INCONSISTENCIES: TURRETFIELD 2024 \U0001F527\n")
  cat("=======================================================\n")
  
  if (length(patched_logs) > 0) {
    cli::cli_alert_warning(sprintf("Found and corrected %d impossible emergence dates (set <= sowing date).", length(patched_logs)))
    cat(paste(patched_logs, collapse = "\n"), "\n")
    cat(sprintf("\n [\u2713] Overwritten file saved to: %s\n", filepath))
    
    tryCatch({
      log_qflag("WARN", "DATA CLEANING", 
                sprintf("Turretfield2024 Emergence Fix: Corrected %d biologically impossible emergence delays. See console logs for simulation names.", length(patched_logs)))
    }, error = function(e) {})
    
  } else {
    cat(" [\u2713] No impossible emergence dates found. File is biologically sound.\n")
  }
  
  cat("=======================================================\n\n")
  
  return(filepath) 
}
