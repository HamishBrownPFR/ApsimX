#' Extract and Interpolate Phenology Dates from Continuous PCD Lists (Step 1 - Wagga)
#'
#' @description
#' A specialized Step 1 component for the Wagga dataset. It collapses a nested list 
#' of continuous PCD scorings, interpolates the exact date the crop reached the target 
#' development threshold, and maps it to the universal schema.
#'
#' @details
#' **Threshold Fallback Logic:** If a crop's continuous scoring ends before it actually 
#' reaches the target percentage:
#'   - If progress peaked at > 0%: The absolute final observation date is forced as the milestone date.
#'   - If progress remained at 0%: The date evaluates to NA and the record is dropped.
#' Both scenarios trigger explicit console warnings detailing the fallback actions taken.
#'
#' @param list_pcds List of data frames. The output from \code{filter_and_extract_pcds}.
#' @param target_perc Numeric. The target percentage score (e.g., 50) representing stage achievement.
#'
#' @return A validated tidy data frame matching the intermediate interface standard.
#' @export
get_pheno_dates_from_pcd_list <- function(list_pcds, target_perc = 50) {
  
  # Ensure the input is a valid, populated list before attempting extraction
  if (missing(list_pcds) || !is.list(list_pcds) || length(list_pcds) == 0) {
    stop("Error [get_pheno_dates_from_pcd_list]: Input must be a populated list of data frames.")
  }
  
  # ---- 1. EXTRACT DATA & FRACTIONAL SCORES SAFELY ----
  # Iterate over every sheet in the list and bind them vertically into one master data frame
  df_processed <- purrr::map_dfr(names(list_pcds), function(sheet_name) {
    df <- list_pcds[[sheet_name]]
    
    # Dynamically locate the date column (accepting either 'Clock.Today' or 'Date')
    date_col <- intersect(c("Clock.Today", "Date"), names(df))[1]
    
    # Assume the remaining column (that isn't SimulationName or the date) contains the progress values
    val_col <- setdiff(names(df), c("SimulationName", date_col))[1]
    
    df %>%
      # Rename columns dynamically to standard internal tracking names
      dplyr::select(
        SimulationName, 
        Clock.Today = dplyr::all_of(date_col), 
        ProgressValue = dplyr::all_of(val_col)
      ) %>%
      dplyr::mutate(
        # Safely parse a wide variety of date text formats into standard Date objects
        Clock.Today = as.Date(suppressWarnings(lubridate::parse_date_time(
          as.character(Clock.Today), orders = c("dmy HMS", "ymd HMS", "dmy", "ymd", "Ymd")
        ))),
        # Ensure progress values are numeric for interpolation math
        ProgressValue = as.numeric(ProgressValue),
        # Tag the data with its original sheet name so we can map it to APSIM stages later
        PCD_Source = sheet_name
      ) %>%
      # Drop any rows where the date or the progress value could not be read
      dplyr::filter(!is.na(Clock.Today), !is.na(ProgressValue))
  })
  
  # ---- 2. INTERPOLATE EXACT CALENDAR DATE & TRACK FAILURES ----
  df_interpolated <- df_processed %>%
    # Sort chronologically to ensure interpolation logic moves forward in time
    dplyr::arrange(SimulationName, PCD_Source, Clock.Today) %>%
    dplyr::group_by(SimulationName, PCD_Source) %>%
    dplyr::summarise(
      # Calculate progression boundaries to determine which interpolation logic applies
      MaxProgress = max(ProgressValue, na.rm = TRUE),
      MinProgress = min(ProgressValue, na.rm = TRUE),
      MaxDate     = max(Clock.Today, na.rm = TRUE),
      
      # The Fallback Logic Tree: determining the specific date the crop hit 'target_perc'
      Date_Num = if (dplyr::n() >= 2 && MaxProgress >= target_perc && MinProgress <= target_perc) {
        # Primary Route: We have data below and above the threshold. Perform standard linear interpolation.
        approx(x = ProgressValue, y = as.numeric(Clock.Today), xout = target_perc, ties = "mean")$y
        
      } else if (MaxProgress >= target_perc && MinProgress > target_perc) {
        # Fallback 1: Data collection started too late (the first observation is already past the target).
        # Action: Lock the milestone to the very first day data was recorded.
        as.numeric(min(Clock.Today, na.rm = TRUE))
        
      } else if (dplyr::n() == 1 && ProgressValue[1] >= target_perc) {
        # Fallback 2: There is only one single observation in the entire dataset, but it exceeds the target.
        # Action: Accept that single date as the milestone.
        as.numeric(Clock.Today[1])
        
      } else if (MaxProgress > 0 && MaxProgress < target_perc) {
        # Fallback 3: The crop made partial progress, but data collection stopped before it hit the target.
        # Action: Lock the milestone to the absolute final observation date recorded for that simulation.
        as.numeric(max(Clock.Today, na.rm = TRUE))
        
      } else {
        # Total Failure: The crop progress remained at 0% across all observations.
        # Action: Assign NA so this stage can be safely dropped from the timeline later.
        NA_real_
      },
      .groups = "drop"
    ) %>%
    # Convert the interpolated/fallback numeric date back into a standard Date class
    dplyr::mutate(Clock.Today = as.Date(Date_Num, origin = "1970-01-01"))
  
  # ---- 3. DIAGNOSTIC WARNING FOR INCOMPLETE STAGES ----
  # Isolate all simulations that triggered Fallback 3 or Total Failure
  df_failures <- df_interpolated %>% dplyr::filter(MaxProgress < target_perc)
  
  # If any failures exist, construct a detailed console warning block to alert the user
  if (nrow(df_failures) > 0) {
    warning_box <- c(
      "",
      "======================================================================",
      "    ⚠️  WARNING: PHENOLOGY STAGES DID NOT REACH TARGET THRESHOLD  ⚠️",
      "======================================================================",
      sprintf(" Target Threshold Required: %.0f%%", target_perc),
      " The following simulations ended before the stage was fully reached:\n"
    )
    
    # Iterate through each failed simulation to provide a specific, line-by-line explanation
    for (i in seq_len(nrow(df_failures))) {
      sim <- df_failures$SimulationName[i]
      src <- df_failures$PCD_Source[i]
      max_p <- df_failures$MaxProgress[i]
      max_d <- format(df_failures$MaxDate[i], "%Y-%m-%d")
      
      if (max_p == 0) {
        msg <- sprintf("    -> [FAILED - NA]    '%s' | Stage: '%s' | Peaked at 0%%. Dropped from output.", sim, src)
      } else {
        msg <- sprintf("    -> [FORCED - LATE]  '%s' | Stage: '%s' | Peaked at %.1f%%. Forced to final date (%s).", 
                       sim, src, max_p, max_d)
      }
      warning_box <- c(warning_box, msg)
    }
    
    warning_box <- c(warning_box, "======================================================================", "")
    message(paste(warning_box, collapse = "\n"))
    
    # Machine-readable Q-Flag for Incomplete Phenology Stages
    log_qflag(
      severity = "WARN", 
      category = "PHENOLOGY", 
      message = sprintf("Phenology stage target threshold failed for %d simulation/stage record(s): fallback assumptions applied.", nrow(df_failures))
    )
    
    warning("Some phenology stages failed to reach the target threshold and required fallback assumptions. See console.", call. = FALSE)
  }
  
  # ---- 4. ALIGN TO UNIVERSAL 3-COLUMN SCHEMA ----
  df_final <- df_interpolated %>%
    # Remove any rows that evaluated to NA (the Total Failures)
    dplyr::filter(!is.na(Clock.Today)) %>% 
    dplyr::mutate(
      # Map the original Excel sheet names to the strictly required APSIM numeric stage codes using RegEx
      Wheat.Phenology.Stage = dplyr::case_when(
        grepl("Emerg|3", PCD_Source, ignore.case = TRUE) ~ 3,
        grepl("PCDS.*6|Stem|6", PCD_Source, ignore.case = TRUE) ~ 6,
        grepl("Head|7", PCD_Source, ignore.case = TRUE) ~ 7,
        grepl("PCDS.*8|Flower|8", PCD_Source, ignore.case = TRUE) ~ 8,
        grepl("PCDS.*10|Grain|10", PCD_Source, ignore.case = TRUE) ~ 10,
        TRUE ~ NA_real_ # Unrecognized sheets are tagged as NA
      )
    ) %>%
    # Drop any records that could not be mapped to a valid APSIM stage
    dplyr::filter(!is.na(Wheat.Phenology.Stage)) %>%
    # Select only the three columns permitted by the downstream universal interface
    dplyr::select(SimulationName, Clock.Today, Wheat.Phenology.Stage) %>%
    # Remove any accidental duplicates and ensure chronological order
    dplyr::distinct() %>%
    dplyr::arrange(SimulationName, Clock.Today)
  
  message(sprintf("Success [get_pheno_dates_from_pcd_list]: Standardized %d raw records at %.0f%% progress.", 
                  nrow(df_final), target_perc))
  
  # ---> NEW: Machine-readable Q-Flag for Step 1 Phenology Standardization Success
  log_qflag(
    severity = "INFO", 
    category = "PHENOLOGY", 
    message = sprintf("PCD list extraction (Step 1): successfully standardized %d raw record(s) at target progress threshold %.0f%%.", nrow(df_final), target_perc)
  )
  
  return(df_final)
}