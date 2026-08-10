#' Derive Interpolated Phenology Dates from Haun Stage Observations (Step 3 - Universal Engine)
#'
#' @description
#' A unified Step 3 pipeline component that extracts main-stem leaf emergence records, 
#' calculates the Final Leaf Number (FLN), and applies a monotonic linear approximation to map 
#' specific physiological milestones—Terminal Spikelet (FLN - 3) and Double Ridge (max(2, FLN - 6))—
#' back into true calendar dates.
#'
#' @details
#' **Console Diagnostic Trace:** This function computes wide internal metrics (FLN, Max Leaf limits) 
#' and prints them directly to the console for math-tracing visibility, before melting only the 
#' valid APSIM stages (4 and 5) into the strict 3-column pipeline interface.
#'
#' @param df_input Data frame or List. Can be either a flat, wide observation data frame (Grass) 
#'   or a compiled nested tibble list (Wagga).
#' @param max_leaf_limit Numeric. Fractional threshold coefficient used to pinpoint the date 
#'   of maximum leaf development (default = 0.95).
#' @param input_type Character. Core ingestion strategy selector. Options are \code{"auto"} (detects structure), 
#'   \code{"list_dfs"} (forces nested parsing), or \code{"df_wide"} (forces flat parsing).
#'
#' @return A validated tidy data frame matching the intermediate interface standard containing 
#'   strictly the calculated morphologically derived stages 4 and 5.
#' @export
derive_pheno_stages_from_haun <- function(df_input, max_leaf_limit = 0.95, input_type = "auto") {
  
  # ---- 1. POLYMORPHIC LAYOUT INGESTION SWITCH ----
  # Ensure the input data container exists before evaluating structure
  if (missing(df_input) || is.null(df_input)) {
    stop("Error [derive_pheno_dates_from_haun]: Input data container argument is missing or null.")
  }
  
  # Automatically detect structure if set to "auto" (distinguishes nested tibbles from flat data frames)
  resolved_type <- input_type
  if (resolved_type == "auto") {
    if (all(c("df_name", "data") %in% names(df_input))) {
      resolved_type <- "list_dfs"
    } else {
      resolved_type <- "df_wide"
    }
  }
  
  # Extract the target working data frame based on the resolved ingestion mode
  if (resolved_type == "list_dfs") {
    haun_idx <- grep("haun", df_input$df_name, ignore.case = TRUE)
    if (length(haun_idx) == 0) {
      stop("Error [derive_pheno_dates_from_haun]: No operational dataset with 'haun' found inside nested list structures.")
    }
    df_working <- df_input$data[[haun_idx[1]]]
  } else if (resolved_type == "df_wide") {
    df_working <- df_input
  } else {
    stop(sprintf("Error [derive_pheno_dates_from_haun]: Invalid input_type selection '%s'. Use 'auto', 'list_dfs', or 'df_wide'.", input_type))
  }
  
  # ---- 2. DYNAMIC FIELD ANCHOR VALIDATION ----
  # Verify that the required tracking simulation column exists
  if (!"SimulationName" %in% names(df_working)) {
    stop("Error [derive_pheno_dates_from_haun]: Core tracker column 'SimulationName' is missing from the working dataset.")
  }
  
  # Locate the observation date column (accepting either 'Clock.Today' or 'Date')
  date_col <- intersect(c("Clock.Today", "Date"), names(df_working))
  if (length(date_col) == 0) {
    stop("Error [derive_pheno_dates_from_haun]: Could not locate 'Clock.Today' or 'Date' headers in data frame.")
  }
  date_col <- date_col[1]
  
  # Locate the Haun stage column using a case-insensitive pattern match
  haun_col <- names(df_working)[grepl("HaunStage", names(df_working), ignore.case = TRUE)]
  if (length(haun_col) != 1) {
    stop("Error [derive_pheno_dates_from_haun]: Unable to isolate a unique 'HaunStage' column in input headers.")
  }
  haun_col <- haun_col[1]
  
  # ---- 3. DATA CLEANING & RECASTING ----
  df_clean <- df_working %>%
    # Drop rows where either the Haun stage or the date value is missing (NA)
    dplyr::filter(!is.na(.data[[haun_col]]), !is.na(.data[[date_col]])) %>%
    dplyr::mutate(
      # Safely convert various date string formats into standard numeric date values
      .temp_date_num = as.numeric(as.Date(
        suppressWarnings(lubridate::parse_date_time(
          as.character(.data[[date_col]]), 
          orders = c("dmy HMS", "ymd HMS", "dmy", "ymd", "Ymd")
        ))
      )),
      # Force Haun stage values to numeric format for downstream calculations
      .target_haun = as.numeric(.data[[haun_col]])
    ) %>%
    # Drop rows where date parsing failed and resulted in NA
    dplyr::filter(!is.na(.temp_date_num))
  
  # ---- 4. MONOTONIC INTERPOLATION ENGINE ----
  df_wide_metrics <- df_clean %>%
    # Sort data chronologically within each simulation by Haun stage progression
    dplyr::arrange(SimulationName, .target_haun) %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::summarise(
      # Calculate the peak leaf development observed for the simulation
      LeafNumberMaximum = max(.target_haun, na.rm = TRUE),
      # Estimate Final Leaf Number (FLN) by rounding the maximum leaf count to an integer
      FLN               = as.integer(round(LeafNumberMaximum)),
      # Calculate the upper leaf development limit using the threshold coefficient
      LeafNumberLimit   = LeafNumberMaximum * max_leaf_limit,
      
      # Determine morphological milestone targets based on FLN rules
      Haun_TS = FLN - 3,             # Terminal Spikelet stage target
      Haun_DR = max(2, FLN - 6),     # Double Ridge stage target (floored at minimum stage 2)
      
      # Interpolate calendar dates corresponding to the calculated morphological targets
      Date_Num_Limit = if (dplyr::n() >= 2 && LeafNumberMaximum > 0) {
        approx(x = .target_haun, y = .temp_date_num, xout = LeafNumberLimit, rule = 2, ties = "mean")$y
      } else { NA_real_ },
      
      Date_Num_TS = if (dplyr::n() >= 2 && LeafNumberMaximum > 0) {
        approx(x = .target_haun, y = .temp_date_num, xout = Haun_TS, rule = 2, ties = "mean")$y
      } else { NA_real_ },
      
      Date_Num_DR = if (dplyr::n() >= 2 && LeafNumberMaximum > 0) {
        approx(x = .target_haun, y = .temp_date_num, xout = Haun_DR, rule = 2, ties = "mean")$y
      } else { NA_real_ },
      
      .groups = "drop"
    )
  
  # ---- 5. INTERNAL LOGIC CONSOLE TRACE ----
  # Reformat numeric dates into true Date objects for diagnostic review
  df_diagnostic <- df_wide_metrics %>%
    dplyr::mutate(
      Date_MaxLeafLimit = as.Date(Date_Num_Limit, origin = "1970-01-01"),
      Date_Stage4_DR    = as.Date(Date_Num_DR, origin = "1970-01-01"),
      Date_Stage5_TS    = as.Date(Date_Num_TS, origin = "1970-01-01")
    ) %>%
    dplyr::select(SimulationName, LeafNumberMaximum, FLN, LeafNumberLimit, 
                  Date_MaxLeafLimit, Date_Stage4_DR, Date_Stage5_TS)
  
  # Print the formatted math-tracing table directly to the console
  message("\n===========================================================")
  message("           HAUN DERIVATION INTERNAL TRACE LOGIC            ")
  message("===========================================================")
  print(as.data.frame(df_diagnostic))
  message("===========================================================\n")
  
  # Count how many simulations successfully generated milestone dates for summary reporting
  sims_with_dr <- sum(!is.na(df_wide_metrics$Date_Num_DR))
  sims_with_ts <- sum(!is.na(df_wide_metrics$Date_Num_TS))
  
  # ---- 6. MELT VERTICAL AND RE-ALIGN TO STANDARD INTERFACE SCHEMA ----
  df_final <- df_wide_metrics %>%
    dplyr::mutate(
      Stage_4 = as.Date(Date_Num_DR, origin = "1970-01-01"), # Map Double Ridge date to Stage 4
      Stage_5 = as.Date(Date_Num_TS, origin = "1970-01-01")  # Map Terminal Spikelet date to Stage 5
    ) %>%
    # Isolate only the target milestone columns to prepare for vertical reshaping
    dplyr::select(SimulationName, Stage_4, Stage_5) %>%
    # Reshape from wide format into a tidy vertical layout
    tidyr::pivot_longer(
      cols = c(Stage_4, Stage_5),
      names_to = "StageKey",
      values_to = "Clock.Today"
    ) %>%
    # Drop rows where date values evaluated to NA
    dplyr::filter(!is.na(Clock.Today)) %>%
    # Map internal stage names to standard numeric APSIM phenology stage codes
    dplyr::mutate(
      Wheat.Phenology.Stage = dplyr::case_when(
        StageKey == "Stage_4" ~ 4, # LeavesInitiating (Double Ridge)
        StageKey == "Stage_5" ~ 5, # SpikeletsDifferentiating (Terminal Spikelet)
        TRUE                  ~ NA_real_
      )
    ) %>%
    # Realign strictly to the mandatory 3-column pipeline interface schema
    dplyr::select(SimulationName, Clock.Today, Wheat.Phenology.Stage) %>%
    dplyr::distinct() %>%
    dplyr::arrange(SimulationName, Clock.Today)
  
  # ---- 7. PIPELINE NOTIFICATION & SUMMARY WARNING ----
  message(sprintf("Success [derive_pheno_stages_from_haun]: Morphologically generated milestone rows from Haun counts. (Mode: %s)", 
                  resolved_type))
  
  message(sprintf(" -> `[Wheat].Phenology.LeavesInitiating.DateToProgress` as Stage 4 generated for %d simulations.", 
                  sims_with_dr))
  
  message(sprintf(" -> `[Wheat].Phenology.SpikeletsDifferentiating.DateToProgress` as Stage 5 generated for %d simulations.", 
                  sims_with_ts))
  
  # Log a machine-readable Q-Flag entry for the Quarto report dashboard
  log_qflag(
    severity = "INFO", 
    category = "PHENOLOGY", 
    message = sprintf("Haun derivation (Step 3): successfully generated morphologically derived stages for %d simulation(s) (Stage 4: %d, Stage 5: %d).", nrow(df_wide_metrics), sims_with_dr, sims_with_ts)
  )
  
  return(df_final)
}