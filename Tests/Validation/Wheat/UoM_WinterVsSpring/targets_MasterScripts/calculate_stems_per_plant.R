#' Calculate Stems per Plant from APSIM Targets (Anchored with Sync-Check)
#'
#' @description
#' A robust target calculation function that checks if Stem and Plant Population 
#' measurements share the exact same dates within a simulation. If they sync, 
#' it calculates a strict Date-by-Date ratio. If they do not sync, it falls back 
#' to using a single Plant Population average for the entire simulation against 
#' Date-specific Stem averages. Includes range checking, zero-division guards, 
#' and detailed Quarto Q-Flag logging.
#'
#' @param df_obs Dataframe. A wide-format APSIM observed dataset.
#' @param average_by Character. The column name to group by (e.g., "SimulationName").
#' @param stem_per_plant_new_var_name Character. The name of the new column to create.
#' @param stems_per_m2 Character. The column name containing Stems/m2 observations.
#' @param plants_per_m2 Character. The column name containing Plants/m2 observations.
#' @param range_check Numeric Vector. A length-2 vector specifying [min, max] range. Defaults to c(1, 6).
#'
#' @return The original dataframe with the new Stems per Plant variable appended to the exact observation dates.
#' @export
calculate_stems_per_plant <- function(df_obs, average_by, stem_per_plant_new_var_name, stems_per_m2, plants_per_m2, range_check = c(1, 6)) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("CRITICAL: Package 'dplyr' is required.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("CRITICAL: Package 'rlang' is required.")
  
  # ====================================================================
  # 1. INPUT VALIDATION & TYPE SAFETY
  # ====================================================================
  if (!is.data.frame(df_obs)) stop("CRITICAL: 'df_obs' must be a valid dataframe.")
  if (!is.numeric(range_check) || length(range_check) != 2) stop("CRITICAL: 'range_check' must be a numeric vector of exactly length 2.")
  
  req_cols <- c("Clock.Today", average_by, stems_per_m2, plants_per_m2)
  missing_cols <- setdiff(req_cols, names(df_obs))
  if (length(missing_cols) > 0) stop(sprintf("CRITICAL: Missing columns: %s", paste(missing_cols, collapse = ", ")))
  
  if (!is.numeric(df_obs[[stems_per_m2]])) df_obs[[stems_per_m2]] <- suppressWarnings(as.numeric(df_obs[[stems_per_m2]]))
  if (!is.numeric(df_obs[[plants_per_m2]])) df_obs[[plants_per_m2]] <- suppressWarnings(as.numeric(df_obs[[plants_per_m2]]))
  
  na_stems  <- sum(is.na(df_obs[[stems_per_m2]]))
  na_plants <- sum(is.na(df_obs[[plants_per_m2]]))
  
  # Track logging info
  zero_plant_invalids <- 0
  nan_results <- 0
  fallback_logs <- c()
  
  # ====================================================================
  # 2. CORE COMPUTATION (Conditional Sync-Check)
  # ====================================================================
  
  # Create a wrapper function to process each simulation group individually
  process_simulation <- function(df_sim) {
    sim_name <- df_sim[[average_by]][1]
    
    # Identify unique dates where we have valid data for each variable
    dates_stems  <- df_sim$Clock.Today[!is.na(df_sim[[stems_per_m2]])]
    dates_plants <- df_sim$Clock.Today[!is.na(df_sim[[plants_per_m2]])]
    
    # Are the observation dates perfectly in sync?
    dates_in_sync <- length(dates_stems) > 0 && length(dates_plants) > 0 && setequal(dates_stems, dates_plants)
    
    if (dates_in_sync) {
      # PATH A: IN-SYNC (Date-by-Date Math)
      df_sim_summary <- df_sim %>%
        dplyr::group_by(Clock.Today) %>%
        dplyr::summarise(
          Avg_Stems_m2  = mean(.data[[stems_per_m2]], na.rm = TRUE),
          Avg_Plants_m2 = mean(.data[[plants_per_m2]], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # PATH B: OUT-OF-SYNC (Fallback to Simulation Average for Plants)
      fallback_logs <<- c(fallback_logs, sim_name) # Track for reporting
      
      sim_avg_plants <- mean(df_sim[[plants_per_m2]], na.rm = TRUE)
      
      df_sim_summary <- df_sim %>%
        dplyr::group_by(Clock.Today) %>%
        dplyr::summarise(
          Avg_Stems_m2 = mean(.data[[stems_per_m2]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Avg_Plants_m2 = sim_avg_plants)
    }
    
    # Perform math logic
    df_sim_summary <- df_sim_summary %>%
      dplyr::mutate(
        StemsPerPlant = dplyr::case_when(
          is.na(Avg_Plants_m2) | is.nan(Avg_Plants_m2) ~ NA_real_,
          Avg_Plants_m2 <= 0 ~ NA_real_,
          TRUE ~ Avg_Stems_m2 / Avg_Plants_m2
        )
      )
    
    # Track metrics for final log
    zero_plant_invalids <<- zero_plant_invalids + sum(df_sim_summary$Avg_Plants_m2 <= 0, na.rm = TRUE)
    
    # Return the summary table for this simulation
    df_sim_summary %>% dplyr::mutate(!!average_by := sim_name)
  }
  
  # Apply the conditional logic across all simulations
  df_summary_master <- df_obs %>%
    dplyr::group_split(.data[[average_by]]) %>%
    purrr::map_dfr(process_simulation)
  
  # Append back to the exact dates where stems were measured
  df_final <- df_obs %>%
    dplyr::left_join(
      df_summary_master %>% dplyr::select(dplyr::all_of(average_by), Clock.Today, StemsPerPlant),
      by = c(average_by, "Clock.Today")
    ) %>%
    dplyr::mutate(
      !!stem_per_plant_new_var_name := dplyr::if_else(
        !is.na(.data[[stems_per_m2]]), 
        StemsPerPlant, 
        NA_real_
      )
    ) %>%
    dplyr::select(-StemsPerPlant)
  
  # ====================================================================
  # 3. WARNINGS, RANGE EVALUATION & QUARTO Q-FLAGS
  # ====================================================================
  
  out_of_range <- df_summary_master %>%
    dplyr::filter(!is.na(StemsPerPlant) & (StemsPerPlant < range_check[1] | StemsPerPlant > range_check[2]))
  
  message("\n======================================================================")
  message(" \U0001F4CB TARGET CALCULATION SUMMARY: STEMS PER PLANT \U0001F4CB")
  message("======================================================================")
  
  if (na_stems > 0 || na_plants > 0) message(sprintf(" -> [\u26A0\uFE0F] Raw Data: Ignored %d NA(s) in stems and %d NA(s) in plants.", na_stems, na_plants))
  
  # Inform user about Out-Of-Sync Fallback Usage
  if (length(fallback_logs) > 0) {
    message(sprintf(" -> [\u2139\uFE0F] SYNC FALLBACK: Stems and Plants observations were out-of-sync in %d simulation(s).", length(fallback_logs)))
    message("      Action: Reverted to Simulation-level average for Plant Populations.")
    
    tryCatch({ 
      log_qflag("INFO", "CALCULATIONS", sprintf("Sync Fallback: %d simulation(s) (e.g., %s) lacked synced observation dates. Calculated using Simulation-level Plant averages.", length(fallback_logs), fallback_logs[1])) 
    }, error = function(e) {})
  }
  
  if (zero_plant_invalids > 0) {
    message(sprintf(" -> [\u2757] Math Guard: Prevented division by zero in %d instance(s).", zero_plant_invalids))
    tryCatch({ log_qflag("WARN", "CALCULATIONS", sprintf("Division by Zero Guard: %d instances evaluated to <= 0 average plants.", zero_plant_invalids)) }, error = function(e) {})
  }
  
  if (nrow(out_of_range) > 0) {
    bad_sims_str <- paste(sprintf("%s on %s (%.2f)", out_of_range[[average_by]], out_of_range$Clock.Today, out_of_range$StemsPerPlant), collapse = ", ")
    message(sprintf(" -> [\U0001F6A8] RANGE WARNING: %d date(s) fell outside the biological range [%g, %g].", nrow(out_of_range), range_check[1], range_check[2]))
    message(sprintf("      Affected: %s", bad_sims_str))
    tryCatch({ log_qflag("WARN", "TARGETS", sprintf("Range Check Failed: %d dates fell outside [%g, %g].", nrow(out_of_range), range_check[1], range_check[2])) }, error = function(e) {})
  }
  
  mapped_records <- sum(!is.na(df_final[[stem_per_plant_new_var_name]]))
  message(sprintf(" -> [\u2713] Mapped metric to %d physical measurement date(s) across the dataset.", mapped_records))
  message("======================================================================\n")
  
  tryCatch({ log_qflag("INFO", "TARGETS", sprintf("Stems per Plant mapped to %d measurement dates.", mapped_records)) }, error = function(e) {})
  
  return(df_final)
}