#' Calculate Stems per Plant from APSIM Targets (Appended to Max Date with Range Checking)
#'
#' @description
#' A robust target calculation function that averages stem and plant populations 
#' by a specified grouping variable (e.g., SimulationName) and derives the final 
#' Stems per Plant metric. It attaches this calculated metric as a new column back 
#' to the exact original dataframe, populated ONLY on the maximum 'Clock.Today' 
#' date for each group. Includes biological range checking, zero-division 
#' guards, and Quarto Q-Flag logging.
#'
#' @param df_obs Dataframe. A wide-format APSIM observed dataset.
#' @param average_by Character. The column name to group by (e.g., "SimulationName").
#' @param stem_per_plant_new_var_name Character. The name of the new column to create.
#' @param stems_per_m2 Character. The column name containing Stems/m2 observations.
#' @param plants_per_m2 Character. The column name containing Plants/m2 observations.
#' @param range_check Numeric Vector. A length-2 vector specifying the [min, max] biologically acceptable range. Defaults to c(1, 6).
#'
#' @return The original dataframe with the new Stems per Plant variable appended at the latest date per group.
#' @export
calculate_stems_per_plant <- function(df_obs, average_by, stem_per_plant_new_var_name, stems_per_m2, plants_per_m2, range_check = c(1, 6)) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("CRITICAL: Package 'dplyr' is required.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("CRITICAL: Package 'rlang' is required.")
  
  # ====================================================================
  # 1. STATE-OF-THE-ART INPUT VALIDATION & SAFETY CHECKS
  # ====================================================================
  if (!is.data.frame(df_obs)) {
    stop("CRITICAL: 'df_obs' must be a valid dataframe.")
  }
  
  if (!is.numeric(range_check) || length(range_check) != 2) {
    stop("CRITICAL: 'range_check' must be a numeric vector of exactly length 2 (e.g., c(1, 6)).")
  }
  
  # Check for missing columns (including Clock.Today check early)
  req_cols <- c("Clock.Today", average_by, stems_per_m2, plants_per_m2)
  missing_cols <- setdiff(req_cols, names(df_obs))
  
  if (length(missing_cols) > 0) {
    stop(sprintf("CRITICAL: Missing required columns in 'df_obs': %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Type-safety enforcement
  if (!is.numeric(df_obs[[stems_per_m2]])) {
    warning(sprintf("Coercing '%s' to numeric.", stems_per_m2), call. = FALSE)
    df_obs[[stems_per_m2]] <- suppressWarnings(as.numeric(df_obs[[stems_per_m2]]))
  }
  if (!is.numeric(df_obs[[plants_per_m2]])) {
    warning(sprintf("Coercing '%s' to numeric.", plants_per_m2), call. = FALSE)
    df_obs[[plants_per_m2]] <- suppressWarnings(as.numeric(df_obs[[plants_per_m2]]))
  }
  
  # Track baseline missingness for logging
  na_stems  <- sum(is.na(df_obs[[stems_per_m2]]))
  na_plants <- sum(is.na(df_obs[[plants_per_m2]]))
  
  # ====================================================================
  # 2. CORE COMPUTATION (Grouping, Averaging & Date Anchoring)
  # ====================================================================
  # Step A: Calculate the single ratio per group
  df_summary <- df_obs %>%
    dplyr::group_by(.data[[average_by]]) %>%
    dplyr::summarise(
      Avg_Stems_m2  = mean(.data[[stems_per_m2]], na.rm = TRUE),
      Avg_Plants_m2 = mean(.data[[plants_per_m2]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Prevent Division by Zero & Calculate Metric
    dplyr::mutate(
      StemsPerPlant = dplyr::case_when(
        is.na(Avg_Plants_m2) ~ NA_real_,
        Avg_Plants_m2 <= 0 ~ NA_real_,
        TRUE ~ Avg_Stems_m2 / Avg_Plants_m2
      )
    )
  
  # Step B: Append back to the original df exactly on the max date per group
  df_final <- df_obs %>%
    dplyr::group_by(.data[[average_by]]) %>%
    dplyr::mutate(
      .is_max_date = Clock.Today == suppressWarnings(max(Clock.Today, na.rm = TRUE))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      df_summary %>% dplyr::select(dplyr::all_of(average_by), StemsPerPlant),
      by = average_by
    ) %>%
    dplyr::mutate(
      !!stem_per_plant_new_var_name := dplyr::if_else(
        .is_max_date & !is.na(.is_max_date), 
        StemsPerPlant, 
        NA_real_
      )
    ) %>%
    dplyr::select(-.is_max_date, -StemsPerPlant)
  
  # ====================================================================
  # 3. WARNINGS, RANGE EVALUATION & QUARTO Q-FLAGS
  # ====================================================================
  zero_plant_invalids <- sum(df_summary$Avg_Plants_m2 <= 0, na.rm = TRUE)
  nan_results         <- sum(is.na(df_summary$StemsPerPlant))
  
  # Biological Range Evaluation
  out_of_range <- df_summary %>%
    dplyr::filter(!is.na(StemsPerPlant) & (StemsPerPlant < range_check[1] | StemsPerPlant > range_check[2]))
  
  message("\n======================================================================")
  message(" \U0001F4CB TARGET CALCULATION SUMMARY: STEMS PER PLANT \U0001F4CB")
  message("======================================================================")
  
  # Log missing raw inputs
  if (na_stems > 0 || na_plants > 0) {
    message(sprintf(" -> [\u26A0\uFE0F] Raw Data: Ignored %d NA(s) in stems and %d NA(s) in plants.", na_stems, na_plants))
  }
  
  # Log mathematical edge cases (Division by Zero)
  if (zero_plant_invalids > 0) {
    message(sprintf(" -> [\u2757] Math Guard: Prevented division by zero in %d group(s). Set to NA.", zero_plant_invalids))
    tryCatch({
      log_qflag(severity = "WARN", category = "CALCULATIONS", 
                message = sprintf("Division by Zero Guard: %d grouping(s) evaluated to <= 0 average plants and were cast to NA.", zero_plant_invalids))
    }, error = function(e) {})
  }
  
  # Log Range Check Failures
  if (nrow(out_of_range) > 0) {
    bad_sims_str <- paste(sprintf("%s (%.2f)", out_of_range[[average_by]], out_of_range$StemsPerPlant), collapse = ", ")
    message(sprintf(" -> [\U0001F6A8] RANGE WARNING: %d group(s) fell outside the expected biological range of [%g, %g].", 
                    nrow(out_of_range), range_check[1], range_check[2]))
    message(sprintf("      Affected: %s", bad_sims_str))
    
    tryCatch({
      log_qflag(severity = "WARN", category = "TARGETS", 
                message = sprintf("Biological Range Check Failed: %d grouping(s) (e.g., %s) fell outside the Stems per Plant range of [%g, %g].", 
                                  nrow(out_of_range), out_of_range[[average_by]][1], range_check[1], range_check[2]))
    }, error = function(e) {})
  }
  
  # Success reporting
  valid_calcs <- nrow(df_summary) - nan_results
  message(sprintf(" -> [\u2713] Success: Processed %d groups. Yielded %d valid '%s' metrics attached to latest dates.", 
                  nrow(df_summary), valid_calcs, stem_per_plant_new_var_name))
  message("======================================================================\n")
  
  tryCatch({
    log_qflag(severity = "INFO", category = "TARGETS", 
              message = sprintf("Stems per Plant calculation completed for %d groups and joined to latest dates.", valid_calcs))
  }, error = function(e) {})
  
  return(df_final)
}