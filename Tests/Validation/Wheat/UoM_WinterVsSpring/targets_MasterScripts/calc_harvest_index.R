#' Calculate and Append Harvest Index (Dual Engine)
#'
#' @description
#' Safely calculates the Harvest Index (HI). 
#' By default (Synchronous), it mandates that Grain and Above-Ground Biomass (AGB) 
#' are measured on the exact same day. 
#' If `agb_grain_asynch = TRUE`, it overrides this and calculates an Asynchronous HI 
#' by finding the absolute maximum Grain and maximum AGB across the simulation timeline, 
#' regardless of when they occurred.
#'
#' @export
calc_harvest_index <- function(df, grain_col = "Wheat.Grain.Wt", 
                               agb_col = "Wheat.AboveGround.Wt", hi_col_name = "HarvestIndex", 
                               agb_grain_asynch = FALSE) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
  
  # ---- 1. DEFENSIVE INTEGRITY CHECKS ----
  if (missing(df) || !is.data.frame(df) || nrow(df) == 0) {
    stop("CRITICAL [calc_harvest_index]: Main observation dataframe is missing or empty.")
  }
  
  if (!all(c(grain_col, agb_col) %in% names(df))) {
    warning(sprintf(
      "Warning [calc_harvest_index]: Required columns '%s' or '%s' not found. Returning df unmodified.", 
      grain_col, agb_col
    ))
    return(df)
  }
  
  date_col <- intersect(c("Clock.Today", "Date"), names(df))[1]
  if (is.na(date_col)) stop("CRITICAL [calc_harvest_index]: Missing Date/Clock.Today column.")
  
  # ---- 2. THE DUAL ENGINE ----
  
  if (agb_grain_asynch == TRUE) {
    
    # --- ENGINE A: ASYNCHRONOUS (OVERRIDE) ---
    df_out <- df %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::mutate(
        # Find absolute maximums
        temp_max_grain = suppressWarnings(max(.data[[grain_col]], na.rm = TRUE)),
        temp_max_agb   = suppressWarnings(max(.data[[agb_col]], na.rm = TRUE)),
        
        # Identify the exact dates those maximums occurred
        date_max_grain = .data[[date_col]][which.max(tidyr::replace_na(.data[[grain_col]], -Inf))],
        date_max_agb   = .data[[date_col]][which.max(tidyr::replace_na(.data[[agb_col]], -Inf))]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        # Only print the HI on the row where Grain was measured (Harvest day)
        !!hi_col_name := dplyr::if_else(
          !is.na(.data[[grain_col]]) & !is.infinite(temp_max_grain) & !is.infinite(temp_max_agb) & temp_max_agb > 0,
          temp_max_grain / temp_max_agb,
          NA_real_
        )
      )
    
    # Extract the offset data for the audit log
    async_offsets <- df_out %>%
      dplyr::select(SimulationName, date_max_grain, date_max_agb) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(date_max_grain) & !is.na(date_max_agb) & date_max_grain != date_max_agb)
    
    # Clean up temp columns
    df_out <- df_out %>% dplyr::select(-temp_max_grain, -temp_max_agb, -date_max_grain, -date_max_agb)
    
  } else {
    
    # --- ENGINE B: SYNCHRONOUS (DEFAULT) ---
    df_out <- df %>%
      dplyr::mutate(
        !!hi_col_name := dplyr::if_else(
          !is.na(.data[[grain_col]]) & !is.na(.data[[agb_col]]) & .data[[agb_col]] > 0,
          .data[[grain_col]] / .data[[agb_col]],
          NA_real_
        )
      )
    
    # Find orphaned grain weights
    orphaned_grain <- df_out %>%
      dplyr::filter(!is.na(.data[[grain_col]]) & (is.na(.data[[agb_col]]) | .data[[agb_col]] <= 0))
  }
  
  # ---- 3. THE DIAGNOSTIC ALARM & QC CHECK ----
  valid_hi <- df_out[[hi_col_name]][!is.na(df_out[[hi_col_name]])]
  
  message("\n", strrep("=", 60))
  if (agb_grain_asynch) {
    message(" \U0001F6A8 CALCULATION OVERRIDE: ASYNCHRONOUS HARVEST INDEX \U0001F6A8 ")
  } else {
    message(" \u26A0\uFE0F  CALCULATION COMPLETE: SYNCHRONOUS HARVEST INDEX \u26A0\uFE0F ")
  }
  message(strrep("=", 60))
  
  if (length(valid_hi) > 0) {
    min_hi <- round(min(valid_hi), 3)
    max_hi <- round(max(valid_hi), 3)
    
    message(sprintf(" -> SUCCESS      : %d valid HI values generated.", length(valid_hi)))
    message(sprintf(" -> VALUE RANGE  : %.3f to %.3f", min_hi, max_hi))
  } else {
    message(" -> STATUS       : No valid Harvest Index values could be calculated.")
  }
  
  # ---- 4. CONTEXT-SPECIFIC WARNINGS ----
  
  if (agb_grain_asynch && nrow(async_offsets) > 0) {
    message(strrep("-", 60))
    message(" \U0001F50D ASYNCHRONOUS DATE DISCREPANCY DETECTED \U0001F50D")
    message(" -> WARNING      : HI was calculated using maximum values from DIFFERENT DATES.")
    message(" -> OFFSETS      : See timeline disparities below:")
    
    # ---> NEW: Machine-readable Q-Flag for Asynchronous HI Discrepancy
    log_qflag(
      severity = "WARN", 
      category = "HARVEST INDEX", 
      message = "Asynchronous Harvest Index: HI calculated using maximum values from DIFFERENT DATES."
    )
    
    # Print up to 5 simulations to keep the console clean
    for (i in 1:min(5, nrow(async_offsets))) {
      message(sprintf("    * %s: Grain Peak (%s) vs AGB Peak (%s)", 
                      async_offsets$SimulationName[i], 
                      async_offsets$date_max_grain[i], 
                      async_offsets$date_max_agb[i]))
    }
    if (nrow(async_offsets) > 5) {
      message(sprintf("    * ... and %d more.", nrow(async_offsets) - 5))
    }
  }
  
  if (!agb_grain_asynch && nrow(orphaned_grain) > 0) {
    message(strrep("-", 60))
    message(" \U0001F6A8 DATA LOSS WARNING: ORPHANED GRAIN DETECTED \U0001F6A8")
    message(sprintf(" -> ISSUE        : Found %d instance(s) where Grain has a value, but AGB is NA.", nrow(orphaned_grain)))
    message(" -> ACTION       : HI was NOT calculated for these dates.")
    
    # ---> NEW: Machine-readable Q-Flag for Orphaned Grain
    log_qflag(
      severity = "WARN", 
      category = "HARVEST INDEX", 
      message = sprintf("Orphaned grain detected: %d instance(s) where Grain has a value but AGB is NA.", nrow(orphaned_grain))
    )
  }
  
  message(strrep("=", 60), "\n")
  
  return(df_out)
}