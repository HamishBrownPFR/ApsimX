#' Calculate and Append Harvest Index (Synchronous Engine)
#'
#' @description
#' Safely calculates the Harvest Index (HI) synchronously (row-by-row) from Grain Weight 
#' and Total Above-Ground Biomass (AGB). It only calculates HI when both values exist on 
#' the exact same date. It also audits the data for orphaned grain weights.
#'
#' @export
calc_harvest_index <- function(df, grain_col = "Wheat.Grain.Wt", agb_col = "Wheat.AboveGround.Wt", hi_col_name = "HarvestIndex") {
  
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
  
  # ---- 2. SYNCHRONOUS CALCULATION ENGINE ----
  # No grouping needed. We calculate strictly row-by-row.
  df_out <- df %>%
    dplyr::mutate(
      !!hi_col_name := dplyr::if_else(
        !is.na(.data[[grain_col]]) & !is.na(.data[[agb_col]]) & .data[[agb_col]] > 0,
        .data[[grain_col]] / .data[[agb_col]],
        NA_real_
      )
    )
  
  # ---- 3. ORPHANED GRAIN AUDIT ----
  # Find rows where we have grain data, but no AGB to divide it by
  orphaned_grain <- df_out %>%
    dplyr::filter(!is.na(.data[[grain_col]]) & (is.na(.data[[agb_col]]) | .data[[agb_col]] <= 0))
  
  # ---- 4. THE DIAGNOSTIC ALARM & QC CHECK ----
  valid_hi <- df_out[[hi_col_name]][!is.na(df_out[[hi_col_name]])]
  
  message("\n", strrep("=", 60))
  message(" \u26A0\uFE0F  CALCULATION COMPLETE: SYNCHRONOUS HARVEST INDEX \u26A0\uFE0F ")
  message(strrep("=", 60))
  
  if (length(valid_hi) > 0) {
    min_hi <- round(min(valid_hi), 3)
    max_hi <- round(max(valid_hi), 3)
    
    message(sprintf(" -> SUCCESS      : %d valid HI values generated.", length(valid_hi)))
    message(sprintf(" -> VALUE RANGE  : %.3f to %.3f", min_hi, max_hi))
    
    # Biological Plausibility Warning
    if (max_hi > 1.0) {
      message(" -> QC ALARM     : CRITICAL - HI values > 1.0 detected!")
      message(sprintf("                   Check '%s' and '%s' for lab data entry errors.", grain_col, agb_col))
    } else {
      message(" -> QC STATUS    : PASS (All HI values <= 1.0)")
    }
  } else {
    message(" -> STATUS       : No valid Harvest Index values could be calculated.")
    message(sprintf("                   Check if '%s' or '%s' are entirely NA.", grain_col, agb_col))
  }
  
  # Inject the Orphaned Grain Warning if any exist
  if (nrow(orphaned_grain) > 0) {
    message(strrep("-", 60))
    message(" \U0001F6A8 DATA LOSS WARNING: ORPHANED GRAIN DETECTED \U0001F6A8")
    message(sprintf(" -> ISSUE        : Found %d instance(s) where '%s' has a value,", nrow(orphaned_grain), grain_col))
    message(sprintf("                   but '%s' is NA or 0 on the exact same date.", agb_col))
    message(" -> ACTION       : HI was NOT calculated for these dates.")
    
    # Extract a few names to point the user in the right direction
    if ("SimulationName" %in% names(orphaned_grain)) {
      bad_sims <- unique(as.character(orphaned_grain$SimulationName))
      display_sims <- paste(head(bad_sims, 3), collapse = ", ")
      if (length(bad_sims) > 3) display_sims <- paste0(display_sims, "...")
      message(sprintf(" -> AFFECTED     : %s", display_sims))
    }
  }
  
  message(strrep("=", 60), "\n")
  
  return(df_out)
}