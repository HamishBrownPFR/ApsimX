#' Linearly Interpolate Missing Phenology Milestone Stages (Step 2 - Universal Engine)
#'
#' @description
#' A clean, interface-driven Step 2 pipeline component that accepts normalized raw phenology 
#' observations and calculates the missing intermediate micro-milestone Stage 7 (Heading) 
#' based on a fractional progress parameter.
#'
#' @details
#' **Strict Interface Compliance:** This function reads and returns the exact same standard 
#' intermediate three-column schema: \code{(SimulationName, Clock.Today, Wheat.Phenology.Stage)}. 
#' It completely strips away downstream responsibilities like character string reformatting or 
#' hardcoded APSIM path renames, making it universally plug-and-play.
#'
#' @param df_raw Data frame. The normalized output from Step 1 containing \code{SimulationName}, 
#'   \code{Clock.Today} (Date class), and \code{Wheat.Phenology.Stage} (numeric codes).
#' @param btwStgFrac Numeric (0-1). The fractional progress coefficient to interpolate 
#'   between adjacent developmental stages (e.g., \code{0.50} for an exact chronological midpoint).
#'
#' @return A validated tidy data frame matching the intermediate interface standard containing 
#'   strictly the calculated intermediate stage 7.
#' @export
create_interp_pheno_dates <- function(df_raw, btwStgFrac) {
  
  # ---- 1. DEFENSIVE INTEGRITY CHECKS ----
  # Ensure the input data frame exists and actually contains records
  if (missing(df_raw) || is.null(df_raw) || nrow(df_raw) == 0) {
    stop("Error [create_interp_pheno_dates]: Input data frame asset 'df_raw' is missing or empty.")
  }
  
  # Validate that the fractional progress coefficient is provided and mathematically sound (0-1)
  if (missing(btwStgFrac) || is.null(btwStgFrac) || btwStgFrac < 0 || btwStgFrac > 1) {
    stop("Error [create_interp_pheno_dates]: Interpolation parameter 'btwStgFrac' must be a numeric value between 0 and 1.")
  }
  
  # Verify standard interface column presence to prevent downstream crashing
  req_cols <- c("SimulationName", "Clock.Today", "Wheat.Phenology.Stage")
  missing_cols <- setdiff(req_cols, names(df_raw))
  if (length(missing_cols) > 0) {
    stop(paste("Error [create_interp_pheno_dates]: Input data does not match the universal interface schema. Missing:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # ---- 2. PIVOT WIDE FOR SAFE PAIRWISE DATE MATH ----
  # Transform numeric stages into predictable column headers (e.g., "Stage_6", "Stage_8")
  # We use values_fn = max to safely grab the latest date if multiple samples were taken on the same stage
  df_wide <- df_raw %>%
    dplyr::mutate(StageKey = paste0("Stage_", Wheat.Phenology.Stage)) %>%
    dplyr::select(SimulationName, Clock.Today, StageKey) %>%
    tidyr::pivot_wider(
      names_from = StageKey, 
      values_from = Clock.Today,
      values_fn = max 
    )
  
  # ---- 3. CALCULATE INTERMEDIATE CALENDAR MILESTONE (STAGE 7) ----
  # Ensure the anchor columns exist dynamically, even if the raw data is entirely missing them
  allocated_stages <- names(df_wide)
  if (!"Stage_6"  %in% allocated_stages) df_wide$Stage_6  <- as.Date(NA)
  if (!"Stage_8"  %in% allocated_stages) df_wide$Stage_8  <- as.Date(NA)
  
  df_interp_wide <- df_wide %>%
    dplyr::mutate(
      # Stage 7 (Heading) occurs at a specific fractional progress interval 
      # between Stage 6 (Stem Elongation) and Stage 8 (Flowering)
      Stage_7 = Stage_6 + (as.numeric(Stage_8 - Stage_6) * btwStgFrac)
    )
  
  # ---- 4. EXTRACT AND RE-ALIGN TO STANDARD INTERFACE SCHEMA ----
  # Because we are only generating one stage (7), we can bypass heavy pivot_longer operations.
  # We extract the calculated column directly and assign the interface schema variables.
  df_interp_long <- df_interp_wide %>%
    # Isolate just the newly generated Stage 7 dates and rename to the standard 'Clock.Today'
    dplyr::select(SimulationName, Clock.Today = Stage_7) %>%
    # Drop rows where interpolation was impossible (i.e., missing anchor blocks Stage 6 or 8)
    dplyr::filter(!is.na(Clock.Today)) %>%
    # Assign the static stage identifier
    dplyr::mutate(Wheat.Phenology.Stage = 7) %>%
    # Ensure column order matches the exact 3-column specification and drop duplicates
    dplyr::select(SimulationName, Clock.Today, Wheat.Phenology.Stage) %>%
    dplyr::distinct()
  
  # ---- 5. PIPELINE NOTIFICATION LOG ----
  message(sprintf("Success [create_interp_pheno_dates]: Linearly generated %d missing micro-milestone rows (Stage 7).", 
                  nrow(df_interp_long)))
  
  # Machine-readable Q-Flag for Linear Phenology Interpolation
  log_qflag(
    severity = "INFO", 
    category = "PHENOLOGY", 
    message = sprintf("Linear interpolation (Step 2): successfully generated %d intermediate micro-milestone row(s) for Stage 7 using fractional progress %.2f.", nrow(df_interp_long), btwStgFrac)
  )
  
  return(df_interp_long)
}