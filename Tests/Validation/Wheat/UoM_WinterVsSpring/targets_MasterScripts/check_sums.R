library(dplyr)
library(rlang)
library(cli)

#' Validate Composite Variables in Observation Data
#'
#' @description
#' Performs a mass-balance audit comparing a reference variable against the sum 
#' of its component variables. Displays a prominent warning table for discrepancies 
#' or a technical audit confirmation when mass balance is verified.
#' 
#' @param df Data frame containing the observation data.
#' @param ref_var String. Target reference column (e.g., "Wheat.AboveGround.Wt").
#' @param comp_vars Character vector. Component columns to sum (e.g., c("Wheat.Leaf.Wt", "Wheat.Stem.Wt")).
#' @param tolerance Numeric. Variance percentage threshold to flag (default = 5%).
#' @return A data frame with identifiers, reference variable, component breakdown, sum, absolute difference, and % difference.
check_sums <- function(df, ref_var, comp_vars, tolerance = 5) {
  
  # ---- 1. DEFENSIVE CHECKS ----
  req_vars <- c(ref_var, comp_vars)
  missing_vars <- setdiff(req_vars, names(df))
  
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "x" = "Cannot run check_sums. Missing columns in dataframe:",
      "i" = "{.var {missing_vars}}"
    ))
  }
  
  # ---- 2. COMPOSITE MATH ----
  df_calc <- df
  df_calc$Comp_Sum <- rowSums(df_calc[comp_vars], na.rm = TRUE)
  
  # ---- 3. LOGIC & FORMATTING ----
  df_out <- df_calc %>%
    mutate(
      # Calculate the raw difference in absolute terms (units)
      DiffAbs = Comp_Sum - .data[[ref_var]],
      
      # Calculate the percentage difference
      DiffPerc = case_when(
        is.na(.data[[ref_var]]) ~ NA_real_,
        .data[[ref_var]] == 0 & Comp_Sum == 0 ~ 0,
        .data[[ref_var]] == 0 & Comp_Sum != 0 ~ Inf,
        TRUE ~ (DiffAbs / .data[[ref_var]]) * 100
      ),
      # Round both for clean viewing in the console
      DiffAbs = round(DiffAbs, 2),
      DiffPerc = round(DiffPerc, 2)
    ) %>%
    # Select identifiers, main reference, calculated sum, absolute diff, % diff, and components
    select(
      any_of(c("SimulationName", "Clock.Today", "Date")),
      all_of(ref_var),
      Comp_Sum,
      DiffAbs,
      DiffPerc,
      all_of(comp_vars)
    )
  
  # ---- 4. DISCREPANCY AUDIT & LOGGING ----
  flagged_rows <- df_out %>%
    filter(!is.na(DiffPerc) & abs(DiffPerc) > tolerance)
  
  flagged_count <- nrow(flagged_rows)
  
  if (flagged_count > 0) {
    # --- BIG WARNING BANNER ---
    cat(paste0(
      "\n======================================================================\n",
      " \u26A0\uFE0F  MASS BALANCE AUDIT WARNING: DISCREPANCIES DETECTED \u26A0\uFE0F \n",
      "======================================================================\n",
      sprintf(" Target Reference Variable : %s\n", ref_var),
      sprintf(" Component Fractions        : %s\n", paste(comp_vars, collapse = " + ")),
      sprintf(" Discrepancy Threshold      : > %g%%\n", tolerance),
      sprintf(" Flagged Observations      : %d row(s) out of %d\n", flagged_count, nrow(df)),
      "----------------------------------------------------------------------\n\n"
    ))
    
    # Print formatted discrepancy table directly to the console
    print(as.data.frame(flagged_rows), row.names = FALSE)
    
    cat(paste0(
      "\n----------------------------------------------------------------------\n",
      " Action Required: Please inspect the component variables for the rows above.\n",
      "======================================================================\n\n"
    ))
    
    # ---> NEW: Machine-readable Q-Flag for Mass Balance Discrepancy
    log_qflag(
      severity = "WARN", 
      category = "MASS BALANCE", 
      message = sprintf("Mass balance audit failed: %d row(s) exceeded %g%% variance for '%s'.", flagged_count, tolerance, ref_var)
    )
    
    # Official targets warning log
    warning(
      sprintf("Mass balance audit failed: %d row(s) exceeded %g%% variance for '%s'.", 
              flagged_count, tolerance, ref_var), 
      call. = FALSE
    )
    
  } else {
    # --- TECHNICAL SUCCESS BANNER ---
    cat(paste0(
      "\n======================================================================\n",
      " \u2705 MASS BALANCE AUDIT PASSED: ", ref_var, " \u2705 \n",
      "======================================================================\n",
      " -> Technical Verification : Component fraction stoichiometric mass balance confirmed.\n",
      sprintf(" -> Reference Variable     : '%s'\n", ref_var),
      sprintf(" -> Evaluated Fractions    : [%s]\n", paste(comp_vars, collapse = " + ")),
      sprintf(" -> Evaluated Observations : N = %d observations\n", nrow(df)),
      sprintf(" -> Variance Tolerance     : Relative error strictly <= %g%%\n", tolerance),
      "----------------------------------------------------------------------\n\n"
    ))
  }
  
  return(df_out)
}