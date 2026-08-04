library(dplyr)
library(rlang)
library(cli)

#' Rename and Rescale Observation Variables Dynamically
#'
#' @description
#' Standardizes variable names and applies unit conversions based on a strict 
#' 3-column dictionary CSV (`RawDataName`, `ObservedFileName`, `ScaleBy`).
#' There is no hardcoded fallback; a valid CSV is strictly required.
#'
#' @param df_obs Data frame. The finalized observation dataset.
#' @param mapping_csv_path String. Path to the mapping CSV.
#' @return A processed data frame with renamed and scaled columns.
rename_rescale_obs_vars <- function(df_obs, mapping_csv_path = NULL) {
  
  # ---- 1. DEFENSIVE INTEGRITY CHECKS ----
  if (missing(df_obs) || is.null(df_obs) || nrow(df_obs) == 0) {
    cli::cli_abort("Incoming data frame asset {.arg df_obs} is missing or empty.")
  }
  
  if (is.null(mapping_csv_path) || !file.exists(mapping_csv_path)) {
    cli::cli_abort(c(
      "x" = "Mapping CSV file not found.",
      "i" = "Attempted path: {.file {mapping_csv_path}}"
    ))
  }
  
  # ---- 2. LOAD AND VALIDATE CSV ----
  df_map_cfg <- tryCatch({
    read.csv(mapping_csv_path, stringsAsFactors = FALSE, header = TRUE)
  }, error = function(e) {
    cli::cli_abort("Failed to read the CSV file. Reason: {e$message}")
  })
  
  names(df_map_cfg) <- trimws(names(df_map_cfg))
  
  req_cols <- c("RawDataName", "ObservedFileName", "ScaleBy")
  missing_cols <- setdiff(req_cols, names(df_map_cfg))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "The mapping CSV is missing required columns.",
      "i" = "Missing: {.var {missing_cols}}",
      "i" = "The CSV must contain exactly: {.var {req_cols}}"
    ))
  }
  
  df_map_cfg$RawDataName <- trimws(df_map_cfg$RawDataName)
  df_map_cfg$ObservedFileName <- trimws(df_map_cfg$ObservedFileName)
  
  df_map_cfg$ScaleBy <- suppressWarnings(as.numeric(df_map_cfg$ScaleBy))
  df_map_cfg$ScaleBy[is.na(df_map_cfg$ScaleBy)] <- 1
  
  if (any(duplicated(df_map_cfg$RawDataName))) {
    cli::cli_abort("Duplicate entries found in the {.var RawDataName} column of the mapping CSV.")
  }
  
  # ---- 3. EXECUTE RENAMING ----
  active_map <- df_map_cfg[df_map_cfg$RawDataName %in% names(df_obs), ]
  
  if (nrow(active_map) == 0) {
    cli::cli_alert_warning("No matching raw headers located inside input table. Returning original data.")
    return(df_obs)
  }
  
  rename_vec <- setNames(active_map$RawDataName, active_map$ObservedFileName)
  
  df_processed <- df_obs %>%
    dplyr::rename(!!!rename_vec)
  
  # --- DRAMATIC LOGGING: RENAMING ---
  cat(paste0(
    "\n======================================================================\n",
    " \U0001F504 PIPELINE ACTION: VARIABLES REMAPPED \U0001F504 \n",
    "======================================================================\n"
  ))
  
  for (i in seq_len(nrow(active_map))) {
    cat(sprintf(" -> %-25s ==>  %s\n", active_map$RawDataName[i], active_map$ObservedFileName[i]))
  }
  cat("----------------------------------------------------------------------\n")
  
  # ---> NEW: Machine-readable Q-Flag for Remapping
  log_qflag(
    severity = "INFO", 
    category = "DATA MODIFIED", 
    message = sprintf("Remapped %d variable names to APSIM standards.", nrow(active_map))
  )
  
  # ---- 4. EXECUTE RESCALING ----
  scale_map <- active_map[active_map$ScaleBy != 1, ]
  
  if (nrow(scale_map) > 0) {
    
    # --- DRAMATIC LOGGING: SCALING ---
    cat(paste0(
      "\n======================================================================\n",
      " \u26A0\uFE0F  PIPELINE ACTION: DATA SCALED \u26A0\uFE0F \n",
      "======================================================================\n"
    ))
    
    for (i in seq_len(nrow(scale_map))) {
      col_name <- scale_map$ObservedFileName[i]
      scale_val <- scale_map$ScaleBy[i]
      
      if (is.numeric(df_processed[[col_name]])) {
        df_processed[[col_name]] <- df_processed[[col_name]] * scale_val
        cat(sprintf(" -> \u2705 SUCCESS : %s (Multiplied by %g)\n", col_name, scale_val))
      } else {
        cat(sprintf(" -> \u274C SKIPPED : %s is not numeric!\n", col_name))
      }
    }
    cat("----------------------------------------------------------------------\n")
    
    # ---> NEW: Machine-readable Q-Flag for Scaling
    log_qflag(
      severity = "INFO", 
      category = "DATA MODIFIED", 
      message = sprintf("Scaled %d numeric column(s) based on CSV dictionary multipliers.", nrow(scale_map))
    )
  }
  
  return(df_processed)
}