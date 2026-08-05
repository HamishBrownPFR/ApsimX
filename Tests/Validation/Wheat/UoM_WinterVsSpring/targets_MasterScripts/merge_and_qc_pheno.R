#' Master Merge and Quality Control for Phenology Timelines (Step 4)
#'
#' @description
#' A unified Step 4 pipeline component that ingests the three phenology tracking streams 
#' (Raw, Interpolated, and Haun-derived). It enforces a strict truth hierarchy to resolve 
#' duplicate stage predictions and runs a chronological validation engine to ensure the 
#' crop timeline only moves forward.
#'
#' @details
#' **Truth Hierarchy:** 
#' 1st Priority: `df_raw` (Direct field observations)
#' 2nd Priority: `df_haun` (Morphological derivations)
#' 3rd Priority: `df_int` (Proportional mathematical interpolations)
#'
#' **Chronological Resolution Engine:** If a timeline inversion is detected (e.g., Stage 5 
#' is dated earlier than Stage 4), the engine compares the hierarchy of the two offending 
#' rows. The row with the lower trusted priority is dropped to restore chronological order.
#'
#' @param df_raw Data frame. Standard 3-column output from Step 1 (Raw Ingestion).
#' @param df_haun Data frame. Standard 3-column output from Step 3 (Haun Derivation).
#' @param df_int Data frame. Standard 3-column output from Step 2 (Linear Interpolation).
#'
#' @return A tidy, quality-checked data frame matching the interface standard, strictly 
#'   ordered by `SimulationName` and `Wheat.Phenology.Stage`.
#' @export
merge_and_qc_pheno <- function(df_raw, df_haun, df_int) {
  
  # ---- 1. SAFE DATA INGESTION & HIERARCHY TAGGING ----
  # Internal helper function to safely attach source tracking names and truth priority levels
  prep_df <- function(df, src_name, priority_lvl) {
    if (!is.null(df) && nrow(df) > 0) {
      df %>% dplyr::mutate(Source = src_name, Priority = priority_lvl)
    } else {
      NULL
    }
  }
  
  # Tag each stream with its respective truth hierarchy level (Raw = 1, Haun = 2, Int = 3)
  df_1 <- prep_df(df_raw,  "Raw",  1)
  df_2 <- prep_df(df_haun, "Haun", 2)
  df_3 <- prep_df(df_int,  "Int",  3)
  
  # Combine all available data streams into a single vertical master data frame
  df_all <- dplyr::bind_rows(df_1, df_2, df_3)
  
  # Return an empty schema safely if all input streams contain zero records
  if (nrow(df_all) == 0) {
    warning("Warning [merge_and_qc_pheno]: All input data frames are empty. Returning empty schema.")
    return(data.frame(SimulationName=character(), Clock.Today=as.Date(character(), Wheat.Phenology.Stage=numeric())))
  }
  
  # ---- 2. WARNING 1: HIERARCHY CONFLICT RESOLUTION ----
  # Scan for identical stages within the same simulation predicted with differing dates across streams
  conflicts <- df_all %>%
    dplyr::group_by(SimulationName, Wheat.Phenology.Stage) %>%
    dplyr::filter(dplyr::n() > 1, dplyr::n_distinct(Clock.Today) > 1) %>%
    dplyr::summarise(
      Date_Clash = paste(Clock.Today, collapse = " vs "),
      Source_Clash = paste(Source, collapse = " vs "),
      .groups = "drop"
    )
  
  # Report conflicts to the console and log a machine-readable Q-Flag if clashes occur
  if (nrow(conflicts) > 0) {
    message("\n[!] NOTICE: Contrasting Dates Detected for Identical Stages")
    message("    Applying Truth Hierarchy (Raw > Haun > Int) to resolve. Affected simulations:")
    print(as.data.frame(conflicts))
    
    # Machine-readable Q-Flag for Hierarchy Conflict Resolution
    log_qflag(
      severity = "WARN", 
      category = "PHENOLOGY", 
      message = sprintf("Hierarchy conflict resolution: contrasting dates detected for identical stages across %d simulation(s). Truth hierarchy applied.", nrow(conflicts))
    )
  }
  
  # Enforce Truth Hierarchy: Group by simulation and stage, sort by priority level, and keep only the strongest row
  df_filtered <- df_all %>%
    dplyr::group_by(SimulationName, Wheat.Phenology.Stage) %>%
    dplyr::arrange(Priority, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # ---- 3. WARNING 2: CHRONOLOGICAL VALIDATION ENGINE ----
  # Custom iterative function to inspect and resolve backward-moving timelines per simulation
  fix_chronology <- function(df_sim) {
    dropped_logs <- c()
    df_sim <- df_sim %>% dplyr::arrange(Wheat.Phenology.Stage)
    
    keep_checking <- TRUE
    while(keep_checking) {
      if (nrow(df_sim) <= 1) break
      
      # Calculate date differences between sequential developmental stages
      date_diffs <- as.numeric(df_sim$Clock.Today[-1]) - as.numeric(df_sim$Clock.Today[-nrow(df_sim)])
      inversions <- which(date_diffs < 0) # Identify where a later stage occurs earlier in time
      
      if (length(inversions) == 0) {
        keep_checking <- FALSE # Exit loop when timeline is strictly chronological
      } else {
        # Isolate the first conflicting pair of stages
        idx <- inversions[1] 
        row_A <- df_sim[idx, ]     # The earlier stage row
        row_B <- df_sim[idx + 1, ] # The later stage row
        
        # Compare priority levels to determine which conflicting row to discard (lower score wins)
        if (row_A$Priority <= row_B$Priority) {
          drop_idx <- idx + 1 # Row A is stronger; drop Row B
        } else {
          drop_idx <- idx     # Row B is stronger; drop Row A
        }
        
        # Capture details of the dropped vs. kept records for audit logging
        drop_row <- df_sim[drop_idx, ]
        kept_row <- df_sim[if(drop_idx == idx) idx+1 else idx, ]
        
        msg <- sprintf("Simulation '%s': Stage %s (%s, %s) occurred BEFORE Stage %s (%s, %s). Hierarchy logic dropped Stage %s (%s).",
                       df_sim$SimulationName[1],
                       row_B$Wheat.Phenology.Stage, row_B$Source, row_B$Clock.Today,
                       row_A$Wheat.Phenology.Stage, row_A$Source, row_A$Clock.Today,
                       drop_row$Wheat.Phenology.Stage, drop_row$Source)
        
        dropped_logs <- c(dropped_logs, msg)
        
        # Remove the offending row and restart validation loop checks
        df_sim <- df_sim[-drop_idx, ]
      }
    }
    
    # Attach tracking attributes to the corrected simulation data frame
    attr(df_sim, "chrono_logs") <- dropped_logs
    return(df_sim)
  }
  
  # Split the filtered master frame by simulation, apply the chronological fix, and recombine
  sim_list <- split(df_filtered, df_filtered$SimulationName)
  fixed_list <- lapply(sim_list, fix_chronology)
  
  # Gather all chronological inversion warning logs across simulations
  all_chrono_logs <- unlist(lapply(fixed_list, function(x) attr(x, "chrono_logs")))
  
  # Print warnings to the console and log a Q-Flag if timeline inversions were corrected
  if (length(all_chrono_logs) > 0) {
    message("\n=========================================================================================")
    message(" [!!!] CRITICAL WARNING: CHRONOLOGICAL TIMELINE INVERSIONS DETECTED & CORRECTED")
    message("=========================================================================================")
    for (log_msg in all_chrono_logs) {
      message(" -> ", log_msg)
    }
    message("=========================================================================================\n")
    
    # Machine-readable Q-Flag for Chronological Timeline Inversions
    log_qflag(
      severity = "WARN", 
      category = "PHENOLOGY", 
      message = sprintf("Chronological timeline inversions detected: %d inversion(s) corrected via hierarchy logic.", length(all_chrono_logs))
    )
  }
  
  # ---- 4. CLEANUP AND FINAL INTERFACE ALIGNMENT ----
  # Rebuild the final clean data frame adhering strictly to the 3-column interface standard
  df_final <- dplyr::bind_rows(fixed_list) %>%
    dplyr::select(SimulationName, Clock.Today, Wheat.Phenology.Stage) %>%
    dplyr::arrange(SimulationName, Wheat.Phenology.Stage)
  
  message(sprintf("Success [merge_and_qc_pheno]: Master timeline assembled. Retained %d strictly chronological stage events.", 
                  nrow(df_final)))
  
  return(df_final)
}