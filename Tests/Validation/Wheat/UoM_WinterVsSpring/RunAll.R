#' Master Script: Run All _targets Pipelines using Relative Paths

# ===================================================================
# PIPELINE EXECUTION OPTION
# ===================================================================
# TRUE  = Destroys the _targets cache in every folder and rebuilds from scratch.
# FALSE = Only runs nodes that are outdated or missing.
clean_build <- TRUE
#clean_build <- FALSE

# Load custom functions
source("targets_MasterScripts/secure_zip_folder.R") # Update this path!
# ===================================================================

# 1. Anchor the script to the root UoM_WinterVsSpring folder
base_wd <- getwd()

# 2. Define your relative paths
pipeline_dirs <- c(
  "Dookie2024",
  "Dookie2025",
  "Gnarwarre2024",
  "Gnarwarre2025",
  "GrassPatch2024",
  "GrassPatch2025",
  "WaggaWagga2024",
  "WaggaWagga2025",
  "Turretfield2024",
  "Fords2025"
)

# ===================================================================
# INITIALIZE STATUS TRACKERS
# ===================================================================
pipeline_status <- list()
qc_status <- "NOT RUN"
zip_status <- "NOT RUN"
export_status <- list()

# 3. Loop through and execute safely
for (p_dir in pipeline_dirs) {
  
  # Build a temporary absolute path safely from the base
  target_dir <- file.path(base_wd, p_dir)
  
  if (!dir.exists(target_dir)) {
    warning("Directory not found, skipping: ", p_dir)
    pipeline_status[[p_dir]] <- "SKIPPED (Not Found)"
    next
  }
  
  message("\n=======================================================")
  message(sprintf(" INITIATING PIPELINE: %s", p_dir))
  message("=======================================================")
  
  # Jump into the specific project folder
  setwd(target_dir)
  
  # Execute the pipeline with your standard console logging
  tryCatch({
    
    # Check the master toggle and destroy if requested
    if (clean_build) {
      message("  [*] Action: Destroying previous pipeline cache...")
      targets::tar_destroy(ask = FALSE)
    } else {
      message("  [*] Action: Updating outdated nodes only...")
    }
    
    message("  [*] Action: Running tar_make()...")
    targets::tar_make(
      callr_arguments = list(
        stdout = "pipeline_log.txt",
        stderr = "pipeline_log.txt"
      )
    )
    message(sprintf(" [\u2713] Successfully completed: %s", p_dir))
    pipeline_status[[p_dir]] <- "SUCCESS"
    
  }, error = function(e) {
    warning(sprintf(" [X] Error in pipeline %s: %s", p_dir, e$message), call. = FALSE)
    pipeline_status[[p_dir]] <- "FAILED"
  })
  
  # CRITICAL: Jump back to the root folder before the loop continues!
  setwd(base_wd)
}

message("\n=======================================================")
message(" ALL PIPELINES EXECUTED.")
message("=======================================================\n")

# ===================================================================
# GENERATE QUALITY CONTROL REPORT
# ===================================================================
message("\n=======================================================")
message(" GENERATING QUARTO QC REPORT...")
message("=======================================================")

qc_status <- tryCatch({
  quarto::quarto_render("QC_Report.qmd")
  message(" [\u2713] QC Report generated successfully: QC_Report.html")
  
  "SUCCESS" # Returns this value to qc_status if it works
  
}, error = function(e) {
  warning(" [X] Failed to generate QC Report: ", e$message)
  
  "FAILED"  # Returns this value to qc_status if it breaks
})

# ===================================================================
# PACKAGE & ENCRYPT OBSERVED FOLDER
# ===================================================================
message("\n=======================================================")
message(" PACKAGING OBSERVED FOLDER...")
message("=======================================================")

zip_status <- tryCatch({
  # Define paths
  html_source <- "QC_Report.html"
  observed_dir <- "Observed"
  html_target <- file.path(observed_dir, html_source)
  
  final_zip_path <- "Observed.zip" 
  password_file <- "secret_pass.txt"
  
  # 1. Inject the HTML report into the Observed folder
  if (file.exists(html_source)) {
    if (!dir.exists(observed_dir)) {
      dir.create(observed_dir, recursive = TRUE)
    }
    file.copy(from = html_source, to = html_target, overwrite = TRUE)
    message(" [\u2713] QC Report duplicated to: ", html_target)
  } else {
    warning(" [!] QC Report not found at source, skipping injection.")
  }
  
  # 2. Execute the Secure Zip function
  message("  [*] Encrypting the Observed folder...")
  final_zip_tracker <- secure_zip_folder(
    input_folder = observed_dir,
    output_zip   = final_zip_path,
    pass_file    = password_file
  )
  message(" [\u2713] Packaging complete! Secure Zip located at: ", final_zip_tracker)
  
  "SUCCESS" # Returns this value to zip_status if it works
  
}, error = function(e) {
  warning(" [X] Failed during packaging phase: ", e$message)
  
  "FAILED"  # Returns this value to zip_status if it breaks
})

# ===================================================================
# PRISTINE MASTER MERGE EXPORT
# ===================================================================
#' Master Merge Preparation: Isolate Core Integration Files
#'
#' @description
#' Scans the base working directory for explicitly defined files, folders, or 
#' wildcards and copies them into a clean, isolated directory for the master merge.
copy_files_for_master_merge <- function(base_wd, export_dir_name, 
                                        filesAndFoldersToExport = c("*.apsimx", "Met", "Inputs", "Observed.zip")) {
  
  message("\n=======================================================")
  message(" \U0001F5C2\U0000FE0F EXECUTING PRISTINE MASTER EXPORT \U0001F5C2\U0000FE0F")
  message("=======================================================")
  
  if (!dir.exists(export_dir_name)) {
    dir.create(export_dir_name, recursive = TRUE)
    message(" [*] Created pristine master directory: ", export_dir_name)
  } else {
    message(" [*] Pristine master directory already exists. Updating contents...")
  }
  
  local_export_status <- list()
  
  for (target in filesAndFoldersToExport) {
    # Resolve exact files, folders, or wildcards at the base_wd level
    search_path <- file.path(base_wd, target)
    matched_items <- Sys.glob(search_path)
    
    if (length(matched_items) == 0) {
      local_export_status[[target]] <- "SKIPPED (Not Found)"
      warning(sprintf(" [X] Target not found: %s", target), call. = FALSE)
      next
    }
    
    copied_count <- 0
    
    for (item_path in matched_items) {
      item_name <- basename(item_path)
      
      if (dir.exists(item_path)) {
        # Handle Directory: Rebuild folder locally and copy contents
        dest_dir <- file.path(export_dir_name, item_name)
        if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
        
        sub_items <- list.files(item_path, full.names = TRUE)
        if (length(sub_items) > 0) {
          file.copy(from = sub_items, to = dest_dir, overwrite = TRUE, recursive = TRUE)
          copied_count <- copied_count + length(sub_items)
        }
      } else if (file.exists(item_path)) {
        # Handle File: Copy directly to root of the pristine target
        file.copy(from = item_path, to = export_dir_name, overwrite = TRUE)
        copied_count <- copied_count + 1
      }
    }
    
    local_export_status[[target]] <- ifelse(copied_count > 0, 
                                            sprintf("SUCCESS (%d items copied)", copied_count), 
                                            "EMPTY (Matched but no files copied)")
  }
  
  message(" [\u2713] Export process completed.")
  return(local_export_status)
}

# Execute pristine merge export (Hardcoded for now in test phase)
analysis_source_wd <- "C:/github/ApsimX/Tests/Validation/Wheat/UoM_WinterVsSpring"
pristine_target_wd <- "C:/github/ApsimX_To_Merge_SpringVsWinterWheat/ApsimX/Tests/Validation/Wheat/UoM_WinterVsSpring"
targets_to_export  <- c("*.apsimx", "Met", "Inputs", "Observed.zip")

export_status <- tryCatch({
  copy_files_for_master_merge(
    base_wd = analysis_source_wd, 
    export_dir_name = pristine_target_wd,
    filesAndFoldersToExport = targets_to_export
  )
}, error = function(e) {
  warning(" [X] Failed to execute pristine master export: ", e$message)
  list(Fatal_Error = "FAILED")
})

# ===================================================================
# FINAL EXECUTION DASHBOARD
# ===================================================================
message("\n")
message("======================================================================")
message(" \U0001F4CB MASTER SCRIPT EXECUTION SUMMARY \U0001F4CB")
message("======================================================================")

# 1. Print Pipeline Results
message(" [PIPELINES]")
for (p_dir in names(pipeline_status)) {
  status <- pipeline_status[[p_dir]]
  icon <- switch(status,
                 "SUCCESS" = "\u2705", # Green check
                 "FAILED"  = "\u274C", # Red X
                 "\u26A0\uFE0F")        # Warning symbol for skipped
  
  # Format with padding so the statuses align neatly in the console
  message(sprintf("   %-20s : %s %s", p_dir, icon, status))
}

# 2. Print Post-Processing Results
message("\n [POST-PROCESSING]")
qc_icon <- ifelse(qc_status == "SUCCESS", "\u2705", "\u274C")
message(sprintf("   %-20s : %s %s", "QC Report Render", qc_icon, qc_status))

zip_icon <- ifelse(zip_status == "SUCCESS", "\u2705", "\u274C")
message(sprintf("   %-20s : %s %s", "Data Packaging", zip_icon, zip_status))

# 3. Print Pristine Export Results
message("\n [PRISTINE MASTER EXPORT]")
if (length(export_status) > 0) {
  for (target in names(export_status)) {
    status <- export_status[[target]]
    # Dynamically assign icon based on keyword in the status string
    icon <- ifelse(grepl("SUCCESS", status), "\u2705", 
                   ifelse(grepl("FAILED", status), "\u274C", "\u26A0\uFE0F"))
    message(sprintf("   %-20s : %s %s", target, icon, status))
  }
} else {
  message(sprintf("   %-20s : \u274C %s", "Export Task", "FAILED / DID NOT RUN"))
}

message("======================================================================")

# 4. Final Warning Trigger (Strict Check)
has_export_failures <- any(grepl("FAILED", unlist(export_status)))

if (any(pipeline_status != "SUCCESS") || qc_status != "SUCCESS" || zip_status != "SUCCESS" || has_export_failures) {
  message(" \U0001F6A8 WARNING: One or more steps failed or were not run. Check the dashboard above.")
  message("======================================================================\n")
} else {
  message(" \U0001F389 ALL SYSTEMS GO: Master script completed entirely without errors.")
  message("======================================================================\n")
}