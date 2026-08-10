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
message("======================================================================")

# 3. Final Warning Trigger (Strict Check)
if (any(pipeline_status != "SUCCESS") || qc_status != "SUCCESS" || zip_status != "SUCCESS") {
  message(" \U0001F6A8 WARNING: One or more steps failed or were not run. Check the dashboard above.")
  message("======================================================================\n")
} else {
  message(" \U0001F389 ALL SYSTEMS GO: Master script completed entirely without errors.")
  message("======================================================================\n")
}