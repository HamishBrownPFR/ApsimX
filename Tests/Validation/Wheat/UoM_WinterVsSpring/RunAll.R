#' Master Script: Run All _targets Pipelines using Relative Paths

# ===================================================================
# PIPELINE EXECUTION OPTION
# ===================================================================
# TRUE  = Destroys the _targets cache in every folder and rebuilds from scratch.
# FALSE = Only runs nodes that are outdated or missing.
clean_build <- TRUE
#clean_build <- FALSE
# ===================================================================

# 1. Anchor the script to the root UoM_WinterVsSpring folder
base_wd <- getwd()

# 2. Define your relative paths
pipeline_dirs <- c(
  # "Dookie2024",
  # "Dookie2025",
  # "Gnarwarre2024",
  # "Gnarwarre2025",
  # "GrassPatch2024",
  # "GrassPatch2025",
  # "WaggaWagga2024",
  # "WaggaWagga2025",
  # "Turretfield2024",
  "Fords2025"
)

# 3. Loop through and execute safely
for (p_dir in pipeline_dirs) {
  
  # Build a temporary absolute path safely from the base
  target_dir <- file.path(base_wd, p_dir)
  
  if (!dir.exists(target_dir)) {
    warning("Directory not found, skipping: ", p_dir)
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
    
  }, error = function(e) {
    warning(sprintf(" [X] Error in pipeline %s: %s", p_dir, e$message), call. = FALSE)
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

# Ensure the quarto package is installed before running: install.packages("quarto")
tryCatch({
  quarto::quarto_render("QC_Report.qmd")
  message(" [\u2713] QC Report generated successfully: QC_Report.html")
}, error = function(e) {
  warning(" [X] Failed to generate QC Report: ", e$message)
})


# ===================================================================
# PACKAGE & ENCRYPT OBSERVED FOLDER
# ===================================================================
message("\n=======================================================")
message(" PACKAGING OBSERVED FOLDER...")
message("=======================================================")

tryCatch({
  # Define paths
  html_source <- "QC_Report.html"
  observed_dir <- "Observed"
  html_target <- file.path(observed_dir, html_source)
  
  final_zip_path <- "Observed.zip" # Target destination for the zip
  password_file <- "secret_pass.txt"              # Location of the text file with the password
  
  # 1. Inject the HTML report into the Observed folder
  if (file.exists(html_source)) {
    if (!dir.exists(observed_dir)) {
      dir.create(observed_dir, recursive = TRUE)
    }
    
    # Copy file, replacing any older version inside the folder
    file.copy(from = html_source, to = html_target, overwrite = TRUE)
    
    # file.remove(html_source) # <-- Commented out: Original stays in root folder!
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
  
}, error = function(e) {
  warning(" [X] Failed during packaging phase: ", e$message)
})