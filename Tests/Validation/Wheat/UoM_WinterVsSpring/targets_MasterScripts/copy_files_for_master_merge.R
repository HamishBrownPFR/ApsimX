#' Master Merge Preparation: Isolate Core Integration Files
#'
#' @description
#' Scans the base working directory for explicitly defined files, folders, or 
#' wildcards and copies them into a clean, isolated directory for the AppSync 
#' master merge. Leaves all analytical workflows behind.
#'
#' @param base_wd Character. The root directory containing the source files.
#' @param export_dir_name Character. The absolute or relative path to the pristine target folder.
#' @param export_targets Character vector. The exact files, folders, or wildcards to export. 
#'        Defaults to c("*.apsimx", "Met", "Input", "Observed.zip").
#'
#' @return A list detailing the export status of each requested target.
#' @export
copy_files_for_master_merge <- function(base_wd, 
                                        export_dir_name, export_targets = c("*.apsimx", "Met", "Input", "Observed.zip")) {
  
  message("\n=======================================================")
  message(" \U0001F5C2\U0000FE0F EXECUTING PRISTINE MASTER EXPORT \U0001F5C2\U0000FE0F")
  message("=======================================================")
  
  if (!dir.exists(export_dir_name)) {
    dir.create(export_dir_name, recursive = TRUE)
    message(" [*] Created pristine master directory: ", export_dir_name)
  } else {
    message(" [*] Pristine master directory already exists. Updating contents...")
  }
  
  export_status <- list()
  
  for (target in export_targets) {
    # Resolve exact files, folders, or wildcards (e.g., *.apsimx) using Sys.glob
    search_path <- file.path(base_wd, target)
    matched_items <- Sys.glob(search_path)
    
    if (length(matched_items) == 0) {
      export_status[[target]] <- "SKIPPED (Not Found)"
      warning(sprintf(" [X] Target not found: %s", target), call. = FALSE)
      next
    }
    
    copied_count <- 0
    
    tryCatch({
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
      
      if (copied_count > 0) {
        export_status[[target]] <- sprintf("SUCCESS (%d items copied)", copied_count)
      } else {
        export_status[[target]] <- "EMPTY (Matched but no files copied)"
      }
      
    }, error = function(e) {
      warning(sprintf(" [X] Failed exporting %s: %s", target, e$message), call. = FALSE)
      export_status[[target]] <- "FAILED"
    })
  }
  
  message(" [\u2713] Export process completed.")
  return(export_status)
}