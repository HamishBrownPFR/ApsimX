library(dplyr)
library(rlang)
library(cli)

#' Calculate a new observed variable with optional range validation
#'
#' @param df_obs Dataframe containing the wide observed data.
#' @param col_name_A String. Name of the first column ('A').
#' @param col_name_B String. Name of the second column ('B').
#' @param oprt String. Mathematical operation to perform (e.g., "A/B").
#' @param col_name_Result String. Name of the new column.
#' @param val_range Optional. A numeric vector `c(min, max)` or a string `"min,max"` 
#'                  defining the acceptable boundaries for the calculated results.
#' @return A dataframe with the newly calculated column.
calc_new_obs_var <- function(df_obs, col_name_A, col_name_B, oprt, col_name_Result, val_range = NULL) {
  
  # 1. Check for the presence of essential columns
  essential_cols <- c("SimulationName", "Clock.Today", col_name_A, col_name_B)
  missing_cols <- setdiff(essential_cols, names(df_obs))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing required columns in the dataset.",
      "i" = "Please ensure the following columns exist: {.var {missing_cols}}"
    ))
  }
  
  # 2. Check data formats
  if (!is.character(df_obs$SimulationName) && !is.factor(df_obs$SimulationName)) {
    cli::cli_abort("{.var SimulationName} must be a character or factor.")
  }
  if (!inherits(df_obs$Clock.Today, c("Date", "POSIXt", "character"))) {
    cli::cli_abort("{.var Clock.Today} must be a Date, POSIXt, or character format.")
  }
  if (!is.numeric(df_obs[[col_name_A]])) {
    cli::cli_abort("Column A ({.var {col_name_A}}) must be numeric.")
  }
  if (!is.numeric(df_obs[[col_name_B]])) {
    cli::cli_abort("Column B ({.var {col_name_B}}) must be numeric.")
  }
  
  # 3. Process val_range if provided
  if (!is.null(val_range)) {
    if (is.character(val_range) && length(val_range) == 1) {
      val_range <- as.numeric(trimws(unlist(strsplit(val_range, ","))))
    }
    
    if (!is.numeric(val_range) || length(val_range) != 2 || any(is.na(val_range))) {
      cli::cli_abort("{.arg val_range} must be a numeric vector of length 2, e.g., {.code c(0, 1000)} or a string {.code '0,1000'}.")
    }
    
    min_val <- val_range[1]
    max_val <- val_range[2]
    
    if (min_val >= max_val) {
      cli::cli_abort("In {.arg val_range}, the minimum value ({min_val}) must be strictly less than the maximum ({max_val}).")
    }
  }
  
  # 4. Parse and translate the operation string
  expr_str <- gsub("sqr", "sqrt", oprt)
  expr_str <- gsub("\\bA\\b", paste0("`", col_name_A, "`"), expr_str)
  expr_str <- gsub("\\bB\\b", paste0("`", col_name_B, "`"), expr_str)
  
  # 5. Safely evaluate and mutate the dataframe
  tryCatch({
    parsed_expr <- rlang::parse_expr(expr_str)
    
    df_obs_out <- df_obs %>%
      dplyr::mutate("{col_name_Result}" := !!parsed_expr)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to evaluate the mathematical operation.",
      "i" = "Attempted to parse: {.code {expr_str}}",
      "!" = "Original R error: {e$message}"
    ))
  })
  
  # 6. Perform Range Validation with "Pop Up" Formatting
  if (!is.null(val_range)) {
    calculated_vals <- df_obs_out[[col_name_Result]]
    valid_vals <- calculated_vals[!is.na(calculated_vals)]
    
    if (length(valid_vals) == 0) {
      cli::cli_alert_info("Range check skipped: All calculated values for {.var {col_name_Result}} evaluated to {.val NA}.")
    } else {
      n_below <- sum(valid_vals < min_val)
      n_above <- sum(valid_vals > max_val)
      
      if (n_below == 0 && n_above == 0) {
        # Success Banner
        cat(paste0(
          "\n============================================================\n",
          " \u2705  RANGE CHECK PASSED: ", toupper(col_name_Result), " \u2705 \n",
          "============================================================\n",
          " -> LIMITS       : ", min_val, " to ", max_val, "\n",
          " -> STATUS       : All calculated values are within boundaries.\n",
          "------------------------------------------------------------\n"
        ))
      } else {
        # Warning Banner
        msg <- paste0(
          "\n============================================================\n",
          " \u26A0\uFE0F  RANGE VIOLATION DETECTED: ", toupper(col_name_Result), " \u26A0\uFE0F \n",
          "============================================================\n",
          " -> LIMITS       : ", min_val, " to ", max_val, "\n"
        )
        if (n_below > 0) {
          msg <- paste0(msg, " -> BELOW MIN    : ", n_below, " value(s) fell below ", min_val, "\n")
        }
        if (n_above > 0) {
          msg <- paste0(msg, " -> ABOVE MAX    : ", n_above, " value(s) exceeded ", max_val, "\n")
        }
        msg <- paste0(msg, "------------------------------------------------------------\n")
        
        cat(msg)
      }
    }
  } else {
    # Standard success message if no range was checked
    cli::cli_alert_success("Successfully created column {.var {col_name_Result}}.")
  }
  
  return(df_obs_out)
}