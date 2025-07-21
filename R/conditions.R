#' Apply conditions and sum corresponding coefficients
#'
#' This function evaluates a series of conditions based on the input data
#' and sums the coefficients associated with the true conditions.
#'
#' @param config A list representing the parsed YAML configuration.
#' @param data A data frame or list containing the input data (potentially transformed).
#' @return A numeric value representing the sum of coefficients for which
#'         conditions were met.
#' @keywords internal
#' @importFrom rlang eval_tidy parse_expr
apply_conditions <- function(config, data) {
  if (!is.list(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a list or a data frame.")
  }

  # Ensure data is a data.frame for easier column access if it's a single row list
  if (is.list(data) && !is.data.frame(data)) {
    # This is tricky if data represents a single "row" for prediction.
    # For eval_tidy, it's often easier if data is a list or an environment.
    # If it's multi-row, data.frame is fine.
    # Let's assume for now `data` can be a list for single predictions or a data.frame.
  }

  model_config <- config$plgf_model # Or whichever model is specified/relevant
  if (is.null(model_config)) {
    stop("Model configuration (e.g., 'plgf_model') not found in the config.")
  }

  conditions_list <- model_config$conditions
  if (is.null(conditions_list) || length(conditions_list) == 0) {
    # No conditions to apply
    return(0) # Return 0 if no conditions or no coefficients from conditions
  }

  total_conditional_coefficient <- 0

  # Create an environment or list for evaluation that includes the data
  # rlang::eval_tidy can work with data frames or lists directly as the `data` argument.
  eval_data_env <- as.list(data) # Convert data frame row or list to a list for eval_tidy

  for (cond_item in conditions_list) {
    if (is.null(cond_item$name) || is.null(cond_item$condition) || is.null(cond_item$coefficient)) {
      warning("Skipping condition due to missing 'name', 'condition', or 'coefficient'.")
      next
    }

    condition_met <- FALSE
    tryCatch({
      # Evaluate the condition string
      # Using rlang::eval_tidy for safer evaluation with data masking
      condition_expr <- rlang::parse_expr(cond_item$condition)
      condition_met <- rlang::eval_tidy(condition_expr, data = eval_data_env)

      # Ensure condition_met is a single logical value
      if (length(condition_met) != 1 || !is.logical(condition_met)) {
        warning(paste0("Condition '", cond_item$condition, "' for '", cond_item$name,
                       "' did not evaluate to a single logical value. Treating as FALSE."))
        condition_met <- FALSE
      }
      # Handle NA results from condition evaluation as FALSE
      if (is.na(condition_met)) {
        condition_met <- FALSE
      }

    }, error = function(e) {
      warning(paste0("Error evaluating condition '", cond_item$condition, "' for '", cond_item$name, "': ", e$message))
      condition_met <- FALSE # Treat as false if error
    })

    if (condition_met) {
      # Retrieve the coefficient value
      # The coefficient string like "intercepts.delfia" needs to be resolved within model_config
      path_elements <- strsplit(cond_item$coefficient, "\\.")[[1]]
      current_val <- model_config
      valid_path <- TRUE
      for (el in path_elements) {
        if (is.list(current_val) && el %in% names(current_val)) {
          current_val <- current_val[[el]]
        } else {
          warning(paste0("Coefficient path '", cond_item$coefficient, "' not found in model config for condition '", cond_item$name, "'."))
          valid_path <- FALSE
          break
        }
      }

      if (valid_path && is.numeric(current_val)) {
        total_conditional_coefficient <- total_conditional_coefficient + current_val
      } else if (valid_path) {
        warning(paste0("Coefficient '", cond_item$coefficient, "' for condition '", cond_item$name, "' is not numeric."))
      }
    }
  }

  return(total_conditional_coefficient)
}
