#' Apply factor-based coefficients
#'
#' This function processes the 'factors' block from the configuration.
#' For each defined factor, it finds the corresponding value in the input data,
#' looks up the coefficient for that level, and adds it to a running total.
#'
#' @param config The full configuration list, typically loaded from a YAML file.
#' @param data A list containing the input data, typically after transformations.
#' @param model_name The name of the specific model configuration block to be used from the `config`.
#' @return A numeric value representing the sum of coefficients from all factors.
#' @keywords internal
apply_factors <- function(config, data, model_name) {
  model_config <- config[[model_name]]
  factors_list <- model_config$factors
  if (is.null(factors_list) || length(factors_list) == 0) {
    return(0)
  }

  total_factor_coefficient <- 0

  for (factor_definition in factors_list) {
    factor_name <- factor_definition$name
    data_value <- data[[factor_name]]

    # Find the matching level in the config
    matched_level <- NULL
    for (level in factor_definition$levels) {
      if (level$value == data_value) {
        matched_level <- level
        break
      }
    }

    if (!is.null(matched_level)) {
      # Resolve the coefficient path (e.g., "coefficients.unemployed_modifier")
      path_elements <- strsplit(matched_level$coefficient, "\\.")[[1]]
      current_val <- model_config
      valid_path <- TRUE
      for (el in path_elements) {
        if (is.list(current_val) && el %in% names(current_val)) {
          current_val <- current_val[[el]]
        } else {
          valid_path <- FALSE
          break
        }
      }

      if (valid_path && is.numeric(current_val)) {
        total_factor_coefficient <- total_factor_coefficient + current_val
      } else {
        # This case should ideally be caught by the validator
        warning(paste0("Coefficient for factor '", factor_name, "' level '", data_value, "' is invalid or not found."))
      }
    }
    # If no level matches, the validator should have already caught it.
  }

  return(total_factor_coefficient)
}

#' Apply conditional coefficients
#'
#' This function processes the 'conditions' block from the configuration.
#' For each defined condition, it evaluates the boolean expression in the context
#' of the input data. If true, it resolves the specified coefficient path within
#' the model's configuration and adds it to a running total.
#'
#' @param model_config The specific model configuration list (e.g., `config[[model_name]]`)
#'        containing condition definitions.
#' @param data A list containing the input data, typically after transformations.
#'        Condition formulas are evaluated against this data.
#' @return A numeric value representing the sum of coefficients from all met conditions.
#' @keywords internal
apply_conditions <- function(model_config, data) {
  if (is.null(model_config) || is.null(model_config$conditions) || length(model_config$conditions) == 0) {
    return(0)
  }

  conditions_list <- model_config$conditions
  total_conditional_coefficient <- 0

  # Create an environment for evaluating condition expressions.
  # This environment should have access to the 'data' list's elements.
  eval_env <- new.env(parent = emptyenv())
  for (name in names(data)) {
    assign(name, data[[name]], envir = eval_env)
  }
  # Also add model_config elements like intercepts, coefficients to the environment
  # so conditions can reference them if needed, e.g. condition = "age > intercepts.age_threshold"
  # This is similar to how apply_transformations sets up its environment.
  if (!is.null(model_config$intercepts)) {
    assign("intercepts", model_config$intercepts, envir = eval_env)
  }
  if (!is.null(model_config$coefficients)) {
    assign("coefficients", model_config$coefficients, envir = eval_env)
  }
  # Potentially add other named lists from model_config if they are commonly referenced by conditions.


  for (i in seq_along(conditions_list)) {
    condition_item <- conditions_list[[i]]

    if (!is.list(condition_item) || is.null(condition_item$name) ||
        is.null(condition_item$condition) || is.null(condition_item$coefficient)) {
      # This validation should ideally be caught by validate_config earlier.
      # However, good to have a check here too.
      warning(paste0("Condition item #", i, " in YAML is invalid. ",
                     "It must be a list with 'name', 'condition', and 'coefficient' keys. Skipping."))
      next
    }

    condition_evaluates_to_true <- FALSE
    tryCatch({
      result <- eval(parse(text = condition_item$condition), envir = eval_env)
      if (is.logical(result) && length(result) == 1 && !is.na(result) && result) {
        condition_evaluates_to_true <- TRUE
      }
    }, error = function(e) {
      warning(paste0("Error evaluating condition '", condition_item$name,
                     "' (", condition_item$condition, "): ", e$message))
    })

    if (condition_evaluates_to_true) {
      # Resolve the coefficient path (e.g., "coefficients.some_value" or "intercepts.another_value")
      # The path is resolved relative to the model_config block.
      path_elements <- strsplit(condition_item$coefficient, "\\.")[[1]]
      current_val <- model_config # Start searching from the model_config block
      valid_path <- TRUE
      for (el in path_elements) {
        if (is.list(current_val) && el %in% names(current_val)) {
          current_val <- current_val[[el]]
        } else {
          valid_path <- FALSE
          break
        }
      }

      if (valid_path && is.numeric(current_val)) {
        total_conditional_coefficient <- total_conditional_coefficient + current_val
      } else {
        warning(paste0("Coefficient path '", condition_item$coefficient,
                       "' for met condition '", condition_item$name,
                       "' is invalid or does not resolve to a numeric value in the model config."))
      }
    }
  }
  return(total_conditional_coefficient)
}
