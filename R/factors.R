#' Apply factor-based coefficients
#'
#' This function processes the 'factors' block from the configuration.
#' For each defined factor, it finds the corresponding value in the input data,
#' looks up the coefficient for that level, and adds it to a running total.
#'
#' @param config A list representing the parsed YAML configuration.
#' @param data A list containing the input data.
#' @param model_name The name of the model configuration block in the YAML.
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
