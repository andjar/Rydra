#' Validate a Rydra configuration
#'
#' This function checks if the parsed YAML configuration list is valid.
#' It collects all errors and stops if any are found, reporting them all at once.
#'
#' @param config The full configuration list to validate, typically loaded from a YAML file.
#' @param model_name The name of the specific model block within the `config` to validate.
#' @param data A list or data frame representing the input data. Used to validate factor
#'        levels against the data present in this parameter.
#' @return Invisible `TRUE` if validation succeeds, otherwise stops with an error.
#' @keywords internal
validate_config <- function(config, model_name, data) {
  errors <- c()

  # Check 1: Top-level keys
  if (!is.list(config)) {
    stop("Configuration is not a valid list.")
  }

  # Require the requested model block to exist, but do not force 'model_name' or 'centering' at the root
  required_toplevel_keys <- c(model_name)
  for (key in required_toplevel_keys) {
    if (is.null(config[[key]])) {
      errors <- c(errors, paste0("Missing required top-level key (model block): '", key, "'."))
    }
  }

  # Ensure there is at least one model block defined besides reserved keys
  reserved_top_level_names <- c("model_name", "centering", "logging")
  candidate_model_blocks <- setdiff(names(config), reserved_top_level_names)
  if (length(candidate_model_blocks) == 0) {
    errors <- c(errors, "No model blocks found in configuration. Define at least one model block at the YAML root.")
  }
  # Model names must be unique (YAML parsing into a list will deduplicate, but we still assert on the vector just in case)
  if (any(duplicated(candidate_model_blocks))) {
    dupes <- unique(candidate_model_blocks[duplicated(candidate_model_blocks)])
    errors <- c(errors, paste0("Duplicate model names found at the top level: ", paste(dupes, collapse = ", "), "."))
  }

  # If model block is missing, we can't check its contents, so stop early.
  if (length(errors) > 0) {
    stop("Configuration validation failed:\n- ", paste(errors, collapse = "\n- "))
  }

  model_config <- config[[model_name]]

  # Check 2: Model block keys
  # 'factors' is optional; 'centering' is optional at top-level
  required_model_keys <- c("intercepts", "coefficients", "transformations")
  for (key in required_model_keys) {
    if (is.null(model_config[[key]])) {
      errors <- c(errors, paste0("Missing required key in '", model_name, "' block: '", key, "'."))
    }
  }

  # Check 3: Structure and types of model block elements
  if (!is.null(model_config$intercepts)) {
    if (!is.list(model_config$intercepts) || is.null(model_config$intercepts$baseline) || !is.numeric(model_config$intercepts$baseline)) {
      errors <- c(errors, "'intercepts' must be a list containing a numeric 'baseline' value.")
    }
  }

  if (!is.null(model_config$coefficients) && !is.list(model_config$coefficients)) {
    errors <- c(errors, "'coefficients' must be a list.")
  }

  if (!is.null(model_config$transformations)) {
    if (!is.list(model_config$transformations)) {
      errors <- c(errors, "'transformations' must be a list.")
    } else {
      lapply(seq_along(model_config$transformations), function(i) {
        trans <- model_config$transformations[[i]]
        if (!is.list(trans) || is.null(trans$name) || is.null(trans$formula)) {
          errors <<- c(errors, paste0("Transformation item #", i, " is invalid. It must be a list with 'name' and 'formula' keys."))
        }
      })
    }
  }

  if (!is.null(model_config$conditions)) {
    if (!is.list(model_config$conditions)) {
      errors <- c(errors, "'conditions' must be a list.")
    } else {
      lapply(seq_along(model_config$conditions), function(i) {
        cond <- model_config$conditions[[i]]
        if (!is.list(cond) || is.null(cond$name) || is.null(cond$condition) || is.null(cond$coefficient)) {
          errors <<- c(errors, paste0("Condition item #", i, " is invalid. It must be a list with 'name', 'condition', and 'coefficient' keys."))
        }
      })
    }
  }

  if (!is.null(model_config$factors)) {
    if (!is.list(model_config$factors)) {
      errors <- c(errors, "'factors' must be a list.")
    } else {
      lapply(seq_along(model_config$factors), function(i) {
        factor <- model_config$factors[[i]]
        if (!is.list(factor) || is.null(factor$name) || is.null(factor$levels)) {
          errors <<- c(errors, paste0("Factor item #", i, " is invalid. It must be a list with 'name' and 'levels' keys."))
        } else {
          if (!is.list(factor$levels)) {
            errors <<- c(errors, paste0("Factor ", factor$name, " levels must be a list."))
          } else {
            level_values <- c()
            lapply(seq_along(factor$levels), function(j) {
              level <- factor$levels[[j]]
              if (!is.list(level) || is.null(level$value) || is.null(level$coefficient)) {
                errors <<- c(errors, paste0("Factor ", factor$name, " level #", j, " is invalid. It must be a list with 'value' and 'coefficient' keys."))
              } else {
                level_values <<- c(level_values, level$value)
              }
            })
            if (length(level_values) != length(unique(level_values))) {
              errors <<- c(errors, paste0("Factor ", factor$name, " has duplicate level values."))
            }
          }
        }
      })
    }
  }

  if (!is.null(model_config$output_transformation) && !is.character(model_config$output_transformation)) {
    errors <- c(errors, "'output_transformation' must be a string.")
  }

  # Check 4: Centering block
  if (!is.null(config$centering) && !is.list(config$centering)) {
    errors <- c(errors, "'centering' must be a list.")
  }

  # Check 5: Data factors against config factors
  if (!is.null(model_config$factors)) {
    for (factor_def in model_config$factors) {
      factor_name <- factor_def$name
      if (is.null(data[[factor_name]])) {
        errors <- c(errors, paste0("Input data is missing factor: '", factor_name, "'."))
      } else {
        data_value <- data[[factor_name]]
        valid_level_found <- FALSE
        for (level_def in factor_def$levels) {
          if (level_def$value == data_value) {
            valid_level_found <- TRUE
            break
          }
        }
        if (!valid_level_found) {
          errors <- c(errors, paste0("Value '", data_value, "' for factor '", factor_name, "' in input data is not a defined level in the configuration."))
        }
      }
    }
  }

  # Final error reporting
  if (length(errors) > 0) {
    stop("Configuration validation failed:\n- ", paste(errors, collapse = "\n- "))
  }

  invisible(TRUE)
}
