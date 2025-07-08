#' Apply transformations to data based on model and global configurations
#'
#' This function applies a series of transformations to the input data as
#' defined in a specific model's configuration block within a larger YAML structure.
#'
#' @param model_yaml_config The specific model configuration list (e.g., `config[[model_name]]`)
#'        containing transformation definitions.
#' @param data A data frame or list containing the input data. This data will be modified
#'        by adding new transformed columns.
#' @param transformation_R_functions A named list of R functions available for use
#'        in transformation formulas.
#' @param full_config The full top-level configuration list, used to access
#'        global elements like `centering` values.
#' @return A data frame with the transformed variables added.
#' @keywords internal
apply_transformations <- function(model_yaml_config, data, transformation_R_functions, full_config) {
  if (!is.list(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a list or a data frame.")
  }
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  if (is.null(model_yaml_config)) {
    warning("Model YAML configuration section is NULL. No transformations will be applied.")
    return(data)
  }

  # These are the transformation rules defined in the YAML for the current model
  yaml_defined_transformations <- model_yaml_config$transformations
  if (is.null(yaml_defined_transformations) || length(yaml_defined_transformations) == 0) {
    return(data) # No transformations defined in YAML for this model
  }

  eval_env <- new.env(parent = emptyenv())

  # Add the active transformation R functions (e.g., base, custom) to the environment
  if (!is.null(transformation_R_functions) && length(transformation_R_functions) > 0) {
    for (name in names(transformation_R_functions)) {
      if (is.function(transformation_R_functions[[name]])) {
        assign(name, transformation_R_functions[[name]], envir = eval_env)
      } else {
        warning(paste0("Item '", name, "' in provided transformation_R_functions list is not a function. Skipping."))
      }
    }
  }

  # Add global centering values from the full_config to the environment
  if (!is.null(full_config$centering)) {
    for (name in names(full_config$centering)) {
      assign(name, full_config$centering[[name]], envir = eval_env)
    }
    assign("centering", full_config$centering, envir = eval_env) # Allow access to centering.VAR
  }

  # Add elements from the current model's configuration (e.g., intercepts, coefficients)
  # to the eval_env, so they can be referenced in transformation formulas.
  if (!is.null(model_yaml_config$intercepts)) {
    assign("intercepts", model_yaml_config$intercepts, envir = eval_env)
  }
  if (!is.null(model_yaml_config$coefficients)) {
    assign("coefficients", model_yaml_config$coefficients, envir = eval_env)
  }
  # One could selectively add other named lists from model_yaml_config if needed.

  # Add data columns to the environment for direct access in formulas
  for (col_name in names(data)) {
    assign(col_name, data[[col_name]], envir = eval_env)
  }

  # Iterate through YAML-defined transformations for the current model
  for (trans in yaml_defined_transformations) {
    if (is.null(trans$name) || is.null(trans$formula)) {
      warning("Skipping transformation due to missing 'name' or 'formula' in YAML.")
      next
    }

    # Evaluate the formula in the prepared environment
    # The result of the evaluation is assigned to a new column in 'data'
    tryCatch({
      # Make sure the data columns are accessible within the formula evaluation
      # One way is to use with() or to ensure eval_env has access to 'data' columns
      # For direct access like `biochemical_ga * 7`, they need to be in eval_env
      # For access like `data$biochemical_ga * 7`, 'data' itself needs to be in eval_env

      # Add data columns to the environment for each transformation
      # This ensures that newly created transformed columns are available for subsequent transformations
      for (col_name in names(data)) {
        assign(col_name, data[[col_name]], envir = eval_env)
      }

      result <- eval(parse(text = trans$formula), envir = eval_env)
      data[[trans$name]] <- result
      # Update eval_env with the new column so it can be used in subsequent transformations
      assign(trans$name, result, envir = eval_env)

    }, error = function(e) {
      warning(paste0("Error evaluating transformation '", trans$name, "' with formula '", trans$formula, "': ", e$message))
    })
  }

  return(data)
}
