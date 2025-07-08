#' Apply transformations to data based on config
#'
#' This function applies a series of transformations to the input data
#' as defined in the configuration file.
#'
#' @param config A list representing the parsed YAML configuration.
#' @param data A data frame or list containing the input data.
#' @return A data frame or list with the transformed variables added.
#' @keywords internal
apply_transformations <- function(config, data, transformations = NULL) {
  if (!is.list(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a list or a data frame.")
  }

  # Ensure data is a data.frame for easier column assignment
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  model_config <- config$plgf_model # Or whichever model is specified/relevant
  if (is.null(model_config)) {
    stop("Model configuration (e.g., 'plgf_model') not found in the config.")
  }

  transformations <- model_config$transformations
  if (is.null(transformations) || length(transformations) == 0) {
    # No transformations to apply
    return(data)
  }

  # Create an environment for evaluation that includes config$centering and data
  eval_env <- new.env(parent = emptyenv())

  # Add the transformation functions to the environment
  if (!is.null(transformations)) {
    for (name in names(transformations)) {
      assign(name, transformations[[name]], envir = eval_env)
    }
  }

  # Add centering values to the environment
  if (!is.null(config$centering)) {
    for (name in names(config$centering)) {
      assign(name, config$centering[[name]], envir = eval_env)
    }
  }

  # Add data columns to the environment
  # This allows formulas to refer to columns directly
  for (col_name in names(data)) {
    assign(col_name, data[[col_name]], envir = eval_env)
  }

  # Add config itself to the environment, so formulas can refer to config elements
  # e.g. centering.ga_days
  assign("centering", config$centering, envir = eval_env)
  # Potentially add other parts of config if needed by formulas

  for (trans in transformations) {
    if (is.null(trans$name) || is.null(trans$formula)) {
      warning("Skipping transformation due to missing 'name' or 'formula'.")
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
