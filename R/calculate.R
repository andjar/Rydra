#' Calculate result based on Rydra configuration and input data
#'
#' This is the main function to perform calculations using a Rydra setup.
#' It loads the configuration, applies transformations, evaluates conditions,
#' computes a model score, and applies an output transformation.
#'
#' @param config_path Path to the YAML configuration file.
#' @param data A list or a single-row data frame representing the input data
#'        for which the calculation is to be performed.
#' @param model_name The name of the model configuration block in the YAML
#'        (e.g., "plgf_model"). Defaults to "plgf_model".
#' @return The final calculated result after applying all steps.
#' @param transformations A named list of custom transformation functions.
#'        Defaults to a list of basic transformations: `center_variable`,
#'        `square_variable`, `log_transform`, `exp_transform`. If you provide
#'        your own list, the default base transformations will not be included
#'        unless you explicitly add them. Provide `list()` to exclude all defaults
#'        and use only what's in the YAML if those functions are globally available.
#' @export
#' @importFrom jsonlite write_json
#' @importFrom uuid UUIDgenerate
#' @examples
#' \dontrun{
#'   # Assuming config.yaml and input_data are defined
#'   # result <- rydra_calculate("path/to/config.yaml", input_data)
#'
#'   # To use only custom transformations (and exclude base ones):
#'   # my_custom_funcs <- list(my_func = function(x) x * 10)
#'   # result <- rydra_calculate("path/to/config.yaml", input_data,
#'   #                           transformations = my_custom_funcs)
#'
#'   # To exclude even base transformations (if functions in YAML are globally defined):
#'   # result <- rydra_calculate("path/to/config.yaml", input_data,
#'   #                           transformations = list())
#' }
#'
# Define the list of base transformation functions
# These functions must be exported from the package or otherwise available.
# Assuming they are exported (which they are, from R/basic_transformations.R)
.default_rydra_transformations <- list(
  center_variable = Rydra::center_variable,
  square_variable = Rydra::square_variable,
  log_transform = Rydra::log_transform,
  exp_transform = Rydra::exp_transform
)

rydra_calculate <- function(config_path, data, model_name = "plgf_model", transformations = .default_rydra_transformations) {
  # 1. Load and validate configuration
  config <- load_config(config_path)
  validate_config(config, model_name, data) # Assuming data is the original input here for validation

  # Get logging configuration
  log_settings <- get_logging_config(config)

  # Capture original data for logging before potential modification
  original_data_for_log <- data

  if (is.null(config[[model_name]])) {
    stop(paste0("Model configuration '", model_name, "' not found in the YAML file."))
  }
  model_config <- config[[model_name]]

  # Ensure data is a list or a single-row data frame, as this function
  # is designed for single-instance calculation based on the example.
  # For batch processing, this function or its callers would need modification.
  if (is.data.frame(data) && nrow(data) > 1) {
    warning("Input 'data' has multiple rows. This function processes one instance at a time. Using the first row.")
    data <- data[1, , drop = FALSE]
  }
  # Convert to list to ensure consistent handling in helper functions
  # and for eval_tidy/env construction.
  if (is.data.frame(data)) {
    data_list <- as.list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("'data' must be a list or a data.frame.")
  }

  # 2. Apply transformations
  # Pass the specific model_config for transformation rules,
  # the full config for global elements like 'centering',
  # data, and the list of available transformation R functions.
  transformed_data_df <- apply_transformations(
    model_yaml_config = model_config,
    data = as.data.frame(data_list),
    transformation_R_functions = transformations,
    full_config = config
  )
  transformed_data_list <- as.list(transformed_data_df)

  # 3. Calculate sum of coefficients from factors
  factor_coeffs_sum <- apply_factors(config = config, data = transformed_data_list, model_name = model_name)

  # 4. Calculate the "base" model prediction
  # Sum of: intercepts.baseline + sum(coefficients * data_values)

  # Initialize base_score with baseline intercept
  base_score <- 0
  if (!is.null(model_config$intercepts$baseline) && is.numeric(model_config$intercepts$baseline)) {
    base_score <- model_config$intercepts$baseline
  } else {
    warning("Baseline intercept (intercepts.baseline) not found or not numeric in model config. Defaulting to 0.")
  }

  # Add sum of (coefficients * data_values)
  # These coefficients directly map to variables in the (transformed) data
  if (!is.null(model_config$coefficients) && is.list(model_config$coefficients)) {
    for (coeff_name in names(model_config$coefficients)) {
      coeff_value <- model_config$coefficients[[coeff_name]]

      # The variable name in data should match the coefficient name
      # e.g., coefficient "ga" maps to data variable "ga_centered" (after transformation)
      # The YAML definition implies that keys under `coefficients:` are terms in the linear model
      # whose values are taken from the (transformed) data.
      # Example: `ga: 0.0122` means `0.0122 * data$ga_centered` (if 'ga' is mapped to 'ga_centered')
      # The current YAML structure is a bit ambiguous here.
      # Let's assume coefficient names directly match variable names in `transformed_data_list`
      # OR, if not found, they match original data columns for simplicity before transformations.
      # A more robust solution might involve explicit mapping in the YAML if names differ.

      # The variable name in the data that corresponds to this coefficient.
      # Based on the YAML example:
      # `coefficients: { ga: ..., weight: ... }`
      # `transformations: [{ name: "ga_centered", formula: "...ga..."}]`
      # The model term `ga` in `coefficients` should use `ga_centered` from transformed data.
      # This requires a mapping or a convention.
      # Convention: if a transformation creates `X_centered` or `X_squared_centered` for `X`,
      # the coefficient named `X` or `X_squared` should use that.

      # Let's try to match coefficient name to transformed data:
      # 'ga' in coefficients should use 'ga_centered' from data
      # 'ga_squared' in coefficients should use 'ga_squared_centered'
      # This implies a naming convention or an explicit mapping in the config.
      # For now, let's assume the coefficient name *is* the variable name in transformed_data_list.
      # E.g. if config has `coefficients: { ga_centered: 0.01, ...}`.
      # Your example has `coefficients: { ga: 0.012263018, ... }` and
      # `transformations: { name: "ga_centered", formula: "..." }`
      # This means `ga` in coefficients should use `ga_centered` from data.
      # This is the trickiest part to generalize from the example.

      # Let's make a simplifying assumption for now: the keys in `model_config$coefficients`
      # are the names of the variables that should be used from `transformed_data_list`.
      # E.g., if you have `coefficients: { my_var: 0.5 }`, it will look for `transformed_data_list$my_var`.
      # This means transformed variables must match these keys.
      # The example YAML `plgf_model.coefficients` has `ga`, `ga_squared`, `weight`, `weight_squared`, `age`.
      # The `transformations` create `ga_centered`, `ga_squared_centered`, etc.
      # The model needs to use these _transformed_ values.
      # So, the `coefficients` section should actually be:
      # coefficients: { ga_centered: ..., ga_squared_centered: ..., ... }
      # Or we need a mapping.

      # For now, let's iterate through coefficients and expect them to be in transformed_data_list
      # This might require adjusting the example YAML's coefficient names.
      # Let's assume the names in `coefficients` are the *final* variable names to be used.
      data_value_for_coeff <- transformed_data_list[[coeff_name]]

      if (!is.null(data_value_for_coeff) && is.numeric(data_value_for_coeff) && is.numeric(coeff_value)) {
        if(length(data_value_for_coeff) > 1) {
          warning(paste0("Data value for coefficient '", coeff_name, "' has length > 1. Using the first element."))
          data_value_for_coeff <- data_value_for_coeff[1]
        }
        base_score <- base_score + (coeff_value * data_value_for_coeff)
      } else {
        # This warning will trigger for coefficients like 'afro_caribbean', 'smoking', etc.
        # if they are not present as variables in `transformed_data_list`.
        # These are handled by the `conditions` logic.
        # So, we should only iterate over coefficients that are meant for direct multiplication.
        # The example YAML mixes these. `coefficients` has both direct model terms (ga, weight)
        # AND coefficients that are applied conditionally (afro_caribbean, smoking).
        # This design needs clarification.
        # Assumption: `coefficients` are for continuous variables or pre-calculated factors.
        # `conditions` handle indicator variables.
        # The provided YAML puts all of these under `plgf_model.coefficients`.
        # Some of these (e.g. afro_caribbean) are then *also* referenced by `conditions`.
        # This suggests `plgf_model.coefficients` is a store of *all* potential coefficients,
        # and `conditions` section decides how some of them are applied.
        # The "base" score should only use coefficients for variables that are directly multiplied.

        # Let's refine: iterate `transformed_data_list`. If a variable name matches a coefficient name, multiply.
        # This is still not quite right.
        # The linear model is: intercept + B1*X1 + B2*X2 + ... + conditional_coeffs_sum
        # The `coefficients` block in YAML seems to define the B_i values.
        # The X_i values are the transformed variables.
        # The names in `coefficients` (ga, ga_squared) must map to the transformed data names (ga_centered, ga_squared_centered).

        # Let's assume the YAML structure needs to be precise:
        # coefficients:
        #   ga_centered: 0.012263018  (transformed name)
        #   ga_squared_centered: 0.000149743 (transformed name)
        #   weight_centered: -0.001682761 (transformed name)
        #   ...
        # If this assumption holds:
        if (!is.null(transformed_data_list[[coeff_name]])) { # Check if this coefficient name is a variable in transformed data
          if (is.numeric(coeff_value) && is.numeric(transformed_data_list[[coeff_name]])) {
            if(length(transformed_data_list[[coeff_name]]) > 1) {
              warning(paste0("Data value for '", coeff_name, "' has length > 1. Using the first element."))
              transformed_data_list[[coeff_name]] <- transformed_data_list[[coeff_name]][1]
            }
            base_score <- base_score + (coeff_value * transformed_data_list[[coeff_name]])
          } else {
            # This coefficient is not for a direct term, or data is missing/non-numeric
            # warning(paste0("Coefficient '", coeff_name, "' found, but corresponding data is missing, non-numeric, or coefficient itself is non-numeric. Skipping this term for base score."))
          }
        }
        # Coefficients like 'afro_caribbean' are handled by the conditions logic, so they won't be found in transformed_data_list
        # unless they were also created as columns, which is not the case here.
      }
    }
  }

  # 5. Calculate sum of conditional coefficients
  #    Pass model_config (which is config[[model_name]]) and transformed_data_list
  conditional_coeffs_sum <- apply_conditions(model_config = model_config, data = transformed_data_list)

  # Sum all components for the total score before output transformation
  total_score <- base_score + factor_coeffs_sum + conditional_coeffs_sum

  # 6. Apply output transformation
  final_result <- total_score
  if (!is.null(model_config$output_transformation) && nzchar(model_config$output_transformation)) {
    # The output transformation formula needs access to 'result' (total_score)
    # and potentially other config values.
    eval_env_output <- new.env(parent = emptyenv())
    assign("result", total_score, envir = eval_env_output)
    # Add config elements if needed by output_transformation, e.g. config$constants$X

    tryCatch({
      final_result <- eval(parse(text = model_config$output_transformation), envir = eval_env_output)
    }, error = function(e) {
      stop(paste0("Error evaluating output transformation '", model_config$output_transformation, "': ", e$message))
    })
  }

  # 7. Log calculation details if enabled
  if (log_settings$enabled) {
    tryCatch({
      if (!dir.exists(log_settings$path)) {
        dir.create(log_settings$path, recursive = TRUE, showWarnings = FALSE)
      }

      # Sanitize input data for logging: data frames can be complex for JSON.
      # Convert to list if it's a data.frame. Ensure it's serializable.
      input_data_log <- original_data_for_log # Use the captured original data
      if (is.data.frame(input_data_log)) {
        input_data_log <- as.list(input_data_log)
      }
      # Ensure all elements in the list are simple enough for JSON
      # This is a basic attempt; complex objects might need more handling.
      input_data_log <- lapply(input_data_log, function(x) {
        if (is.factor(x)) as.character(x) else x
      })


      log_data <- list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z"),
        invocation_params = list(
          config_path = config_path,
          model_name = model_name,
          # Use the original_data_for_log that was captured before modifications
          data = input_data_log
        ),
        model_config_used = model_config, # This is config[[model_name]]
        intermediate_values = list(
          # data_list is the input data after initial processing (e.g. first row taken, to list)
          # transformed_data_list is after transformations
          input_data_processed_for_calc = data_list,
          transformed_data = transformed_data_list,
          factor_coeffs_sum = factor_coeffs_sum,
          base_score = base_score,
          conditional_coeffs_sum = conditional_coeffs_sum,
          total_score_pre_output_transform = total_score
        ),
        final_result = final_result
      )

      # Generate a sortable, unique filename
      # Using timestamp and a v4 UUID for uniqueness.
      # For sortability, timestamp prefix is key.
      unique_id_part <- uuid::UUIDgenerate(output = "string")
      filename <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "_", unique_id_part, ".json")
      filepath <- file.path(log_settings$path, filename)

      jsonlite::write_json(log_data, filepath, auto_unbox = TRUE, pretty = TRUE)

    }, error = function(e) {
      warning(paste0("Rydra logging failed: ", e$message))
    })
  }

  return(final_result)
}
