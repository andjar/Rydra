#' Calculate result based on Rydra configuration and input data
#'
#' This is the main function to perform calculations using a Rydra setup.
#' It loads the configuration, applies transformations, evaluates conditions,
#' computes a model score, and applies an output transformation.
#'
#' @param config_path Path to the YAML configuration file.
#' @param data A list or a single-row data frame representing the input data
#'        for which the calculation is to be performed.
#' @param model_name The name of the model configuration block in the YAML. If NULL,
#'        the function requires 'model_name' to be specified at the YAML root; otherwise it errors.
#' @return The final calculated result after applying all steps.
#' @param transformations A named list of custom transformation functions.
#'        Defaults to internal list including `center_variable`, `square_variable`,
#'        `log_transform`, `exp_transform`, `multiply_by`, `add_value`, and `truncate_variable`.
#'        Provide your own list to override defaults.
#'        Provide `list()` to use only functions globally available or defined in YAML.
#' @param constants_key Root key name for global constants injection. Default "constants".
#'        Legacy configs using `centering` are supported automatically.
#' @export
#' @importFrom jsonlite write_json
#' @importFrom uuid UUIDgenerate
#' @examples
#' \dontrun{
#' # Basic example using the package's built-in example configuration
#' config_path <- system.file("extdata", "example_config.yaml", package = "Rydra")
#'
#' # Fallback for development when package is not installed
#' if (config_path == "" && file.exists("inst/extdata/example_config.yaml")) {
#'   config_path <- "inst/extdata/example_config.yaml"
#' }
#'
#' if (file.exists(config_path)) {
#'   input_data <- list(
#'     biochemical_ga    = 12, # weeks
#'     weight            = 70, # kg
#'     age               = 30, # years
#'     plgf_machine      = 1,  # Corresponds to 'Delfia' in example config
#'     race              = 1,  # e.g., Caucasian/Other
#'     smoking           = 0,  # 0 for No, 1 for Yes
#'     diabetes_type_i   = 0,  # 0 for No, 1 for Yes
#'     diabetes_type_ii  = 0,  # 0 for No, 1 for Yes
#'     conception        = 1,  # e.g., 1 for Spontaneous, 3 for IVF
#'     previous          = 0   # e.g., 0 for Nulliparous
#'   )
#'   result <- rydra_calculate(config_path = config_path, data = input_data)
#'   print(paste("Calculated result:", result))
#'
#'   # Example with custom transformations (overriding defaults)
#'   # This example defines a new transformation and uses only that one.
#'   # Note: The example_config.yaml might not use 'custom_doubler'.
#'   # This is for illustration of the 'transformations' parameter.
#'   my_transforms <- list(custom_doubler = function(x) x * 2)
#'   # If your config expects 'ga_centered', this example might need adjustment
#'   # or a config that uses 'custom_doubler(some_variable)'.
#'   # For this to run meaningfully, you'd typically align custom functions
#'   # with what your specific YAML configuration expects.
#'
#'   # Example using NO base transformations, relying only on globally defined ones
#'   # (if your YAML refers to functions like 'log' directly and they are available)
#'   # result_no_base <- rydra_calculate(config_path = config_path,
#'   #                                   data = input_data,
#'   #                                   transformations = list())
#'   # print(paste("Calculated result (no base transformations):", result_no_base))
#'
#' } else {
#'   print("Could not find example_config.yaml for rydra_calculate examples.")
#' }
#' }
#'
# Define the list of base transformation functions
# These functions must be exported from the package or otherwise available.
# Assuming they are exported (which they are, from R/basic_transformations.R)
.default_rydra_transformations <- list(
  center_variable = center_variable,
  square_variable = square_variable,
  log_transform = log_transform,
  exp_transform = exp_transform,
  multiply_by = multiply_by,
  add_value = add_value,
  subtract_value = subtract_value,
  divide_by = divide_by,
  truncate_variable = truncate_variable
)

rydra_calculate <- function(config_path, data, model_name = NULL, transformations = .default_rydra_transformations, constants_key = "constants") {
  # 1. Load and validate configuration
  config <- load_config(config_path)
  # Determine model_name from argument or YAML
  if (is.null(model_name) || !nzchar(as.character(model_name))) {
    if (!is.null(config$model_name) && is.character(config$model_name) && length(config$model_name) == 1 && nzchar(config$model_name)) {
      model_name <- config$model_name
    } else {
      stop("No model specified. Provide 'model_name' argument or set 'model_name' at the YAML root.")
    }
  }

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
    full_config = config,
    constants_key = constants_key
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
    output_transform_str <- model_config$output_transformation

    # Enforce single function call only (no trailing tokens) and presence of 'result' argument
    single_call_pattern <- "^(?s)\\s*([a-zA-Z_][A-Za-z0-9_.]*)\\s*\\(.*\\)\\s*$"
    if (!grepl(single_call_pattern, output_transform_str, perl = TRUE)) {
      stop(paste0("Output transformation '", output_transform_str, "' must be a single function call like 'fn(result, ...)'."))
    }
    if (!grepl("\\bresult\\b", output_transform_str)) {
      stop(paste0("Output transformation '", output_transform_str, "' must include 'result' as an argument."))
    }

    # Attempt to parse the function name from the string.
    # This regex extracts the function name before the first parenthesis.
    parsed_function_name_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_.]*)\\s*\\(", output_transform_str)

    if (parsed_function_name_match == -1) {
      stop(paste0("Output transformation '", output_transform_str, "' is not a valid function call format. Expected format like 'function_name(arguments)'."))
    }

    parsed_function_name <- sub("^\\s*([a-zA-Z_][a-zA-Z0-9_.]*)\\s*\\(.*$", "\\1", output_transform_str)

    # Check if the parsed function name is in the list of available transformations
    if (!(parsed_function_name %in% names(transformations))) {
      stop(paste0(
        "Output transformation function '", parsed_function_name, "' from string '", output_transform_str,
        "' is not found in the available transformations list. " ,
        "Add it to the 'transformations' argument (e.g., transformations = list(", parsed_function_name,
        " = ", parsed_function_name, ")) or use a supported function."))
    }

    # The output transformation formula needs access to 'result' (total_score),
    # transformed variables (e.g., those created in `apply_transformations`),
    # and the allowed transformation functions.
    eval_env_output <- new.env(parent = baseenv())
    assign("result", total_score, envir = eval_env_output)

    # Expose transformed variables so output transformations can reference them directly
    if (!is.null(transformed_data_list) && length(transformed_data_list) > 0) {
      for (name in names(transformed_data_list)) {
        assign(name, transformed_data_list[[name]], envir = eval_env_output)
      }
    }

    # Also expose model configuration lists for convenience (e.g., coefficients.discount_percentage)
    if (!is.null(model_config$intercepts)) {
      assign("intercepts", model_config$intercepts, envir = eval_env_output)
      for (n in names(model_config$intercepts)) {
        assign(paste0("intercepts.", n), model_config$intercepts[[n]], envir = eval_env_output)
      }
    }
    if (!is.null(model_config$coefficients)) {
      assign("coefficients", model_config$coefficients, envir = eval_env_output)
      for (n in names(model_config$coefficients)) {
        assign(paste0("coefficients.", n), model_config$coefficients[[n]], envir = eval_env_output)
      }
    }

    # Add available transformation functions to the evaluation environment
    # This makes functions like multiply_by, log_transform etc. directly callable.
    if (!is.null(transformations) && length(transformations) > 0) {
      for (name in names(transformations)) {
        if (is.function(transformations[[name]])) {
          assign(name, transformations[[name]], envir = eval_env_output)
        }
      }
    }
    # Ensure common operators are available in the evaluation environment
    # to avoid issues with unary/binary operators being treated as functions
    assign("-", base::`-`, envir = eval_env_output)
    assign("+", base::`+`, envir = eval_env_output)
    assign("*", base::`*`, envir = eval_env_output)
    assign("/", base::`/`, envir = eval_env_output)
    assign("^", base::`^`, envir = eval_env_output)
    assign(">", base::`>`, envir = eval_env_output)
    assign("<", base::`<`, envir = eval_env_output)
    assign(">=", base::`>=`, envir = eval_env_output)
    assign("<=", base::`<=`, envir = eval_env_output)
    assign("==", base::`==`, envir = eval_env_output)
    assign("!=", base::`!=`, envir = eval_env_output)
    # Add global constants values from the full_config to the environment, if any
    constants_list <- NULL
    if (!is.null(constants_key) && nzchar(constants_key) && !is.null(config[[constants_key]])) {
      constants_list <- config[[constants_key]]
    } else if (!is.null(config$centering)) { # legacy
      constants_list <- config$centering
    }
    if (!is.null(constants_list)) {
      for (name in names(constants_list)) {
        assign(name, constants_list[[name]], envir = eval_env_output)
        assign(paste0("constants.", name), constants_list[[name]], envir = eval_env_output)
        assign(paste0("centering.", name), constants_list[[name]], envir = eval_env_output)
      }
      assign("constants", constants_list, envir = eval_env_output)
      assign("centering", constants_list, envir = eval_env_output)
    }


    tryCatch({
      final_result <- eval(parse(text = output_transform_str), envir = eval_env_output)
    }, error = function(e) {
      # Check if the error is due to the function itself (e.g. wrong number of args)
      # or something else like a missing variable *within* the arguments.
      detailed_error_message <- e$message
      # Check if the error message already contains the output_transform_str to avoid redundancy
      if (!grepl(output_transform_str, detailed_error_message, fixed = TRUE)) {
         detailed_error_message <- paste0("in '", output_transform_str, "': ", detailed_error_message)
      }
      stop(paste0("Error evaluating output transformation ", detailed_error_message))
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


      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
      log_data <- list(
        timestamp = format(now_utc, "%Y-%m-%dT%H:%M:%OS6Z"),
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
      ts_secs <- format(now_utc, "%Y%m%d%H%M%S")
      filename <- paste0(ts_secs, "000", "_", unique_id_part, ".json")
      filepath <- file.path(log_settings$path, filename)

      jsonlite::write_json(log_data, filepath, auto_unbox = TRUE, pretty = TRUE)

    }, error = function(e) {
      warning(paste0("Rydra logging failed: ", e$message))
    })
  }

  return(final_result)
}
