test_that("Transformation Defaults and Overrides", {
  library(testthat)
  library(Rydra)

  # Config path for these tests
  config_path_simple_transform <- system.file("extdata", "test_config_simple_transform.yaml", package = "Rydra")
  if (config_path_simple_transform == "") { # Fallback for dev environment
      config_path_simple_transform <- "inst/extdata/test_config_simple_transform.yaml"
  }

  test_that("Base transformations are used by default", {
    # test_config_simple_transform.yaml uses center_variable and log_transform
    input_data <- list(val_a = 15, val_b = exp(2)) # centering.val_a = 10

    # val_a_centered = 15 - 10 = 5
    # val_b_logged = log(exp(2)) = 2
    # result = baseline(0) + 1*5 + 2*2 = 9
    expected_value <- 9

    # Call rydra_calculate WITHOUT providing the 'transformations' argument
    actual_value <- suppressWarnings(rydra_calculate(
      config_path = config_path_simple_transform,
      data = input_data,
      model_name = "transform_test_model"
    ))
    expect_equal(actual_value, expected_value, tolerance = 1e-7)
  })

  test_that("Providing transformations = list() excludes defaults", {
    input_data <- list(val_a = 15, val_b = exp(2))

    # Expect a warning because center_variable and log_transform won't be found
    # The transformation will likely result in NA or an error during eval,
    # leading to issues in the sum.
    # The functions `center_variable` etc. are not in the empty list, so eval will fail.
    # `eval(parse(text = trans$formula), envir = eval_env)` will error.
    expect_error(
      rydra_calculate(
        config_path = config_path_simple_transform,
        data = input_data,
        model_name = "transform_test_model",
        transformations = list() # Provide empty list; strict mode errors on missing output transform func
      ),
      regexp = "not found in the available transformations list"
    )

    # A more specific error or NA propagation test might be better,
    # but for now, checking for the warning from eval is a good start.
    # The result might be NA or an error depending on how errors in eval are handled.
    # If eval error stops execution, then expect_error.
    # Current apply_transformations has a tryCatch that issues a warning.
    # So, the calculation will proceed but with NAs for failed transforms if they are not caught earlier.
    # If val_a_centered becomes NA, then result becomes NA.
    # If coefficients are not found, they are skipped. If data values are NA, NA * coeff = NA.
    # Baseline is 0. So result is likely NA.
    # In strict mode, calling again also errors; no further value assertions are applicable here.
  })

  test_that("Custom transformation overrides default base function with the same name", {
    input_data <- list(val_a = 15, val_b = 100) # centering.val_a = 10

    # Custom log_transform that returns log10(x)
    custom_log_transform <- function(x) { log10(x) }

    # All other base functions should still be available from default partial list
    # if we construct the list carefully.
    # However, the current setup is: user list REPLACES default list.
    # So, we must provide all needed functions if we override one.
    # The test_config_simple_transform also uses center_variable.

    custom_transformations = list(
      log_transform = custom_log_transform,
      center_variable = Rydra::center_variable # Need to re-include other used base functions
      # square_variable = Rydra::square_variable, # Not used by this config
      # exp_transform = Rydra::exp_transform    # Not used by this config
    )

    # val_a_centered = 15 - 10 = 5 (using Rydra::center_variable)
    # val_b_logged = log10(100) = 2 (using custom_log_transform)
    # result = baseline(0) + 1*5 + 2*2 = 9
    expected_value <- 9

    actual_value <- suppressWarnings(rydra_calculate(
      config_path = config_path_simple_transform,
      data = input_data,
      model_name = "transform_test_model",
      transformations = c(custom_transformations, list(add_value = Rydra::add_value))
    ))
    expect_equal(actual_value, expected_value, tolerance = 1e-7)
  })

  test_that("Custom transformation with a new name works alongside defaults", {
    # Modify YAML in memory for this test or use a new YAML.
    # Let's use a slightly different config string for this.
    temp_config_text <- '
model_name: "custom_transform_test"
centering:
  val_a: 10
custom_transform_test:
  intercepts:
    baseline: 1
  coefficients:
    val_a_centered: 1
    val_c_custom: 3
  transformations:
    - name: "val_a_centered"
      formula: "center_variable(val_a, centering.val_a)" # Uses default base
    - name: "val_c_custom"
      formula: "my_custom_doubler(val_c)"
  factors: []
  output_transformation: "add_value(result, 0)"
'
    temp_config_file <- tempfile(fileext = ".yaml")
    writeLines(temp_config_text, temp_config_file)
    on.exit(unlink(temp_config_file))

    my_custom_doubler_func <- function(x) { x * 2 }

    # User provides ONLY their new function. Defaults should still apply for others.
    # This test relies on the .default_rydra_transformations being merged with user's list,
    # if user list doesn't override.
    # The current plan: user list REPLACES default. So this test needs to be adjusted.
    # If user provides a list, that's IT. No merging.
    # So, if they want `center_variable` AND `my_custom_doubler`, they must provide both.

    user_funcs_only_custom <- list(
      my_custom_doubler = my_custom_doubler_func
      # center_variable is NOT provided here.
    )

    input_data <- list(val_a = 12, val_c = 5) # centering.val_a = 10

    # With current replacement logic:
    # center_variable will NOT be found if only `my_custom_doubler` is passed.
    # So, val_a_centered will fail to compute.
    # This test should demonstrate that.

    expect_warning(
      rydra_calculate(
        config_path = temp_config_file,
        data = input_data,
        model_name = "custom_transform_test",
        # Include output transformation function explicitly to surface the intended warning about missing center_variable
        transformations = c(user_funcs_only_custom, list(add_value = Rydra::add_value))
      ),
      regexp = "Error evaluating transformation 'val_a_centered'"
    )

    # Now, test providing BOTH custom and necessary base functions
    user_funcs_mixed <- list(
      my_custom_doubler = my_custom_doubler_func,
      center_variable = Rydra::center_variable # Explicitly include needed base func
    )

    # val_a_centered = 12 - 10 = 2 (using base center_variable)
    # val_c_custom = my_custom_doubler(5) = 10 (using custom function)
    # result = baseline(1) + 1*2 + 3*10 = 1 + 2 + 30 = 33
    expected_value_mixed <- 33

    actual_value_mixed <- suppressWarnings(rydra_calculate(
      config_path = temp_config_file,
      data = input_data,
      model_name = "custom_transform_test",
      transformations = c(user_funcs_mixed, list(add_value = Rydra::add_value))
    ))
    expect_equal(actual_value_mixed, expected_value_mixed, tolerance = 1e-7)
  })

  # Test for the hardcoded plgf_model issue in apply_transformations
  # This test will currently FAIL because apply_transformations uses config$plgf_model
  # instead of the actual model_name from the rydra_calculate call.
  # Once that is fixed, this test should pass.
  test_that("apply_transformations uses the correct model_name from rydra_calculate", {
    # test_config_simple_transform.yaml defines "transform_test_model"
    # If apply_transformations incorrectly looks for "plgf_model" in this config,
    # it will not find any transformations to apply (or error if plgf_model doesn't exist)
    # and the calculation would be wrong.

    input_data <- list(val_a = 15, val_b = exp(2))
    expected_value <- 9 # Same as the first test

    # This call uses model_name = "transform_test_model"
    actual_value <- rydra_calculate(
      config_path = config_path_simple_transform,
      data = input_data,
      model_name = "transform_test_model" # Explicitly use the model in the YAML
    )

    # If apply_transformations internally hardcodes plgf_model, it might find no transformations
    # in test_config_simple_transform.yaml under that name.
    # If no transformations are found, val_a_centered and val_b_logged would not be created.
    # Then the score would be baseline (0) + (1 * data$val_a_centered) + (2 * data$val_b_logged).
    # If these are NULL/NA, score would be 0 or NA.
    # If it correctly uses "transform_test_model", the result is 9.
    expect_equal(actual_value, expected_value, tolerance = 1e-7,
                 info = "This test checks if apply_transformations correctly uses the model_name passed to rydra_calculate. It might fail if apply_transformations hardcodes 'plgf_model'.")
  })

  # Add the new test file to tests/testthat.R if it's not automatically picked up.
  # Typically, testthat runs all files matching `test-*.R` in this directory.
  # Make sure basic_transformations functions are exported by Rydra package. (They are)
  # Make sure to run devtools::load_all() or install the package before testing.

  test_that("Using a transformation function not in the list fails as expected", {
    temp_config_text <- '
model_name: "missing_func_test"
centering: {} # No centering needed for this test
missing_func_test:
  intercepts:
    baseline: 0
  coefficients:
    transformed_val: 1
  transformations:
    - name: "transformed_val"
      formula: "a_truly_missing_function(val)"
  factors: []
  output_transformation: "add_value(result, 0)"
'
    temp_config_file <- tempfile(fileext = ".yaml")
    writeLines(temp_config_text, temp_config_file)
    on.exit(unlink(temp_config_file))

    input_data <- list(val = 5)

    # Calling with default transformations (which don't include 'a_truly_missing_function')
    expect_warning(
      rydra_calculate(
        config_path = temp_config_file,
        data = input_data,
        model_name = "missing_func_test"
      ),
      regexp = "Error evaluating transformation 'transformed_val'"
    )
  })
})
