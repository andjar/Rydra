test_that("Output Transformation Logic", {
  library(testthat)
  library(Rydra) # Ensures Rydra functions are available

  # Base config path for output transformation tests
  base_config_path_output <- system.file("extdata", "test_config_output_transform.yaml", package = "Rydra")
  if (base_config_path_output == "") { # Fallback for dev environment
      base_config_path_output <- "inst/extdata/test_config_output_transform.yaml"
  }

  # Helper function to run rydra_calculate with a modified output_transformation
  # and optionally a custom set of transformation functions
  run_with_output_transform <- function(output_expr_string, transformations_list = Rydra:::.default_rydra_transformations) {
    config <- yaml::read_yaml(base_config_path_output)
    config$output_transform_test_model$output_transformation <- output_expr_string

    temp_config_file <- tempfile(fileext = ".yaml")
    yaml::write_yaml(config, temp_config_file)
    on.exit(unlink(temp_config_file))

    # Input data is minimal as only baseline intercept matters for these tests before output_transformation
    input_data <- list()

    # Initial result before output_transformation is baseline = 10 (from test_config_output_transform.yaml)
    rydra_calculate(
      config_path = temp_config_file,
      data = input_data,
      model_name = "output_transform_test_model",
      transformations = transformations_list
    )
  }

  test_that("Output transformation: using multiply_by", {
    # Initial result = 10. Output: multiply_by(result, 5)
    expect_equal(run_with_output_transform("multiply_by(result, 5)"), 50)
    expect_equal(run_with_output_transform("multiply_by(result, 0)"), 0)
    expect_equal(run_with_output_transform("multiply_by(result, -2)"), -20)
    expect_equal(run_with_output_transform("multiply_by(result, 1.5)"), 15)
  })

  test_that("Output transformation: using add_value", {
    # Initial result = 10. Output: add_value(result, 5)
    expect_equal(run_with_output_transform("add_value(result, 5)"), 15)
    expect_equal(run_with_output_transform("add_value(result, -5)"), 5)
    expect_equal(run_with_output_transform("add_value(result, 0)"), 10)
    expect_equal(run_with_output_transform("add_value(result, 2.5)"), 12.5)
  })

  test_that("Output transformation: using existing log_transform from default transformations", {
    # Initial result = 10.
    expect_equal(run_with_output_transform("log_transform(result)"), log(10))
    expect_equal(run_with_output_transform("log_transform(result, base = 10)"), log(10, base = 10))
  })

  test_that("Output transformation: chaining allowed functions (if they are part of the expression)", {
    # Initial result = 10. multiply_by(add_value(result, 5), 2) = multiply_by(15, 2) = 30
    expect_equal(run_with_output_transform("multiply_by(add_value(result, 5), 2)"), 30)
  })

  test_that("Output transformation: no transformation if string is empty", {
    # Initial result = 10.
    expect_equal(run_with_output_transform(""), 10)
  })

  test_that("Output transformation: no transformation if section is NULL", {
    config <- yaml::read_yaml(base_config_path_output)
    config$output_transform_test_model$output_transformation <- NULL

    temp_config_file <- tempfile(fileext = ".yaml")
    yaml::write_yaml(config, temp_config_file)
    on.exit(unlink(temp_config_file))

    input_data <- list()
    actual_value <- rydra_calculate(
      config_path = temp_config_file,
      data = input_data,
      model_name = "output_transform_test_model"
      # uses default transformations
    )
    expect_equal(actual_value, 10)
  })

  test_that("Output transformation: error if function is not in available transformations", {
    # Initial result = 10. unknown_function is not in .default_rydra_transformations
    expect_error(run_with_output_transform("unknown_function(result, 1)"),
                 regexp = "Output transformation function 'unknown_function'.*not found in the available transformations list")

    # Test with a custom list of transformations that doesn't include multiply_by
    custom_transforms <- list(some_other_func = function(x) x + 1)
    expect_error(run_with_output_transform("multiply_by(result, 10)", transformations_list = custom_transforms),
                 regexp = "Output transformation function 'multiply_by'.*not found in the available transformations list")
  })

  test_that("Output transformation: error for invalid function call format (not a function call)", {
    expect_error(run_with_output_transform("result * 10"),
                 regexp = "Output transformation 'result \\* 10' is not a valid function call format")
    expect_error(run_with_output_transform("result + 5"),
                 regexp = "Output transformation 'result \\+ 5' is not a valid function call format")
    expect_error(run_with_output_transform("100"), # Just a number, not a function call
                 regexp = "Output transformation '100' is not a valid function call format")
  })


  test_that("Output transformation: error during evaluation (e.g. wrong number of args, type error)", {
    # multiply_by expects 2 args, given 1 (result is implicitly the first, then needs one more)
    expect_error(run_with_output_transform("multiply_by(result)"),
                 regexp = "Error evaluating output transformation .*argument \"multiplier\" is missing")

    # add_value expects 2 args, given 3
    expect_error(run_with_output_transform("add_value(result, 5, 10)"),
                 regexp = "Error evaluating output transformation .*unused argument (10)") # R's error for too many args

    # Type error within the function
    expect_error(run_with_output_transform("multiply_by(result, \"a_string\")"),
                 regexp = "Error evaluating output transformation .*non-numeric argument to binary operator")
  })

  test_that("Output transformation: references a non-existent variable within arguments", {
    # Initial result = 10. non_existent_var is not in eval_env_output
    expect_error(run_with_output_transform("multiply_by(result, non_existent_var)"),
                 regexp = "Error evaluating output transformation .*object 'non_existent_var' not found")
  })

  test_that("Output transformation: using centering constants if available in env", {
    # test_config_output_transform.yaml has centering: { my_center: 3 }
    # initial result = 10. multiply_by(result, centering.my_center) = 10 * 3 = 30
    expect_equal(run_with_output_transform("multiply_by(result, centering.my_center)"), 30)
  })
})
