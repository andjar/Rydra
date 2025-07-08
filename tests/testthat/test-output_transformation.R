context("Output Transformation Logic")

library(testthat)
library(Rydra) # Ensures Rydra functions are available

# Base config path for output transformation tests
base_config_path_output <- system.file("extdata", "test_config_output_transform.yaml", package = "Rydra")
if (base_config_path_output == "") { # Fallback for dev environment
    base_config_path_output <- "inst/extdata/test_config_output_transform.yaml"
}

# Helper function to run rydra_calculate with a modified output_transformation
run_with_output_transform <- function(output_expr_string) {
  config <- yaml::read_yaml(base_config_path_output)
  config$output_transform_test_model$output_transformation <- output_expr_string

  temp_config_file <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config, temp_config_file)
  on.exit(unlink(temp_config_file))

  # Input data is minimal as only baseline intercept matters for these tests before output_transformation
  input_data <- list()

  # Initial result before output_transformation is baseline = 10
  rydra_calculate(
    config_path = temp_config_file,
    data = input_data,
    model_name = "output_transform_test_model"
  )
}

test_that("Output transformation: simple multiplication", {
  # Initial result = 10. Output: result * 5
  expect_equal(run_with_output_transform("result * 5"), 50)
})

test_that("Output transformation: addition and power", {
  # Initial result = 10. Output: (result + 5)^2 = 15^2 = 225
  expect_equal(run_with_output_transform("(result + 5)^2"), 225)
})

test_that("Output transformation: using R functions like log/exp", {
  # Initial result = 10. Output: log(result, base = exp(1)) * exp(1)
  # log(10) * exp(1) approx 2.302585 * 2.718282 = 6.2579
  expect_equal(run_with_output_transform("log(result) * exp(1)"), log(10) * exp(1), tolerance = 1e-6)
})

test_that("Output transformation: no transformation if string is empty", {
  # Initial result = 10.
  expect_equal(run_with_output_transform(""), 10) # Empty string means no output transform
})

test_that("Output transformation: no transformation if section is NULL (handled by validator or default)", {
  # This test assumes that if output_transformation is NULL in config, it defaults to "result" or is handled.
  # The current rydra_calculate uses `nzchar`, so NULL would be like empty.
  config <- yaml::read_yaml(base_config_path_output)
  config$output_transform_test_model$output_transformation <- NULL # Set to NULL explicitly

  temp_config_file <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config, temp_config_file)
  on.exit(unlink(temp_config_file))

  input_data <- list()
  actual_value <- rydra_calculate(
    config_path = temp_config_file,
    data = input_data,
    model_name = "output_transform_test_model"
  )
  expect_equal(actual_value, 10) # Expect raw result if output_transformation is NULL
})


test_that("Output transformation: error in expression", {
  # Initial result = 10. Output: result + "a_string" (should error)
  expect_error(run_with_output_transform("result + \"a_string\""),
               regexp = "Error evaluating output transformation")
})

test_that("Output transformation: references a non-existent variable", {
  # Initial result = 10. Output: result + non_existent_var (should error)
  expect_error(run_with_output_transform("result + non_existent_var"),
               regexp = "Error evaluating output transformation.*object 'non_existent_var' not found")
})
