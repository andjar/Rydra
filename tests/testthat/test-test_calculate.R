library(testthat)
# No need to library(Rydra) here if tests/testthat.R does it,
# but good practice for individual file runs.
# library(Rydra)

context("rydra_calculate main functionality")

test_that("rydra_calculate computes correct value with example_config", {
  # Path to the example config file
  # system.file returns path to installed files, need to adjust for local testing
  # For testing during development, relative paths from package root are easier.
  config_file_path <- file.path("..", "..", "inst", "extdata", "example_config.yaml")
  # A more robust way if tests are run from package root:
  # config_file_path <- "inst/extdata/example_config.yaml"
  # Let's ensure it finds it from `tests/testthat/`
  if (!file.exists(config_file_path)) { # if run from tests/testthat
    config_file_path <- system.file("extdata", "example_config.yaml", package = "Rydra", mustWork = FALSE)
    if (config_file_path == "") { # if package not installed, try relative from project root for dev
      config_file_path <- "../../inst/extdata/example_config.yaml" # from tests/testthat to inst/extdata
      if (!file.exists(config_file_path)) {
        config_file_path <- "inst/extdata/example_config.yaml" # from project root
      }
    }
  }

  # Ensure the config file exists at the determined path
  expect_true(file.exists(config_file_path),
              info = paste("Config file not found at:", config_file_path, "CWD:", getwd()))

  input_data <- list(
    biochemical_ga = 12, # weeks
    weight = 70,         # kg
    age = 30,            # years
    plgf_machine = 1,    # Delfia
    race = 1,            # Caucasian/Other
    smoking = 0,         # No
    diabetes_type_i = 0, # No
    diabetes_type_ii = 0,# No
    conception = 1,      # Spontaneous
    previous = 0,        # Nulliparous
    # diabetes_drugs not applicable
    # previous_pe not applicable
    # height_cm is in config but not used by this model's direct terms/transformations
    height_cm = 164
  )

  # Manually calculated expected result (as per detailed steps in prompt)
  # ga_centered = 12 * 7 - 77 = 7
  # ga_squared_centered = 7^2 = 49
  # weight_centered = 70 - 69 = 1
  # weight_squared_centered = 1^2 = 1
  # age_centered = 30 - 35 = -5
  #
  # base_score_terms = (7 * 0.012263018) + (49 * 0.000149743) + (1 * -0.001682761) +
  #                    (1 * 0.000008780) + (-5 * 0.002174191)
  # base_score_terms = 0.085841126 + 0.007337407 - 0.001682761 + 0.000008780 - 0.010870955
  # base_score = 0 (baseline) + 0.080633597 = 0.080633597
  #
  # conditional_coeffs_sum = 1.332959332 (for plgf_machine == 1, Delfia)
  #
  # total_score = 0.080633597 + 1.332959332 = 1.413592929
  #
  # final_result = 10^1.413592929 = 25.9176511
  expected_value <- 25.9176511

  actual_value <- Rydra::rydra_calculate(config_path = config_file_path, data = input_data)

  expect_equal(actual_value, expected_value, tolerance = 1e-7)
})

test_that("load_config handles non-existent file", {
  expect_error(load_config("path/to/non_existent_config.yaml"),
               "Configuration file not found: path/to/non_existent_config.yaml")
})

test_that("apply_transformations works correctly", {
  config <- list(
    centering = list(ga_days = 77),
    plgf_model = list(
      transformations = list(
        list(name = "ga_centered", formula = "ga * 7 - centering.ga_days")
      )
    )
  )
  data <- data.frame(ga = 12) # ga in weeks
  transformed_data <- apply_transformations(config, data)
  expect_equal(transformed_data$ga_centered, 12 * 7 - 77)
})

test_that("apply_conditions works correctly", {
  config <- list(
    plgf_model = list(
      intercepts = list(delfia = 1.5),
      coefficients = list(smoker_coeff = 0.5),
      conditions = list(
        list(name = "is_delfia", condition = "machine == 'delfia'", coefficient = "intercepts.delfia"),
        list(name = "is_smoker", condition = "smoking == TRUE", coefficient = "coefficients.smoker_coeff")
      )
    )
  )
  data <- list(machine = "delfia", smoking = TRUE)
  total_conditional_coeff <- apply_conditions(config, data)
  expect_equal(total_conditional_coeff, 1.5 + 0.5)

  data_no_smoke <- list(machine = "other", smoking = FALSE)
  total_conditional_coeff_no_smoke <- apply_conditions(config, data_no_smoke)
  expect_equal(total_conditional_coeff_no_smoke, 0)
})
