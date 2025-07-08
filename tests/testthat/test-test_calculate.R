test_that("rydra_calculate main functionality", {
  library(testthat)
  # No need to library(Rydra) here if tests/testthat.R does it,
  # but good practice for individual file runs.
  # library(Rydra)

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

    # Corrected input data for example_config.yaml and its 'main_model'
    input_data <- list(
      age = 35,
      income = 60000,
      student = 1, # This corresponds to student_modifier
      employment_status = "Unemployed" # Corresponds to employment_unemployed
    )

    # Manually calculated expected result for example_config.yaml with 'main_model':
    # Centering: age: 30, income: 50000
    # Transformations:
    #   age_centered = center_variable(35, 30) = 5
    #   age_squared_centered = square_variable(5) = 25
    #   income_log = log_transform(60000) approx 11.002102
    # Intercepts: baseline: 1.25
    # Coefficients (direct terms):
    #   age_centered: 0.05 * 5 = 0.25
    #   age_squared_centered: -0.02 * 25 = -0.50
    #   income_log: 0.15 * 11.002102 = 1.6503153
    # Base Score = 1.25 + 0.25 - 0.50 + 1.6503153 = 2.6503153
    # Factors:
    #   student = 1 -> student_modifier = -0.50
    #   employment_status = "Unemployed" -> employment_unemployed = -0.25
    # Factor Sum = -0.50 + (-0.25) = -0.75
    # Conditions: None in example_config.yaml, so sum = 0.
    # Total Score = Base Score + Factor Sum + Conditions Sum = 2.6503153 - 0.75 + 0 = 1.9003153
    # Output Transformation: result * 100 = 1.9003153 * 100 = 190.03153
    expected_value <- 190.03153

    # Use model_name = "main_model" as defined in example_config.yaml
    actual_value <- Rydra::rydra_calculate(
      config_path = config_file_path,
      data = input_data,
      model_name = "main_model" # Specify the correct model name
    )

    expect_equal(actual_value, expected_value, tolerance = 1e-5) # Adjusted tolerance slightly
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
})
