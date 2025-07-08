context("Complex model calculations and error detection")
library(testthat)
library(Rydra)

config_path <- file.path("..", "..", "inst", "extdata", "complex_config.yaml")

# 1. All conditionals triggered
test_that("Correct calculation: age > 40, gender = 1, bmi > 30, smoker = 1", {
  input_data <- list(age = 55, gender = 1, bmi = 32, smoker = 1)
  # gender_dependent = intercepts.gender_male = 0.5
  # age_centered = 15, age_squared_centered = 225, bmi_centered = 7
  # base = 2.0 + 0.1*15 + (-0.01)*225 + 0.2*7 + 1.5*0.5
  #      = 2.0 + 1.5 -2.25 + 1.4 + 0.75 = 3.4
  # high_bmi_bonus: bmi > 30, adds 0.2*7 = 1.4
  # age_gender_special: age > 50 & gender == 1, adds 0.5
  # smoker = 1: adds coefficients.age_centered = 0.1*15 = 1.5
  # gender = 1: adds intercepts.gender_male = 0.5
  # total = 3.4 + 1.4 + 0.5 + 1.5 + 0.5 = 7.3
  # output: 7.3 * 10 = 73
  expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), 73, tolerance = 1e-6)
})

# 2. Else branch, no conditionals
test_that("Correct calculation: age <= 40, gender = 0, bmi = 24, smoker = 0", {
  input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
  # gender_dependent = age_squared_centered + bmi_centered = (35-40)^2 + (24-25) = 25 + (-1) = 24
  # age_centered = -5, age_squared_centered = 25, bmi_centered = -1
  # base = 2.0 + 0.1*-5 + (-0.01)*25 + 0.2*-1 + 1.5*24
  #      = 2.0 -0.5 -0.25 -0.2 + 36 = 37.05
  # no conditions triggered
  # smoker = 0: adds intercepts.baseline = 2.0
  # gender = 0: adds intercepts.gender_female = -0.3
  # total = 37.05 + 2.0 - 0.3 = 38.75
  # output: 38.75 * 10 = 387.5
  expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), 387.5, tolerance = 1e-6)
})

# 3. Error: missing required factor in data
test_that("Error: missing required factor in data", {
  input_data <- list(age = 35, gender = 0, bmi = 24) # smoker missing
  expect_error(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"),
               "Input data is missing factor: 'smoker'")
})

# 4. Error: invalid factor level in data
test_that("Error: invalid factor level in data", {
  input_data <- list(age = 35, gender = 2, bmi = 24, smoker = 0) # gender=2 invalid
  expect_error(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"),
               "Value '2' for factor 'gender' in input data is not a defined level")
})

# 5. Error: config with missing 'coefficient' in a condition
test_that("Error: config with missing 'coefficient' in a condition", {
  input_data <- list(age = -1, gender = 0, bmi = 24, smoker = 0)
  expect_error(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"),
               "Condition item #3 is invalid. It must be a list with 'name', 'condition', and 'coefficient' keys.")
})

# 6. Error: config with coefficient that has no corresponding transformation
test_that("Error: config with coefficient that has no corresponding transformation", {
  input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
  # missing_var is in coefficients but not in data or transformations
  expect_warning(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"),
                 "Coefficient 'missing_var' found, but corresponding data is missing")
})

# 7. Edge case: age = 40 (boundary)
test_that("Edge case: age = 40 (boundary)", {
  input_data <- list(age = 40, gender = 1, bmi = 25, smoker = 1)
  # gender_dependent: else branch (age not > 40): (0)^2 + 0 = 0
  # age_centered = 0, age_squared_centered = 0, bmi_centered = 0
  # base = 2.0 + 0 + 0 + 0 + 1.5*0 = 2.0
  # smoker = 1: adds coefficients.age_centered = 0
  # gender = 1: adds intercepts.gender_male = 0.5
  # total = 2.0 + 0 + 0.5 = 2.5
  # output: 2.5 * 10 = 25
  expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), 25, tolerance = 1e-6)
})

# 8. Edge case: bmi = 30 (boundary)
test_that("Edge case: bmi = 30 (boundary)", {
  input_data <- list(age = 45, gender = 0, bmi = 30, smoker = 0)
  # high_bmi_bonus not triggered (bmi not > 30)
  # gender_dependent = intercepts.gender_female = -0.3
  # age_centered = 5, age_squared_centered = 25, bmi_centered = 5
  # base = 2.0 + 0.1*5 + (-0.01)*25 + 0.2*5 + 1.5*(-0.3)
  #      = 2.0 + 0.5 -0.25 + 1.0 -0.45 = 2.8
  # smoker = 0: adds intercepts.baseline = 2.0
  # gender = 0: adds intercepts.gender_female = -0.3
  # total = 2.8 + 2.0 - 0.3 = 4.5
  # output: 4.5 * 10 = 45
  expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), 45, tolerance = 1e-6)
})

# 9. Error: missing required config key
test_that("Error: missing required config key", {
  # Remove 'output_transformation' from config for this test (simulate by editing in-memory)
  config <- yaml::read_yaml(config_path)
  config$complex_model$output_transformation <- NULL
  input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
  expect_error(validate_config(config, "complex_model", input_data),
               "Missing required key in 'complex_model' block: 'output_transformation'")
}) 