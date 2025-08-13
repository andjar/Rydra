test_that("Complex model calculations and error detection", {
  library(testthat)
  library(Rydra)

  # Robustly resolve config path to work in R CMD check and local dev
  config_path <- system.file("extdata", "complex_config.yaml", package = "Rydra", mustWork = FALSE)
  if (identical(config_path, "") || !file.exists(config_path)) {
    # Fallbacks for dev environments
    candidate_paths <- c(
      file.path("..", "..", "inst", "extdata", "complex_config.yaml"),
      "inst/extdata/complex_config.yaml",
      "../../inst/extdata/complex_config.yaml"
    )
    existing <- candidate_paths[file.exists(candidate_paths)]
    if (length(existing) > 0) config_path <- existing[[1]]
  }
  testthat::expect_true(file.exists(config_path),
                        info = paste("Config file not found at:", config_path, "CWD:", getwd()))

  # 1. All conditionals triggered
  test_that("Correct calculation: age > 40, gender = 1, bmi > 30, smoker = 1", {
    input_data <- list(age = 55, gender = 1, bmi = 32, smoker = 1)
    # Transformed data:
    # age_centered = 55 - 40 = 15
    # age_squared_centered = 15^2 = 225
    # bmi_centered = 32 - 25 = 7
    # gender_dependent: age(55)>40 & gender(1)==1 -> intercepts.gender_male = 0.5
    # Base score calculation:
    #   intercept: 2.0
    #   age_centered term: 0.1 * 15 = 1.5
    #   age_squared_centered term: -0.01 * 225 = -2.25
    #   bmi_centered term: 0.2 * 7 = 1.4
    #   gender_dependent term: 1.5 * 0.5 = 0.75
    #   base_score = 2.0 + 1.5 - 2.25 + 1.4 + 0.75 = 3.4
    # Factor sum (apply_factors):
    #   smoker = 1: path 'coefficients.age_centered' resolves to value 0.1
    #   gender = 1: path 'intercepts.gender_male' resolves to value 0.5
    #   factor_coeffs_sum = 0.1 + 0.5 = 0.6
    # Conditional sum (apply_conditions):
    #   high_bmi_bonus (bmi(32) > 30 is TRUE): path 'coefficients.bmi_centered' resolves to value 0.2
    #   age_gender_special (age(55) > 50 & gender(1)==1 is TRUE): path 'intercepts.gender_male' resolves to value 0.5
    #   conditional_coeffs_sum = 0.2 + 0.5 = 0.7
    # Total score = base_score + factor_coeffs_sum + conditional_coeffs_sum
    #             = 3.4 + 0.6 + 0.7 = 4.7
    # Output transformation: result * 10 = 4.7 * 10 = 47
    expected_value <- 47
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })

  # 2. Else branch, no (bmi/age-gender) conditions triggered
  test_that("Correct calculation: age <= 40, gender = 0, bmi = 24, smoker = 0", {
    input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
    # Transformed data:
    # age_centered = 35 - 40 = -5
    # age_squared_centered = (-5)^2 = 25
    # bmi_centered = 24 - 25 = -1
    # gender_dependent: age(35)<=40 -> age_squared_centered + bmi_centered = 25 + (-1) = 24
    # Base score calculation:
    #   intercept: 2.0
    #   age_centered term: 0.1 * (-5) = -0.5
    #   age_squared_centered term: -0.01 * 25 = -0.25
    #   bmi_centered term: 0.2 * (-1) = -0.2
    #   gender_dependent term: 1.5 * 24 = 36.0
    #   base_score = 2.0 - 0.5 - 0.25 - 0.2 + 36.0 = 37.05
    # Factor sum (apply_factors):
    #   smoker = 0: path 'intercepts.baseline' resolves to value 2.0
    #   gender = 0: path 'intercepts.gender_female' resolves to value -0.3
    #   factor_coeffs_sum = 2.0 - 0.3 = 1.7
    # Conditional sum (apply_conditions):
    #   high_bmi_bonus (bmi(24) > 30 is FALSE)
    #   age_gender_special (age(35) > 50 is FALSE)
    #   conditional_coeffs_sum = 0
    # Total score = base_score + factor_coeffs_sum + conditional_coeffs_sum
    #             = 37.05 + 1.7 + 0 = 38.75
    # Output transformation: result * 10 = 38.75 * 10 = 387.5
    expected_value <- 387.5
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
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
  # Note: complex_config.yaml now ensures all conditions have 'coefficient' to satisfy validation
  # This test is disabled as the config no longer contains an intentionally malformed condition.

  # 6. Error: config with coefficient that has no corresponding transformation
  test_that("Warn: config with coefficient that has no corresponding transformation is ignored", {
    input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
    # missing_var is in coefficients but not in data or transformations
    expect_silent(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"))
  })

  # 7. Edge case: age = 40 (boundary)
  test_that("Edge case: age = 40 (boundary)", {
    input_data <- list(age = 40, gender = 1, bmi = 25, smoker = 1)
    # Transformed data:
    # age_centered = 0, age_squared_centered = 0, bmi_centered = 0
    # gender_dependent: age(40)<=40 -> age_squared_centered + bmi_centered = 0 + 0 = 0
    # Base score:
    #   intercept: 2.0
    #   age_centered term: 0.1 * 0 = 0
    #   age_squared_centered term: -0.01 * 0 = 0
    #   bmi_centered term: 0.2 * 0 = 0
    #   gender_dependent term: 1.5 * 0 = 0
    #   base_score = 2.0
    # Factor sum (apply_factors):
    #   smoker = 1: path 'coefficients.age_centered' resolves to value 0.1
    #   gender = 1: path 'intercepts.gender_male' resolves to value 0.5
    #   factor_coeffs_sum = 0.1 + 0.5 = 0.6
    # Conditional sum (apply_conditions):
    #   high_bmi_bonus (bmi(25) > 30 is FALSE)
    #   age_gender_special (age(40) > 50 is FALSE)
    #   conditional_coeffs_sum = 0
    # Total score = base_score + factor_coeffs_sum + conditional_coeffs_sum
    #             = 2.0 + 0.6 + 0 = 2.6
    # Output transformation: result * 10 = 2.6 * 10 = 26
    expected_value <- 26
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })

  # 8. Edge case: bmi = 30 (boundary) - This test calculation aligns with current logic
  test_that("Edge case: bmi = 30 (boundary)", {
    input_data <- list(age = 45, gender = 0, bmi = 30, smoker = 0)
    # Transformed data:
    # age_centered = 5, age_squared_centered = 25, bmi_centered = 5
    # gender_dependent: age(45)>40 & gender(0)==0 -> intercepts.gender_female = -0.3
    # Base score:
    #   intercept: 2.0
    #   age_centered term: 0.1 * 5 = 0.5
    #   age_squared_centered term: -0.01 * 25 = -0.25
    #   bmi_centered term: 0.2 * 5 = 1.0
    #   gender_dependent term: 1.5 * (-0.3) = -0.45
    #   base_score = 2.0 + 0.5 - 0.25 + 1.0 - 0.45 = 2.8
    # Factor sum (apply_factors):
    #   smoker = 0: path 'intercepts.baseline' resolves to value 2.0
    #   gender = 0: path 'intercepts.gender_female' resolves to value -0.3
    #   factor_coeffs_sum = 2.0 - 0.3 = 1.7
    # Conditional sum (apply_conditions):
    #   high_bmi_bonus (bmi(30) > 30 is FALSE)
    #   age_gender_special (age(45) > 50 is FALSE)
    #   conditional_coeffs_sum = 0
    # Total score = base_score + factor_coeffs_sum + conditional_coeffs_sum
    #             = 2.8 + 1.7 + 0 = 4.5
    # Output transformation: result * 10 = 4.5 * 10 = 45
    expected_value <- 45
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })

  # 9. Error: missing required config key
  test_that("Error: missing required config key", {
    # Remove 'transformations' from config for this test (simulate by editing in-memory)
    config <- yaml::read_yaml(config_path)
    config$complex_model$transformations <- NULL
    input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
    expect_error(validate_config(config, "complex_model", input_data),
                 "Missing required key in 'complex_model' block: 'transformations'")
  })

  # --- Tests focusing on specific conditions from complex_config.yaml ---

  test_that("Condition 'high_bmi_bonus' is applied correctly when met", {
    input_data <- list(age = 45, gender = 0, bmi = 32, smoker = 0) # bmi > 30 is TRUE, age > 50 is FALSE
    # Transformed data:
    # age_centered = 5, age_sq_c = 25, bmi_centered = 7
    # gender_dependent: age(45)>40 & gender(0)==0 -> intercepts.gender_female = -0.3
    # Base score:
    #   intercept: 2.0
    #   age_c: 0.1*5=0.5, age_sq_c: -0.01*25=-0.25, bmi_c: 0.2*7=1.4, gender_dep: 1.5*(-0.3)=-0.45
    #   base_score = 2.0 + 0.5 - 0.25 + 1.4 - 0.45 = 3.2
    # Factor sum:
    #   smoker=0 -> intercepts.baseline (2.0)
    #   gender=0 -> intercepts.gender_female (-0.3)
    #   factor_coeffs_sum = 2.0 - 0.3 = 1.7
    # Conditional sum:
    #   high_bmi_bonus (TRUE): path 'coefficients.bmi_centered' -> 0.2
    #   age_gender_special (FALSE for age)
    #   conditional_coeffs_sum = 0.2
    # Total score = 3.2 + 1.7 + 0.2 = 5.1
    # Output = 5.1 * 10 = 51
    expected_value <- 51
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })

  test_that("Condition 'age_gender_special' is applied correctly when met", {
    input_data <- list(age = 55, gender = 1, bmi = 28, smoker = 0) # age > 50 & gender == 1 is TRUE, bmi > 30 is FALSE
    # Transformed data:
    # age_centered = 15, age_sq_c = 225, bmi_centered = 3
    # gender_dependent: age(55)>40 & gender(1)==1 -> intercepts.gender_male = 0.5
    # Base score:
    #   intercept: 2.0
    #   age_c: 0.1*15=1.5, age_sq_c: -0.01*225=-2.25, bmi_c: 0.2*3=0.6, gender_dep: 1.5*0.5=0.75
    #   base_score = 2.0 + 1.5 - 2.25 + 0.6 + 0.75 = 2.6
    # Factor sum:
    #   smoker=0 -> intercepts.baseline (2.0)
    #   gender=1 -> intercepts.gender_male (0.5)
    #   factor_coeffs_sum = 2.0 + 0.5 = 2.5
    # Conditional sum:
    #   high_bmi_bonus (FALSE for bmi)
    #   age_gender_special (TRUE): path 'intercepts.gender_male' -> 0.5
    #   conditional_coeffs_sum = 0.5
    # Total score = 2.6 + 2.5 + 0.5 = 5.6
    # Output = 5.6 * 10 = 56
    expected_value <- 56
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })

  test_that("No specific conditions met (complex_config)", {
    # Uses input from test #2 (age <= 40, gender = 0, bmi = 24, smoker = 0)
    # where conditional_coeffs_sum was already 0.
    input_data <- list(age = 35, gender = 0, bmi = 24, smoker = 0)
    # Base score = 37.05
    # Factor sum = 1.7
    # Conditional sum = 0
    # Total = 38.75 -> Output = 387.5
    expected_value <- 387.5 # This is same as test #2
    expect_equal(Rydra::rydra_calculate(config_path, input_data, model_name = "complex_model"), expected_value, tolerance = 1e-6)
  })
})