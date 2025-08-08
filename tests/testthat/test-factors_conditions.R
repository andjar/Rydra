test_that("Factors and conditionals", {
  library(testthat)
  library(Rydra)

  # --- FACTORS ---
  test_that("apply_factors returns correct coefficient for single factor", {
    config <- list(
      test_model = list(
        factors = list(
          list(name = "color", levels = list(
            list(value = "red", coefficient = 1.5),
            list(value = "blue", coefficient = 2.5)
          ))
        )
      )
    )
    data <- list(color = "blue")
    expect_equal(apply_factors(config, data, model_name = "test_model"), 2.5)
  })

  test_that("apply_factors returns sum for multiple factors", {
    config <- list(
      test_model = list(
        factors = list(
          list(name = "color", levels = list(
            list(value = "red", coefficient = 1),
            list(value = "blue", coefficient = 2)
          )),
          list(name = "shape", levels = list(
            list(value = "circle", coefficient = 10),
            list(value = "square", coefficient = 20)
          ))
        )
      )
    )
    data <- list(color = "red", shape = "square")
    expect_equal(apply_factors(config, data, model_name = "test_model"), 21)
  })

  test_that("apply_factors warns for invalid factor level", {
    config <- list(
      test_model = list(
        factors = list(
          list(name = "color", levels = list(
            list(value = "red", coefficient = 1),
            list(value = "blue", coefficient = 2)
          ))
        )
      )
    )
    data <- list(color = "green")
    expect_warning(apply_factors(config, data, model_name = "test_model"),
                   "No matching level found for factor 'color' with value 'green'.")
  })

  test_that("apply_factors returns 0 if no factors defined", {
    config <- list(test_model = list(factors = NULL))
    data <- list()
    expect_equal(apply_factors(config, data, model_name = "test_model"), 0)
  })

  # --- CONDITIONS ---
  test_that("apply_conditions applies correct coefficients for met conditions", {
    config <- list(
      test_model = list(
        intercepts = list(bonus = 5),
        coefficients = list(penalty = -2),
        conditions = list(
          list(name = "bonus_if_true", condition = "x > 10", coefficient = "intercepts.bonus"),
          list(name = "penalty_if_false", condition = "y == 0", coefficient = "coefficients.penalty")
        )
      )
    )
    data <- list(x = 15, y = 0)
    # Both conditions met: 5 + (-2) = 3
    expect_equal(apply_conditions(model_config = config$test_model, data = data), 3)
  })

  test_that("apply_conditions returns 0 if no conditions met", {
    config <- list(
      test_model = list(
        intercepts = list(bonus = 5),
        coefficients = list(penalty = -2),
        conditions = list(
          list(name = "bonus_if_true", condition = "x > 10", coefficient = "intercepts.bonus"),
          list(name = "penalty_if_false", condition = "y == 0", coefficient = "coefficients.penalty")
        )
      )
    )
    data <- list(x = 5, y = 1)
    expect_equal(apply_conditions(model_config = config$test_model, data = data), 0)
  })

  test_that("apply_conditions handles malformed condition (missing coefficient)", {
    config <- list(
      test_model = list(
        conditions = list(
          list(name = "bad", condition = "x > 0")
        )
      )
    )
    data <- list(x = 1)
    # The error is a warning in the new function, and it returns sum of valid conditions.
    # For a single malformed condition, the sum should be 0, and a warning issued.
    expect_warning(
      result <- apply_conditions(model_config = config$test_model, data = data),
      "Condition item #1 in YAML is invalid."
    )
    expect_equal(result, 0) # No valid coefficient should be added
  })

  test_that("apply_conditions returns 0 if no conditions defined", {
    config <- list(test_model = list(conditions = NULL)) # model_config directly
    data <- list(x = 1)
    expect_equal(apply_conditions(model_config = config$test_model, data = data), 0)
  })

  # Test that apply_conditions can access model_config elements (e.g. intercepts) in condition formula
  test_that("apply_conditions allows conditions to reference model_config elements", {
    config_complex_condition <- list(
      test_model_cond = list(
        intercepts = list(age_threshold = 40, bonus_coeff = 5),
        conditions = list(
          list(name = "age_bonus",
               condition = "age > intercepts.age_threshold", # References model_config$intercepts
               coefficient = "intercepts.bonus_coeff")
        )
      )
    )
    data_above_threshold <- list(age = 45)
    data_below_threshold <- list(age = 35)

    expect_equal(
      apply_conditions(model_config = config_complex_condition$test_model_cond, data = data_above_threshold),
      5 # age (45) > intercepts.age_threshold (40) is true, apply intercepts.bonus_coeff (5)
    )
    expect_equal(
      apply_conditions(model_config = config_complex_condition$test_model_cond, data = data_below_threshold),
      0 # age (35) > intercepts.age_threshold (40) is false
    )
  })
})