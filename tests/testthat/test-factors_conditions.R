context("Factors and conditionals")
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
                 "Coefficient for factor 'color' level 'green' is invalid or not found")
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
  expect_equal(apply_conditions(config, data, model_name = "test_model"), 3)
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
  expect_equal(apply_conditions(config, data, model_name = "test_model"), 0)
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
  expect_error(apply_conditions(config, data, model_name = "test_model"),
               "is invalid. It must be a list with 'name', 'condition', and 'coefficient' keys")
})

test_that("apply_conditions returns 0 if no conditions defined", {
  config <- list(test_model = list(conditions = NULL))
  data <- list(x = 1)
  expect_equal(apply_conditions(config, data, model_name = "test_model"), 0)
}) 