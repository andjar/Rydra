test_that("Constants key support (default, legacy, and custom)", {
  library(testthat)
  library(Rydra)

  # 1) Default 'constants' at root, referenced in transformations
  cfg1 <- list(
    model_name = "m1",
    constants = list(x0 = 10),
    m1 = list(
      intercepts = list(baseline = 0),
      coefficients = list(x_c = 1),
      transformations = list(
        list(name = "x_c", formula = "center_variable(x, constants.x0)")
      ),
      factors = list()
    )
  )
  f1 <- tempfile(fileext = ".yaml"); yaml::write_yaml(cfg1, f1); on.exit(unlink(f1), add = TRUE)
  expect_equal(rydra_calculate(f1, list(x = 15)), 5)

  # 2) Legacy 'centering' at root still works
  cfg2 <- list(
    model_name = "m2",
    centering = list(x0 = 10),
    m2 = list(
      intercepts = list(baseline = 0),
      coefficients = list(x_c = 1),
      transformations = list(
        list(name = "x_c", formula = "center_variable(x, centering.x0)")
      ),
      factors = list()
    )
  )
  f2 <- tempfile(fileext = ".yaml"); yaml::write_yaml(cfg2, f2); on.exit(unlink(f2), add = TRUE)
  expect_equal(rydra_calculate(f2, list(x = 12)), 2)

  # 3) Custom root key via constants_key argument, and usage in output_transformation
  cfg3 <- list(
    model_name = "m3",
    globals = list(x0 = 7, k = 3),
    m3 = list(
      intercepts = list(baseline = 0),
      coefficients = list(x_c = 1),
      transformations = list(
        list(name = "x_c", formula = "center_variable(x, constants.x0)")
      ),
      factors = list(),
      output_transformation = "multiply_by(result, constants.k)"
    )
  )
  f3 <- tempfile(fileext = ".yaml"); yaml::write_yaml(cfg3, f3); on.exit(unlink(f3), add = TRUE)
  # x = 10, x_c = 10 - 7 = 3, result pre-output = 3; multiply_by 3 => 9
  expect_equal(rydra_calculate(f3, list(x = 10), constants_key = "globals"), 9)
})


