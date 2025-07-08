library(testthat)
library(Rydra)

# center_variable tests
test_that("center_variable subtracts center correctly", {
  expect_equal(center_variable(10, 3), 7)
  expect_equal(center_variable(-5, -2), -3)
  expect_equal(center_variable(0, 0), 0)
  expect_equal(center_variable(NA, 2), NA_real_)
  expect_equal(center_variable(c(1, 2, 3), 2), c(-1, 0, 1))
})

# square_variable tests
test_that("square_variable squares input correctly", {
  expect_equal(square_variable(4), 16)
  expect_equal(square_variable(-3), 9)
  expect_equal(square_variable(0), 0)
  expect_equal(square_variable(NA), NA_real_)
  expect_equal(square_variable(c(2, -2)), c(4, 4))
})

# log_transform tests
test_that("log_transform computes natural log by default", {
  expect_equal(log_transform(exp(1)), 1)
  expect_equal(log_transform(1), 0)
  expect_equal(log_transform(c(1, exp(1))), c(0, 1))
})

test_that("log_transform computes log with specified base", {
  expect_equal(log_transform(8, base = 2), 3)
  expect_equal(log_transform(100, base = 10), 2)
  expect_equal(log_transform(1, base = 10), 0)
})

test_that("log_transform handles edge cases", {
  expect_true(is.nan(log_transform(-1)))
  expect_equal(log_transform(0), -Inf)
  expect_equal(log_transform(NA), NA_real_)
})

# exp_transform tests
test_that("exp_transform computes exponential with default base", {
  expect_equal(exp_transform(1), exp(1))
  expect_equal(exp_transform(0), 1)
  expect_equal(exp_transform(-1), 1/exp(1))
  expect_equal(exp_transform(NA), NA_real_)
  expect_equal(exp_transform(c(0, 1)), c(1, exp(1)))
})

test_that("exp_transform computes exponential with specified base", {
  expect_equal(exp_transform(2, base = 10), 100)
  expect_equal(exp_transform(3, base = 2), 8)
  expect_equal(exp_transform(0, base = 5), 1)
})

# truncate_variable tests
test_that("truncate_variable truncates at lower bound", {
  expect_equal(truncate_variable(5, 10, 20), 10)
  expect_equal(truncate_variable(-5, 0, 10), 0)
  expect_equal(truncate_variable(c(1, 6, 11), 5, 10), c(5, 6, 10))
})

test_that("truncate_variable truncates at upper bound", {
  expect_equal(truncate_variable(25, 10, 20), 20)
  expect_equal(truncate_variable(15, 0, 10), 10)
  expect_equal(truncate_variable(c(1, 6, 11), 0, 5), c(1, 5, 5))
})

test_that("truncate_variable leaves values within bounds unchanged", {
  expect_equal(truncate_variable(15, 10, 20), 15)
  expect_equal(truncate_variable(5, 0, 10), 5)
  expect_equal(truncate_variable(c(2, 7, 9), 0, 10), c(2, 7, 9))
})

test_that("truncate_variable handles NA values correctly", {
  expect_equal(truncate_variable(NA, 0, 10), NA_real_)
  expect_equal(truncate_variable(c(1, NA, 11), 0, 10), c(1, NA_real_, 10))
  expect_equal(truncate_variable(c(NA, 5, NA), 0, 10), c(NA_real_, 5, NA_real_))
})

test_that("truncate_variable works with vector inputs and mixed cases", {
  input_vec <- c(-5, 5, 15, NA, 25)
  expected_vec <- c(0, 5, 10, NA_real_, 10)
  expect_equal(truncate_variable(input_vec, 0, 10), expected_vec)

  input_vec_2 <- c(NA, -10, 0, 50, 100, NA, 110)
  expected_vec_2 <- c(NA_real_, 0, 0, 50, 100, NA_real_, 100)
  expect_equal(truncate_variable(input_vec_2, 0, 100), expected_vec_2)
})

test_that("truncate_variable handles NA in min_val or max_val", {
  expect_equal(truncate_variable(c(-5, 5, 15), NA, 10), c(-5, 5, 10))
  expect_equal(truncate_variable(c(-5, 5, 15), 0, NA), c(0, 5, 15))
  expect_equal(truncate_variable(c(-5, 5, 15), NA, NA), c(-5, 5, 15))
  expect_equal(truncate_variable(c(NA, 0, 20), NA, 10), c(NA_real_, 0, 10))
  expect_equal(truncate_variable(c(NA, 0, 20), 5, NA), c(NA_real_, 5, 20))
})

test_that("truncate_variable handles Inf in min_val or max_val", {
  expect_equal(truncate_variable(c(-5, 5, 15, Inf, -Inf), -Inf, 10), c(-5, 5, 10, 10, -Inf))
  expect_equal(truncate_variable(c(-5, 5, 15, Inf, -Inf), 0, Inf), c(0, 5, 15, Inf, 0))
  expect_equal(truncate_variable(c(-5, 5, 15, Inf, -Inf), -Inf, Inf), c(-5, 5, 15, Inf, -Inf))
  # Test with actual Inf values in data
  expect_equal(truncate_variable(c(Inf, -Inf, 5), 0, 10), c(10, 0, 5))
})

test_that("truncate_variable errors with non-scalar min_val or max_val", {
  expect_error(truncate_variable(10, c(1,2), 5), "min_val and max_val must be single numeric values.")
  expect_error(truncate_variable(10, 1, c(5,6)), "min_val and max_val must be single numeric values.")
  expect_error(truncate_variable(10, c(1,2), c(5,6)), "min_val and max_val must be single numeric values.")
})
