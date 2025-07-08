context("Base transformation functions")
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