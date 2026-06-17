test_that("divergence_coefficient separates well-separated groups", {
  set.seed(42)
  a <- matrix(rnorm(40, mean = 0), ncol = 2)
  b <- matrix(rnorm(40, mean = 5), ncol = 2)
  D <- divergence_coefficient(a, b)
  expect_gt(D, 1.0)
})

test_that("divergence_coefficient returns a single numeric", {
  a <- matrix(rnorm(20), ncol = 2)
  b <- matrix(rnorm(20), ncol = 2)
  D <- divergence_coefficient(a, b)
  expect_type(D, "double")
  expect_length(D, 1)
})

test_that("divergence_coefficient matches analytic value", {
  # One feature. Group a: c(0, 2) -> mean 1, var 2.
  # Group b: c(8, 10) -> mean 9, var 2.
  # D = (1 - 9)^2 / (2 + 2) = 64 / 4 = 16.
  a <- matrix(c(0, 2), ncol = 1)
  b <- matrix(c(8, 10), ncol = 1)
  expect_equal(divergence_coefficient(a, b), 16)
})

test_that("divergence_coefficient handles single-feature (1D) input", {
  a <- c(1, 2, 3)
  b <- c(7, 8, 9)
  D <- divergence_coefficient(a, b)
  expect_length(D, 1)
  expect_gt(D, 0)
})

test_that("divergence_coefficient of identical groups is zero", {
  a <- matrix(c(1, 2, 3, 4), ncol = 2)
  expect_equal(divergence_coefficient(a, a), 0)
})

test_that("divergence_coefficient skips zero-variance features safely", {
  # Constant feature in column 2 -> pooled variance 0 -> skipped.
  a <- matrix(c(0, 2, 5, 5), ncol = 2)
  b <- matrix(c(8, 10, 5, 5), ncol = 2)
  D <- divergence_coefficient(a, b)
  expect_equal(D, 16)
})

test_that("divergence_coefficient errors on mismatched feature counts", {
  a <- matrix(rnorm(20), ncol = 2)
  b <- matrix(rnorm(30), ncol = 3)
  expect_error(divergence_coefficient(a, b), "same number of features")
})
