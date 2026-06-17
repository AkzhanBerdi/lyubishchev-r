test_that("classify returns one entry per class with correct structure", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- classify(c(5.1, 3.5, 1.4, 0.2), ellipses)
  expect_length(res, 3)
  expect_named(res[["setosa"]],
               c("mahalanobis_distance", "log_likelihood", "posterior"))
})

test_that("posteriors are valid probabilities summing to one", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- classify(c(6.0, 3.0, 4.5, 1.5), ellipses)
  posteriors <- sapply(res, function(r) r$posterior)
  expect_equal(sum(posteriors), 1)
  expect_true(all(posteriors >= 0 & posteriors <= 1))
})

test_that("classify assigns a clear setosa specimen to setosa", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- classify(c(5.1, 3.5, 1.4, 0.2), ellipses)
  posteriors <- sapply(res, function(r) r$posterior)
  expect_equal(names(which.max(posteriors)), "setosa")
})

test_that("classify handles single feature (1D)", {
  X <- matrix(c(1, 2, 3, 10, 11, 12), ncol = 1)
  y <- c("a", "a", "a", "b", "b", "b")
  ellipses <- scatter_ellipse(X, y)
  res <- classify(2, ellipses)
  posteriors <- sapply(res, function(r) r$posterior)
  expect_equal(names(which.max(posteriors)), "a")
})

test_that("mahalanobis distance is non-negative", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- classify(c(5.8, 2.7, 5.1, 1.9), ellipses)
  d <- sapply(res, function(r) r$mahalanobis_distance)
  expect_true(all(d >= 0))
})
