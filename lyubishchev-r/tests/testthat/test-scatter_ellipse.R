test_that("scatter_ellipse returns one entry per class", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  expect_length(ellipses, 3)
  expect_setequal(names(ellipses), levels(iris$Species))
})

test_that("scatter_ellipse entries have correct structure", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  e <- ellipses[["setosa"]]
  expect_named(e, c("mean", "cov", "n_samples"))
  expect_length(e$mean, 4)
  expect_equal(dim(e$cov), c(4, 4))
  expect_type(e$n_samples, "integer")
  expect_equal(e$n_samples, 50L)
})

test_that("scatter_ellipse means match base R group means", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  expected <- colMeans(iris[iris$Species == "setosa", 1:4])
  expect_equal(ellipses[["setosa"]]$mean, expected)
})

test_that("scatter_ellipse handles single feature (1D)", {
  X <- matrix(c(1, 2, 3, 10, 11, 12), ncol = 1)
  y <- c("a", "a", "a", "b", "b", "b")
  ellipses <- scatter_ellipse(X, y)
  expect_length(ellipses, 2)
  expect_equal(dim(ellipses[["a"]]$cov), c(1, 1))
})

test_that("scatter_ellipse handles single sample per class", {
  X <- matrix(c(1, 2, 9, 10), ncol = 2, byrow = TRUE)
  y <- c("a", "b")
  ellipses <- scatter_ellipse(X, y)
  expect_equal(ellipses[["a"]]$n_samples, 1L)
  # With one sample covariance falls back to a zero matrix.
  expect_equal(ellipses[["a"]]$cov, matrix(0, 2, 2))
})

test_that("scatter_ellipse errors on mismatched lengths", {
  expect_error(scatter_ellipse(iris[, 1:4], iris$Species[1:10]),
               "length\\(y\\) must equal nrow\\(X\\)")
})
