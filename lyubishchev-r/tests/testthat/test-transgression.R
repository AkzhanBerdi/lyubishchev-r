test_that("transgression returns the documented structure", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- transgression(ellipses, "setosa", "virginica")
  expect_named(res, c("mahalanobis_distance", "threshold",
                      "transgression", "separation_ratio"))
  expect_type(res$transgression, "logical")
  expect_type(res$mahalanobis_distance, "double")
})

test_that("well-separated groups do not transgress", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- transgression(ellipses, "setosa", "virginica")
  expect_false(res$transgression)
  expect_gt(res$separation_ratio, 1)
})

test_that("identical groups transgress", {
  X <- rbind(
    matrix(rnorm(40), ncol = 2),
    matrix(rnorm(40), ncol = 2)
  )
  set.seed(1)
  base <- matrix(rnorm(40), ncol = 2)
  X <- rbind(base, base)
  y <- c(rep("a", 20), rep("b", 20))
  ellipses <- scatter_ellipse(X, y)
  res <- transgression(ellipses, "a", "b")
  expect_true(res$transgression)
})

test_that("transgression threshold respects confidence and df", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  res <- transgression(ellipses, "setosa", "versicolor", confidence = 0.95)
  expect_equal(res$threshold, stats::qchisq(0.95, df = 4))
})

test_that("transgression errors on unknown class", {
  ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
  expect_error(transgression(ellipses, "setosa", "nope"),
               "must be names present")
})
