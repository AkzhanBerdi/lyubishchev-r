#' Fit Scatter Ellipses per Class
#'
#' Fits a covariance ellipse to each class in a labelled multivariate data set.
#' For every class the function computes the centroid (mean vector), the
#' feature covariance matrix and the sample size. These ellipses are the
#' building blocks for \code{\link{transgression}} and \code{\link{classify}}.
#'
#' @param X A numeric matrix or data frame of observations, with one row per
#'   observation and one column per feature.
#' @param y A vector of class labels of length \code{nrow(X)}. May be a factor,
#'   character or numeric vector.
#'
#' @return A named list with one element per class. Each element is itself a
#'   list with components:
#'   \describe{
#'     \item{mean}{Numeric vector of feature means for the class.}
#'     \item{cov}{Feature covariance matrix for the class.}
#'     \item{n_samples}{Integer count of observations in the class.}
#'   }
#'   The names of the list are the class labels (coerced to character).
#'
#' @references
#' Lubischew, A.A. (1962). On the use of discriminant functions in taxonomy.
#' Biometrics, 18(4), 455-477.
#'
#' @examples
#' ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
#' ellipses[["setosa"]]$mean
#' ellipses[["setosa"]]$n_samples
#'
#' @seealso \code{\link{transgression}}, \code{\link{classify}}
#' @export
scatter_ellipse <- function(X, y) {
  X <- as.matrix(X)
  if (length(y) != nrow(X)) {
    stop("length(y) must equal nrow(X).")
  }
  classes <- unique(y)
  ellipses <- vector("list", length(classes))
  names(ellipses) <- as.character(classes)
  for (cls in classes) {
    mask <- y == cls
    X_cls <- X[mask, , drop = FALSE]
    n <- nrow(X_cls)
    cov_cls <- if (n > 1) stats::cov(X_cls) else matrix(0, ncol(X_cls), ncol(X_cls))
    ellipses[[as.character(cls)]] <- list(
      mean = colMeans(X_cls),
      cov = cov_cls,
      n_samples = as.integer(n)
    )
  }
  ellipses
}
