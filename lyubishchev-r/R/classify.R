#' Classify a Specimen by Multivariate Posterior Probability
#'
#' Assigns posterior class probabilities to a new specimen using the
#' Edgeworth-Pearson multivariate Gaussian likelihood for each class scatter
#' ellipse. For each class the log-likelihood of the specimen under a
#' multivariate normal with the class mean and covariance is computed, and a
#' softmax over the per-class log-likelihoods yields posterior probabilities.
#'
#' The log-likelihood for class \eqn{k} is
#' \deqn{-\tfrac{1}{2}\left(p\log 2\pi + \log|\Sigma_k| + (x-\mu_k)^\top \Sigma_k^{-1} (x-\mu_k)\right)}
#' where \eqn{p} is the number of features, \eqn{\mu_k} and \eqn{\Sigma_k} are
#' the class mean and covariance, and \eqn{x} is the specimen.
#'
#' @param specimen A numeric vector of feature values for a single observation.
#' @param ellipses A named list of scatter ellipses as returned by
#'   \code{\link{scatter_ellipse}}.
#'
#' @return A named list with one element per class. Each element is a list with
#'   components:
#'   \describe{
#'     \item{mahalanobis_distance}{Squared Mahalanobis distance from the
#'       specimen to the class centroid.}
#'     \item{log_likelihood}{Multivariate Gaussian log-likelihood of the
#'       specimen under the class.}
#'     \item{posterior}{Posterior probability of the class (softmax over the
#'       per-class log-likelihoods). Posteriors sum to 1 across classes.}
#'   }
#'
#' @references
#' Lubischew, A.A. (1962). On the use of discriminant functions in taxonomy.
#' Biometrics, 18(4), 455-477.
#'
#' @examples
#' ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
#' specimen <- c(5.1, 3.5, 1.4, 0.2)
#' result <- classify(specimen, ellipses)
#' sapply(result, function(r) r$posterior)
#'
#' @seealso \code{\link{scatter_ellipse}}
#' @export
classify <- function(specimen, ellipses) {
  specimen <- as.numeric(specimen)
  class_names <- names(ellipses)

  log_lls <- numeric(length(class_names))
  maha <- numeric(length(class_names))
  names(log_lls) <- class_names
  names(maha) <- class_names

  for (cls in class_names) {
    e <- ellipses[[cls]]
    mean_k <- e$mean
    cov_k <- e$cov
    k <- length(specimen)
    diff <- specimen - mean_k
    cov_inv <- solve(cov_k)
    log_det <- as.numeric(determinant(cov_k, logarithm = TRUE)$modulus)
    quad <- as.numeric(t(diff) %*% cov_inv %*% diff)
    maha[cls] <- quad
    log_lls[cls] <- -0.5 * (k * log(2 * pi) + log_det + quad)
  }

  # Numerically stable softmax over log-likelihoods.
  m <- max(log_lls)
  weights <- exp(log_lls - m)
  posteriors <- weights / sum(weights)

  result <- vector("list", length(class_names))
  names(result) <- class_names
  for (cls in class_names) {
    result[[cls]] <- list(
      mahalanobis_distance = maha[[cls]],
      log_likelihood = log_lls[[cls]],
      posterior = posteriors[[cls]]
    )
  }
  result
}
