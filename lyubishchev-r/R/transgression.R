#' Detect Overlap (Transgression) Between Two Scatter Ellipses
#'
#' Tests whether two class scatter ellipses overlap, in Lyubishchev's sense of
#' "transgression" between groups. The centroids are compared using the squared
#' Mahalanobis distance under the pooled covariance of the two classes, and that
#' distance is compared against a chi-squared threshold with degrees of freedom
#' equal to the number of features. When the Mahalanobis distance is below the
#' threshold the groups are deemed to transgress (overlap).
#'
#' @param ellipses A named list of scatter ellipses as returned by
#'   \code{\link{scatter_ellipse}}.
#' @param class_a Name (character) of the first class in \code{ellipses}.
#' @param class_b Name (character) of the second class in \code{ellipses}.
#' @param confidence Confidence level for the chi-squared threshold, between 0
#'   and 1. Defaults to 0.95.
#'
#' @return A list with components:
#'   \describe{
#'     \item{mahalanobis_distance}{Squared Mahalanobis distance between the two
#'       centroids under the pooled covariance.}
#'     \item{threshold}{Chi-squared threshold at the requested confidence with
#'       degrees of freedom equal to the number of features.}
#'     \item{transgression}{Logical; \code{TRUE} when the distance is below the
#'       threshold (the ellipses overlap).}
#'     \item{separation_ratio}{Ratio of the Mahalanobis distance to the
#'       threshold. Values above 1 indicate well-separated groups.}
#'   }
#'
#' @references
#' Lubischew, A.A. (1962). On the use of discriminant functions in taxonomy.
#' Biometrics, 18(4), 455-477.
#'
#' @examples
#' ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)
#' transgression(ellipses, "versicolor", "virginica")
#'
#' @seealso \code{\link{scatter_ellipse}}
#' @export
transgression <- function(ellipses, class_a, class_b, confidence = 0.95) {
  ea <- ellipses[[class_a]]
  eb <- ellipses[[class_b]]
  if (is.null(ea) || is.null(eb)) {
    stop("Both class_a and class_b must be names present in 'ellipses'.")
  }
  n_features <- length(ea$mean)
  na <- ea$n_samples
  nb <- eb$n_samples
  # Pooled covariance weighted by degrees of freedom.
  pooled_cov <- ((na - 1) * ea$cov + (nb - 1) * eb$cov) / (na + nb - 2)
  # R's mahalanobis() returns the SQUARED distance.
  dist <- stats::mahalanobis(ea$mean, eb$mean, pooled_cov)
  threshold <- stats::qchisq(confidence, df = n_features)
  list(
    mahalanobis_distance = as.numeric(dist),
    threshold = threshold,
    transgression = dist < threshold,
    separation_ratio = as.numeric(dist) / threshold
  )
}
