#' Lyubishchev's Divergence Coefficient
#'
#' Computes Lyubishchev's divergence coefficient \eqn{D} between two groups
#' measured on one or more continuous features. The coefficient summarises the
#' standardised separation between the group means, summed across features:
#' \deqn{D = \sum_j \frac{(M_{1j} - M_{2j})^2}{\sigma_{1j}^2 + \sigma_{2j}^2}}
#' where \eqn{M_{ij}} and \eqn{\sigma_{ij}^2} are the mean and (sample) variance
#' of feature \eqn{j} in group \eqn{i}. Features whose pooled variance is zero
#' are skipped to avoid division by zero.
#'
#' This is the measure described in Lyubishchev's 1943 manuscript and later
#' published in English by Lubischew (1962). It predates and is more general
#' than the binary-character similarity coefficients of Sokal and Sneath
#' (1963), operating directly on continuous measurements.
#'
#' @param a A numeric matrix or data frame for the first group, with one row
#'   per observation and one column per feature. A numeric vector is treated as
#'   a single-feature group.
#' @param b A numeric matrix or data frame for the second group, with the same
#'   columns (features) as \code{a}.
#'
#' @return A single numeric value, the divergence coefficient \eqn{D}. Larger
#'   values indicate greater separation between the groups.
#'
#' @references
#' Lyubishchev, A.A. (1943). Programma obshchey sistematiki
#' [Program of General Systematics]. Manuscript, 22 November 1943.
#'
#' Lubischew, A.A. (1962). On the use of discriminant functions in taxonomy.
#' Biometrics, 18(4), 455-477.
#'
#' @examples
#' setosa <- as.matrix(iris[iris$Species == "setosa", 1:4])
#' versicolor <- as.matrix(iris[iris$Species == "versicolor", 1:4])
#' divergence_coefficient(setosa, versicolor)
#'
#' @export
divergence_coefficient <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  if (ncol(a) != ncol(b)) {
    stop("'a' and 'b' must have the same number of features (columns).")
  }
  mean_a <- colMeans(a)
  mean_b <- colMeans(b)
  var_a <- apply(a, 2, stats::var)
  var_b <- apply(b, 2, stats::var)
  pooled_var <- var_a + var_b
  mask <- pooled_var > 0
  if (!any(mask)) {
    return(0)
  }
  sum((mean_a[mask] - mean_b[mask])^2 / pooled_var[mask])
}
