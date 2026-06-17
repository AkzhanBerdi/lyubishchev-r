# lyubishchev 0.1.0

* Initial CRAN release.
* `divergence_coefficient()`: Lyubishchev's standardised separation
  coefficient D for two groups on continuous features.
* `scatter_ellipse()`: fits per-class covariance ellipses (mean, covariance,
  sample size).
* `transgression()`: detects ellipse overlap via squared Mahalanobis distance
  against a chi-squared threshold.
* `classify()`: Edgeworth-Pearson multivariate posterior classification of a
  new specimen with a numerically stable softmax.
