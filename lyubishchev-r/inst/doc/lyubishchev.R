## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(lyubishchev)

## -----------------------------------------------------------------------------
setosa <- iris[iris$Species == "setosa", 1:4]
versicolor <- iris[iris$Species == "versicolor", 1:4]

divergence_coefficient(setosa, versicolor)

## -----------------------------------------------------------------------------
ellipses <- scatter_ellipse(iris[, 1:4], iris$Species)

ellipses[["setosa"]]$mean
ellipses[["setosa"]]$cov
ellipses[["setosa"]]$n_samples

## -----------------------------------------------------------------------------
transgression(ellipses, "versicolor", "virginica")

## -----------------------------------------------------------------------------
transgression(ellipses, "setosa", "virginica")

## -----------------------------------------------------------------------------
specimen <- c(5.1, 3.5, 1.4, 0.2)
result <- classify(specimen, ellipses)

sapply(result, function(r) r$posterior)

