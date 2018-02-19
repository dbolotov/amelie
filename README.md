
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/dbolotov/anode.svg?branch=master)](https://travis-ci.org/dbolotov/anode)[![codecov](https://codecov.io/github/codecov/example-r/branch/master/graphs/badge.svg)](https://codecov.io/github/codecov/example-r)

About
-----

`amelie` implements anomaly detection with maximum likelihood estimates and normal probability functions. The package follows the approach described in Andrew Ng's [course on machine learning](https://www.coursera.org/learn/machine-learning).

Current version: 0.0.1

Installation
------------

``` r
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("dbolotov/amelie")
```

Example
-------

``` r
library(amelie)

x1 <- c(1,.2,3,1,1,.7,-2,-1)
x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
x <- do.call(cbind,list(x1,x2))
y <- c(0,0,0,0,0,0,1,1)
dframe <- data.frame(x,y)
df_fit <- ad(y ~ x1 + x2, dframe)
```
