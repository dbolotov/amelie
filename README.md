
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![travis-ci build status](https://travis-ci.org/dbolotov/amelie.svg?branch=master)](https://travis-ci.org/dbolotov/amelie) [![codecov status](https://codecov.io/gh/dbolotov/amelie/branch/master/graph/badge.svg)](https://codecov.io/gh/dbolotov/amelie) [![CRAN status](https://www.r-pkg.org/badges/version/amelie)](https://cran.r-project.org/package=amelie)

About
-----

`amelie` implements anomaly detection with maximum likelihood estimates and normal probability density functions. The package follows the approach described in Andrew Ng's [course on machine learning](https://www.coursera.org/learn/machine-learning).

Current CRAN version: [0.1.0](https://cran.r-project.org/web/packages/amelie/index.html)

Development version (this repository): 0.2.0

Installation
------------

### Release from CRAN

``` r
install.packages("amelie")
```

### Latest changes from GitHub

``` r
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

Documentation
-------------

-   [Introduction vignette](https://cran.r-project.org/web/packages/amelie/vignettes/amelie-introduction.html) for a description of the algorithm.
-   [Package site](https://dbolotov.github.io/amelie/) for package documentation (built with `pkgdown`).
