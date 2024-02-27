
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flap

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/flap)](https://CRAN.R-project.org/package=flap)
[![R-CMD-check](https://github.com/FinYang/flap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinYang/flap/actions/workflows/R-CMD-check.yaml)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

The goal of `flap` is to provide the Forecast Linear Augmented
Projection method that can reduce forecast variance by adjusting the
forecasts of multivariate time series to be consistent with the
forecasts of linear combinations (components) of the series by
projecting all forecasts onto the space where the linear constraints are
satisfied.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=flap).

``` r
install.packages("flap")
```

You can install the **development** version from
[Github](https://github.com/FinYang/flap)

``` r
# install.packages("remotes")
remotes::install_github("FinYang/flap")
```

## Example

This is a basic workflow to flap:

``` r
## The following pacakges are required to run this example
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("fpp2")

library(flap)
library(tidyr)
library(ggplot2)

# Obtain the forecast and the residual of the original series
mdl <- apply(fpp2::visnights, 2, forecast::ets)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
fc <- vapply(mdl, function(mdl) forecast::forecast(mdl, h=12)$mean, 
             FUN.VALUE = numeric(12))
res <- vapply(mdl, residuals, 
              FUN.VALUE = numeric(nrow(fpp2::visnights)))

# Obtain components and their forecasts and residuals
pca <- stats::prcomp(fpp2::visnights, center = FALSE, scale. = FALSE)
mdl_comp <- apply(pca$x, 2, forecast::ets)
fc_comp <- vapply(mdl_comp, function(mdl) forecast::forecast(mdl, h=12)$mean, 
                  FUN.VALUE = numeric(12))
res_comp <- vapply(mdl_comp, residuals, 
                   FUN.VALUE = numeric(nrow(pca$x)))
Phi <- t(pca$rotation)

# flap!
proj_fc <- flap(fc, fc_comp, Phi, res, res_comp)
proj_fc
#> Forecast Linear Augmented Projection
#> A named list of numeric matrices of projected forecasts
#> ------------
#>  Num. of Series:            m = 20
#>  Num. of Components:        p = 1-20
#>  Num. of Forecast Horizons: 12
#> ------------
#> List of 20
#>  $ 1 : num [1:12, 1:20] 7.8 7.91 ...
#>  $ 2 : num [1:12, 1:20] 7.64 7.76 ...
#>  $ 3 : num [1:12, 1:20] 7.64 7.78 ...
#>  $ 4 : num [1:12, 1:20] 7.39 7.48 ...
#>  $ 5 : num [1:12, 1:20] 7.39 7.49 ...
#>   [list output truncated]

# Plot
if(interactive()) {
  proj_fc %>% 
    as.data.frame() %>% 
    pivot_longer(!c(h, p)) %>% 
    ggplot(aes(x = h, y = value, colour = p, group = p)) +
    geom_line() +
    facet_wrap("name", scales = "free_y")
}
```
