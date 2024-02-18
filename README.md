
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flap

<!-- badges: start -->
<!-- badges: end -->

The goal of `flap` is to provide the Forecast Linear Augmented
Projection method that can reduce forecast variance by adjusting the
forecasts of multivariate time series to be consistent with the
forecasts of linear combinations (components) of the series by
projecting all forecasts onto the space where the linear constraints are
satisfied.

## Installation

You can install the development version of `flap` like so:

``` r
# install.packages("remotes")
remotes::install_github("FinYang/flap")
```

## Example

This is a basic example which shows you how to solve a common problem:

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
fc <- vapply(mdl, \(mdl) forecast::forecast(mdl, h=12)$mean, 
             FUN.VALUE = numeric(12))
res <- vapply(mdl, residuals, 
              FUN.VALUE = numeric(nrow(fpp2::visnights)))

# Obtain components and their forecasts and residuals
pca <- stats::prcomp(fpp2::visnights, center = FALSE, scale. = FALSE)
mdl_comp <- apply(pca$x, 2, forecast::ets)
fc_comp <- vapply(mdl_comp, \(mdl) forecast::forecast(mdl, h=12)$mean, 
                  FUN.VALUE = numeric(12))
res_comp <- vapply(mdl_comp, residuals, 
                   FUN.VALUE = numeric(nrow(pca$x)))
Phi <- t(pca$rotation)

# flap!
proj_fc <- flap(fc, fc_comp, Phi, res, res_comp)

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