#' Forecast Linear Augmented Projection
#'
#' Reduces forecast variance by adjusting the forecasts of multivariate time
#' series to be consistent with the forecasts of linear combinations (components)
#' of the series by projecting all forecasts onto the space where the linear
#' constraints are satisfied.
#'
#' @param fc An \eqn{h} by \eqn{m} matrix of base forecasts of the original
#' series to be projected. \eqn{h} is the forecast horizon and \eqn{m} is the
#' total number of series.
#' @param fc_comp An \eqn{h} by \eqn{p} matrix of base forecasts of the components
#' used in the projection. \eqn{h} is the forecast horizon and \eqn{p} is the
#' total number of components.
#' @param Phi A \eqn{p} by \eqn{m} weight matrix mapping the original series into
#' the components such that \eqn{c_t = \Phi z_t} where \eqn{c_t} is the vector of
#' components and \eqn{z_t} is the vector of original series.
#' @param res A \eqn{T} by \eqn{m} (in-sample) forecast residual matrix of the
#' original series.
#' @param res_comp A \eqn{T} by \eqn{p} (in-sample) forecast residual matrix of
#' the components.
#' @param p The number of components to use in the projection. The default is
#' trying all the possible number of components capped at the number provided in
#' the forecast.
#'
#' @return A list of class \code{flap} with each element containing a \eqn{h} by
#' \eqn{m} matrix of projected forecast of the original series for the corresponding
#' number of components \code{p}.
#'
#' @examples
#' # Generate example data
#' # T = 70, m = 20
#' train <- matrix(rnorm(70 * 20),ncol = 20)
#'
#' # Obtain the forecast and the residual of the original series
#' mdl <- apply(train, 2, forecast::ets)
#' fc <- vapply(mdl, function(mdl) forecast::forecast(mdl, h=12)$mean,
#'              FUN.VALUE = numeric(12))
#' res <- vapply(mdl, residuals, FUN.VALUE = numeric(70))
#'
#' # Obtain components and their forecasts and residuals
#' pca <- stats::prcomp(train, center = FALSE, scale. = FALSE)
#' mdl_comp <- apply(pca$x, 2, forecast::ets)
#' fc_comp <- vapply(mdl_comp, function(mdl) forecast::forecast(mdl, h=12)$mean,
#'                   FUN.VALUE = numeric(12))
#' res_comp <- vapply(mdl_comp, residuals,
#'                    FUN.VALUE = numeric(nrow(pca$x)))
#' Phi <- t(pca$rotation)
#'
#' # flap!
#' flap(fc, fc_comp, Phi, res, res_comp)
#'
#' @export
flap <- function(fc, fc_comp, Phi, res, res_comp,
                 p = seq_len(ncol(fc_comp))) {
  W <- get_W(res, res_comp, p)
  proj_fc <- project(
    cbind(fc,
          fc_comp),
    W = W,
    Phi = Phi,
    p = p)
  class(proj_fc) <- c("flap", class(proj_fc))
  proj_fc
}

#' @export
as.data.frame.flap <- function(x, row.names = NULL, optional = FALSE, ...){
  mat <- do.call(rbind, x)
  df <- as.data.frame(mat)
  df$p <- rep(as.integer(names(x)), each = nrow(x[[1]]))
  df$h <- rep(seq_len(nrow(x[[1]])), times = length(x))
  df
}
