#' @export
flap <- function(fc_base, fc_comp, Phi, res_base, res_comp,
                 p = seq_len(ncol(res_comp))) {
  W <- get_W(res_base, res_comp, p)
  proj_fc <- project(
    cbind(fc_base,
          fc_comp),
    W = W,
    Phi = Phi,
    p = p)
  class(proj_fc) <- c("flap", class(proj_fc))
  proj_fc
}
