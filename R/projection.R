project <- function(fc, W, Phi, p) {
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  m <- ncol(fc) - nrow(Phi)
  proj_fc <- mapply(\(p, W){
    C <- block(C_all, p, m+p)
    WtC <- tcrossprod(W, C)
    tbf <- fc[,seq_len(m+p), drop = FALSE]
    t((t(tbf)-tcrossprod(WtC, t(solve(C %*% WtC, tcrossprod(C, tbf)))))[seq_len(m), , drop = FALSE])
  },
  p = p,
  W = W,
  SIMPLIFY = FALSE)
  names(proj_fc) <- p
  proj_fc
}

block <- function(mat, m, n = m){
  mat[seq_len(m), seq_len(n), drop = FALSE]
}

get_W <- function(res_ori, res_com, p) {
  m <- NCOL(res_ori)
  res <- cbind(res_ori, res_com)
  res <- res[!apply(res, 1, anyNA),]
  lapply(
    p,
    function(pp) corpcor::cov.shrink(res[,seq_len(m+pp)], verbose = FALSE))
}
