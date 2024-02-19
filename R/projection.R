project <- function(fc, W, Phi, p) {
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  m <- ncol(fc) - nrow(Phi)
  proj_fc <- lapply(
    asplit(fc, 1), function(fc){
      mapply(function(p, W){
        C <- block(C_all, p, m+p)
        WtC <- tcrossprod(W, C)
        bf <- c(fc[seq_len(m+p)])
        (bf -tcrossprod(WtC, t(solve(C %*% WtC, C))) %*% bf)[seq_len(m),]
      },
      p = p,
      W = W,
      SIMPLIFY = FALSE)
    })

  proj_fc <- lapply(proj_fc, function(x) do.call(cbind, x))
  proj_fc <- list2array(proj_fc)
  proj_fc <- aperm(proj_fc, c(3, 1, 2))
  colnames(proj_fc) <- colnames(fc)[seq_len(m)]
  proj_fc <- array2list(proj_fc)
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
