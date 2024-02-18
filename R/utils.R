list2array <- function(xlist){
  d1 <- unique(vapply(xlist, NROW, numeric(1)))
  if(length(d1) != 1) stop("Different row number")
  d2 <- unique(vapply(xlist, NCOL, numeric(1)))
  if(length(d2) != 1) stop("Different col number")
  array(unlist(xlist), dim = (c(d1, d2, length(xlist))))
}

array2list <- function(xarray){
  out <- vector("list", length= dim(xarray)[[3]])
  for(i in seq_len(dim(xarray)[[3]])) {
    out[[i]] <- xarray[,,i]
  }
  out
}
