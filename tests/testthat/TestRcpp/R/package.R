#' @useDynLib TestRcpp
#' @importFrom Rcpp sourceCpp
NULL

#' @useDynLib TestRcpp add_c_impl
add_c <- function(x, y) {
  .Call(add_c_impl, x, y)
}
