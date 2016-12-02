#' @useDynLib TestRcpp
#' @importFrom Rcpp sourceCpp
NULL

#' @export
#' @useDynLib TestRcpp add_call_impl
add_call <- function(x, y) {
  .Call(add_call_impl, x, y)
}

#' @export
#' @useDynLib TestRcpp add_external_impl
add_external <- function(x, y) {
  .External(add_external_impl, x, y)
}

#' @export
#' @useDynLib TestRcpp add_c_impl
add_c <- function(x, y) {
  .C("add_c_impl", as.double(x), as.double(y), ans = double(1))$ans
}

#' @export
test_S3 <- function(x) UseMethod("test_S3")

#' @export
setGeneric("test_S4", function(x) standardGeneric("test_S4"))
