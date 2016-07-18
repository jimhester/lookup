#include <Rcpp.h>

// [[Rcpp::export]]
double add(double x, double y) {
  return x + y;
}

// [[Rcpp::export]]
double add_default(double x, double y = 1) {
  return x + y;
}
