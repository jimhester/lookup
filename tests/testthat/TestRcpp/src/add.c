#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP add_call_impl(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_ScalarReal(1));
  REAL(out)[0] = REAL(x)[0] + REAL(y)[0];
  UNPROTECT(1);
  return out;
}

SEXP add_external_impl(SEXP args) {
  SEXP x, y;

  SEXP out = PROTECT(Rf_ScalarReal(1));
  x = PROTECT(Rf_coerceVector(CADR(args), REALSXP));
  y = PROTECT(Rf_coerceVector(CADDR(args), REALSXP));

  REAL(out)[0] = REAL(x)[0] + REAL(y)[0];
  UNPROTECT(3);
  return out;
}

void add_c_impl(double *x, double *y, double *ans) {
  ans[0] = x[0] + y[0];
}
