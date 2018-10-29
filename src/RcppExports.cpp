// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/mtrx.h"
#include <Rcpp.h>

using namespace Rcpp;

// mtrx_add
xt::rarray<double> mtrx_add(xt::rarray<double> x, xt::rarray<double> y);
RcppExport SEXP _mtrx_mtrx_add(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< xt::rarray<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< xt::rarray<double> >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mtrx_add(x, y));
    return rcpp_result_gen;
END_RCPP
}
// mtrx_subtract
xt::rarray<double> mtrx_subtract(xt::rarray<double> x, xt::rarray<double> y);
RcppExport SEXP _mtrx_mtrx_subtract(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< xt::rarray<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< xt::rarray<double> >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mtrx_subtract(x, y));
    return rcpp_result_gen;
END_RCPP
}
// mtrx_multiply
xt::rarray<double> mtrx_multiply(xt::rarray<double> x, xt::rarray<double> y);
RcppExport SEXP _mtrx_mtrx_multiply(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< xt::rarray<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< xt::rarray<double> >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mtrx_multiply(x, y));
    return rcpp_result_gen;
END_RCPP
}
// mtrx_divide
xt::rarray<double> mtrx_divide(xt::rarray<double> x, xt::rarray<double> y);
RcppExport SEXP _mtrx_mtrx_divide(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< xt::rarray<double> >::type x(xSEXP);
    Rcpp::traits::input_parameter< xt::rarray<double> >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mtrx_divide(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mtrx_mtrx_add", (DL_FUNC) &_mtrx_mtrx_add, 2},
    {"_mtrx_mtrx_subtract", (DL_FUNC) &_mtrx_mtrx_subtract, 2},
    {"_mtrx_mtrx_multiply", (DL_FUNC) &_mtrx_mtrx_multiply, 2},
    {"_mtrx_mtrx_divide", (DL_FUNC) &_mtrx_mtrx_divide, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_mtrx(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}