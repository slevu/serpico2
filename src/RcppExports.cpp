// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// loop_C
NumericMatrix loop_C(NumericMatrix psw, List parms);
RcppExport SEXP _serpico2_loop_C(SEXP pswSEXP, SEXP parmsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type psw(pswSEXP);
    Rcpp::traits::input_parameter< List >::type parms(parmsSEXP);
    rcpp_result_gen = Rcpp::wrap(loop_C(psw, parms));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_serpico2_loop_C", (DL_FUNC) &_serpico2_loop_C, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_serpico2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
