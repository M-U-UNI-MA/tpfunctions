// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// SeqInVec
NumericVector SeqInVec(NumericVector myVector, NumericVector mySequence);
RcppExport SEXP _tpfuns_SeqInVec(SEXP myVectorSEXP, SEXP mySequenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type myVector(myVectorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mySequence(mySequenceSEXP);
    rcpp_result_gen = Rcpp::wrap(SeqInVec(myVector, mySequence));
    return rcpp_result_gen;
END_RCPP
}
// SeqInVecOpt
NumericVector SeqInVecOpt(NumericVector myVector, NumericVector mySequence);
RcppExport SEXP _tpfuns_SeqInVecOpt(SEXP myVectorSEXP, SEXP mySequenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type myVector(myVectorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mySequence(mySequenceSEXP);
    rcpp_result_gen = Rcpp::wrap(SeqInVecOpt(myVector, mySequence));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tpfuns_SeqInVec", (DL_FUNC) &_tpfuns_SeqInVec, 2},
    {"_tpfuns_SeqInVecOpt", (DL_FUNC) &_tpfuns_SeqInVecOpt, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tpfuns(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
