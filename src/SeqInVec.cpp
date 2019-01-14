#include <Rcpp.h>
using namespace Rcpp;
//' Find Sequence in Integer Vector
//'
//' @param vec
//' An integer vecotr
//' @param seq
//' The sequence
//'
//' @useDynLib tpfuns, .registration = TRUE
//' @importFrom Rcpp sourceCpp
//'
//' @export
// [[Rcpp::export]]
NumericVector SeqInVec(NumericVector myVector, NumericVector mySequence) {

  int vecSize = myVector.size();
  int seqSize = mySequence.size();
  NumericVector comparison(seqSize);
  NumericVector res(vecSize);
  int foundCounter = 0;

  for (int i = 0; i < vecSize; i++ ) {

    for (int j = 0; j < seqSize; j++ ) {
      comparison[j] = mySequence[j] == myVector[i + j];
    }

    if (sum(comparison) == seqSize) {
      for (int j = 0; j < seqSize; j++ ) {
        res[foundCounter] = i + j + 1;
        foundCounter++;
      }
    }
  }

  IntegerVector idx = seq(0, (foundCounter-1));
  return res[idx];
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
