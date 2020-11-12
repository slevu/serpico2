#include <Rcpp.h>
using namespace Rcpp;

// C++ implementation of regression predictions


// [[Rcpp::export]]
NumericMatrix loop_C(NumericMatrix psw,
                      List parms){
  // access vectors from list
  NumericMatrix m = parms["beta"];
  NumericVector  beta1 = m( _ , 0);
  NumericVector  beta2 = m( _ , 1);
  NumericVector  alpha_a = parms["alpha_a"];
  NumericVector  sigma_a = parms["sigma_a"];
  NumericVector  alpha_r = parms["alpha_r"];
  NumericVector  sigma_r = parms["sigma_r"];
  NumericVector  alpha_t = parms["alpha_t"];
  NumericVector  sigma_t = parms["sigma_t"];

  // output declaration
  int nrow = psw.nrow(), ncol = beta1.length();
  NumericMatrix probs(nrow, ncol);
  double lc = 0; // \Beta X

  // loop (0-based indexing)
  for (int i = 0; i < nrow; i++){
    for (int j = 0; j < ncol; j++){
      int age = psw(i, 3);
      int reg = psw(i, 1);
      int time = psw(i, 0);
      int sex = psw(i, 2);
      lc = beta1(j) + 
        beta2(j) * sex +
        sigma_a(j) * alpha_a(j, age - 1) +
        sigma_r(j) * alpha_r(j, reg - 1) +
        sigma_t(j) * alpha_t(j, time - 1 );
      probs(i, j) =  1/(1 + exp( - lc ));
    }
  }
  return(probs);
}
