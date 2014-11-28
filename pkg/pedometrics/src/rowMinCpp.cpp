/*******************************************************************************
file pedometrics/src/rowMinCpp.cpp

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 or 3 of the License
(at your option).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License is available at
http://www.r-project.org/Licenses/

*******************************************************************************/
//' Return the minimum value in each row of a numeric matrix
//' 
//' This function returns the minimum value in each row of a numeric matrix.
//' 
//' This function is implemented in C++ to speed-up the computation time for
//' large matrices.
//' 
//' @param x Numeric matrix with two or more rows and/or columns.
//' @return A numeric vector with the minimum value of each row if the matrix.
//' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
//' @seealso \code{\link[matrixStats]{rowMins}}
//' @keywords univar
//' @import Rcpp
//' @export
//' @examples
//' 
//' x <- matrix(rnorm(20), nrow = 5)
//' rowMinCpp(x)
//' 

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export)]]

NumericVector rowMinCpp(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow), xi(ncol);
  
  /* begin the main loop over the rows */
  for (int i = 0; i < nrow; i++) {
    
    /* begin secondary loop over columns */
    /* every j-th element of the i-th row is copied to the 'xi' */
    for (int j = 0; j < ncol; j++) {
      xi[j] = x(i, j);
    }
    
    /* Return the minimum value in 'xi' and copy it to the i-th */
    /* element of 'out'. It can be extended to return other statistics. */
    out[i] = min(xi);
  }
  return out;
}
/* End */