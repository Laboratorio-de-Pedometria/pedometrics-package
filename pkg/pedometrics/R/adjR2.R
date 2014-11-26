#  file pedometrics/R/adjR2.R
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
# DOCUMENTATION ################################################################
#' 
#' @title Adjusted coefficient of determination
#' 
#' @description
#' Calculates the adjusted coefficient of determination of a multiple linear 
#' regression model.
#' 
#' @param r2 Numeric vector with the coeficient of deternmination to be 
#' adjusted.
#' @param n Numeric vector providing the number of observations used to fit the
#' multiple linear regression model.
#' @param p Numeric vector providing the number of parameters included in the
#' multiple linear regression model.
#' 
#' @details Details will be added later.
#' 
#' @return A numeric vector with the adjusted coefficient of determination.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
#' 
#' @references Coefficient of determination. Wikipedia, The Free Encyclopedia.
#' Available at \url{http://en.wikipedia.org/wiki/Coefficient_of_determination}.
#' [Online; accessed 31-July-2014].
#' 
#' @examples
#' adjR2(r2 = 0.95, n = 100, p = 80)
#' 
#' @keywords misc
#' 
# FUNCTION #####################################################################
#
adjR2 <- 
  function (r2, n, p) {
    r2 <- 1 - (1 - r2) * ((n - 1) / (n - p - 1))
    return(r2)
  }
#
# End!