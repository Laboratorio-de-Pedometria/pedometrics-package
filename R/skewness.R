#' Moment coefficient of skewness
#' 
#' @description 
#' Compute the moment coefficient of skewness of a continuous, possibly non-normal variable.
#' 
#' @param x Numeric vector, the values of the variable of interest.
#' 
#' @return
#' A numerical value: the moment coefficient of skewness of `x`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @references
#' B. S. Everitt, _The Cambridge Dictionary of Statistics_, 3rd ed. Cambridge: Cambridge University
#' Press, 2006, p. 432.
#' 
#' D. N. Joanes and C. A. Gill, Comparing measures of sample skewness and kurtosis, _J Royal
#' Statistical Soc D_, vol. 47, no. 1, pp. 183–189, Mar. 1998, doi: 10.1111/1467-9884.00122.
#' 
#' H. Cramér, _Mathematical Methods of Statistics_. Princeton: Princeton University Press, 1946,
#' p. 575.
#' 
#' @examples 
#' x <- rlnorm(10)
#' skw <- skewness(x)
# FUNCTION #########################################################################################
#' @export
skewness <-
  function(x) {
    # Check function arguments
    if (!is.numeric(x)) {
      stop("'x' should be a numeric vector")
    }
    # Compute the moment coefficient of skewness
    n <- length(x)
    mean_x <- mean(x)
    mean_second_moment <- sum((x - mean_x)^2) / n
    mean_third_moment <- sum((x - mean_x)^3) / n
    skewness <- mean_third_moment / mean_second_moment^1.5
    return(skewness)
  }