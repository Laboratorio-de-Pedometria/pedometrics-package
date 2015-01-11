#' Continuous to categorical
#' 
#' Convert continuous data (numeric) into categorical data (factor or integer).
#' 
#' @param x Vector, data frame or matrix with the continuous data to be 
#' converted into categorical data.
#' 
#' @param breaks Vector or list with the lower and upper limits to be used to 
#' break the continuous data into classes.
#' 
#' @details
#' Breaks must be a vector if \code{x} is a vector, but a list if \code{x} is a
#' data frame or matrix. Using a list allows breaking the data into a different
#' number of classes.
#' 
#' @return
#' A vector, data frame, or matrix, depending on the class of \code{x}.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[Hmisc]{cut2}}
#' @export
#' @import Hmisc
#' @examples
#' require(Hmisc)
#' # Matrix
#' x <- c(1:10)
#' x <- cbind(x, x)
#' breaks <- list(c(1, 2, 4, 8, 10), c(1, 5, 10))
#' y <- cont2cat(x, breaks)
#' y
#' # Data frame
#' x <- c(1:10)
#' x <- data.frame(cbind(x, x))
#' breaks <- list(c(1, 2, 4, 8, 10), c(1, 5, 10))
#' y <- cont2cat(x, breaks)
#' y
#' # Vector
#' x <- c(1:10)
#' breaks <- c(1, 2, 4, 8, 10)
#' y <- cont2cat(x, breaks)
#' y
# MAIN FUNCTION ################################################################
cont2cat <-
  function (x, breaks) {
    if (is.vector(x) && is.vector(breaks)) {
      x <- cut2(x, breaks)
    } else {
      n_col <- ncol(x)
      for (i in 1:n_col) {
        x[, i] <- cut2(x[, i], breaks[[i]])
      }
    }
    return (x)
  }
