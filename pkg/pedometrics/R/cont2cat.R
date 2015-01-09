#' Continuous to categorical
#' 
#' Convert continuous data into categorical data.
#' 
#' @param x Data frame or matrix with the continuous data to be converted
#' into categorical data (factors).
#' 
#' @param breaks List with the lower and upper limits to be used to break the
#' continuous data. Using a list allows breaking the data into a
#' different number of classes.
#' 
#' @return A data frame or matrix with the categorical data.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[Hmisc]{cut2}}
#' @import Hmisc
#' @examples
#' require(Hmisc)
#' # Matrix
#' x <- c(1:80)
#' x <- cbind(x, x)
#' breaks <- list(c(1, 10, 20, 40, 80), c(1, 50, 100))
#' y <- cont2cat(x, breaks)
#' y
#' # Data frame
#' x <- c(1:80)
#' x <- data.frame(cbind(x, x))
#' breaks <- list(c(1, 10, 20, 40, 80), c(1, 50, 100))
#' y <- cont2cat(x, breaks)
#' y
# MAIN FUNCTION ################################################################
cont2cat <-
  function (x, breaks) {
    cl <- class(x)
    n_col <- ncol(x)
    for (i in 1:n_col) {
      x[, i] <- cut2(x[, i], breaks[[i]])
    }
    x <- lapply(x, as.integer)
    if (cl == "data.frame") x <- as.data.frame(x)
    if (cl == "matrix") x <- matrix(x, ncol = n_col)
    return (x)
  }
