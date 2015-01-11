#' Numeric integer vectors
#' 
#' Test for objects of type \code{"numeric"} and \code{"integer"}.
#' 
#' @param x Object to be tested.
#' 
#' @return
#' \code{TRUE} or \code{FALSE} depending on whether \code{x} is a numeric
#' integer or not.
#' 
#' @seealso \code{\link[base]{is.numeric}}, \code{\link[base]{is.integer}}
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @export
#' @examples
#' x <- 1:10
#' is.numint(x)
#' x <- as.numeric(x)
#' is.numint(x)
#' x <- c(1.1, 1, 1, 1, 2)
#' is.numint(x)
# FUNCTION #####################################################################
is.numint <-
  function (x) {
    if (is.integer(x) || is.factor(x) || is.character(x)) return (FALSE)
    x <- ifelse(round(x, digits = 0) == x, TRUE, FALSE)
    ifelse(length(unique(x)) == 1, TRUE, FALSE)
  }
