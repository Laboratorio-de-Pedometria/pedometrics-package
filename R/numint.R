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
#' # Vector of integers
#' x <- 1:10
#' is.numint(x) # FALSE
#' # Vector of numeric integers
#' x <- as.numeric(x)
#' is.numint(x) # TRUE
#' # Vector of numeric values
#' x <- c(1.1, 1, 1, 1, 2) # FALSE
#' is.numint(x)
#' # Single numeric integer
#' is.numint(1) # TRUE
#' # Single numeric value
#' is.numint(1.1) # FALSE
# FUNCTION #####################################################################
is.numint <-
  function (x) {
    if (is.integer(x) || is.factor(x) || is.character(x)) return (FALSE)
    if (is.numeric(x) && length(x) > 1) {
      res <- ifelse(round(x, digits = 0) == x, TRUE, FALSE)
      res <- ifelse(length(unique(res)) == 1, TRUE, FALSE)
    } else {
      res <- ifelse(round(x, digits = 0) == x, TRUE, FALSE)
    }
    return (res)
  }
