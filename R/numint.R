#' Tests for data types
#' 
#' Evaluate the data type contained in an object.
#' 
#' @param x Object to be tested.
#' 
#' @return
#' \code{TRUE} or \code{FALSE} depending on whether \code{x} contains a given
#' data type.
#' 
#' @seealso \code{\link[base]{is.numeric}}, \code{\link[base]{is.integer}},
#' \code{\link[base]{is.factor}}.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @aliases is.numint is.all.numint is.all.integer is.all.factor is.any.factor
#' is.all.numeric is.one.type
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
# FUNCTION - IS NUMERIC INTEGER? ###############################################
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
# FUNCTION - ARE ALL NUMERIC INTEGERS? #########################################
#' @rdname is.numint
#' @export
is.all.numint <-
  function (x) {
    res <- sapply(x, is.numint)
    res <- all(res == TRUE)
    return (res)
  }
# FUNCTION - ARE ALL INTEGERS? #################################################
#' @rdname is.numint
#' @export
is.all.integer <-
  function (x) {
    res <- sapply(x, is.integer)
    res <- all(res == TRUE)
    return (res)
  }
# FUNCTION - ARE ALL FACTORS? ##################################################
#' @rdname is.numint
#' @export
is.all.factor <-
  function (x) {
    res <- sapply(x, is.factor)
    res <- all(res == TRUE)
    return (res)
  }
# FUNCTION - IS ANY FACTOR? ####################################################
#' @rdname is.numint
#' @export
is.any.factor <-
  function (x) {
    res <- sapply(x, is.factor)
    res <- any(res == TRUE)
    return (res)
  }
# FUNCTION - ARE ALL NUMERIC ###################################################
#' @rdname is.numint
#' @export
is.all.numeric <-
  function (x) {
    res <- sapply(x, is.numeric)
    res <- all(res == TRUE)
    return (res)
  }
# FUNCTION - IS ONE TYPE #######################################################
#' @rdname is.numint
#' @export
is.one.type <-
  function (x) {
    res <- sapply(x, class)
    res <- length(unique(res))
    res <- ifelse(res == 1, TRUE, FALSE)
    return (res)
  }
