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
#' @aliases is.numint all.numint any.numint all.integer any.integer all.factor
#' any.factor all.numeric any.numeric unique.class
#' @export
#' @examples
# Vector of integers
x <- 1:10
is.numint(x) # FALSE

# Vector of numeric integers
x <- as.numeric(x)
is.numint(x) # TRUE

# Vector of numeric values
x <- c(1.1, 1, 1, 1, 2)
is.numint(x) # FALSE
all.numint(x) # FALSE
any.numint(x) # TRUE
which.numint(x)

# Single numeric integer
is.numint(1) # TRUE

# Single numeric value
is.numint(1.1) # FALSE
# FUNCTION - NUMERIC INTEGERS ##################################################
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
#' @rdname is.numint
#' @export
all.numint <-
  function (x) {
    res <- sapply(x, is.numint)
    res <- all(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
any.numint <-
  function (x) {
    res <- sapply(x, is.numint)
    res <- any(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
which.numint <-
  function (x) {
    res <- sapply(x, is.numint)
    res <- which(res == TRUE)
    return (res)
  }
# FUNCTION - INTEGERS ##########################################################
#' @rdname is.numint
#' @export
all.integer <-
  function (x) {
    res <- sapply(x, is.integer)
    res <- all(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
any.integer <-
  function (x) {
    res <- sapply(x, is.integer)
    res <- any(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
which.integer <-
  function (x) {
    res <- sapply(x, is.integer)
    res <- which(res == TRUE)
    return (res)
  }
# FUNCTION - FACTORS ###########################################################
#' @rdname is.numint
#' @export
all.factor <-
  function (x) {
    res <- sapply(x, is.factor)
    res <- all(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
any.factor <-
  function (x) {
    res <- sapply(x, is.factor)
    res <- any(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
which.factor <-
  function (x) {
    res <- sapply(x, is.factor)
    res <- which(res == TRUE)
    return (res)
  }
# FUNCTION - NUMERIC ###########################################################
#' @rdname is.numint
#' @export
all.numeric <-
  function (x) {
    res <- sapply(x, is.numeric)
    res <- all(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
any.numeric <-
  function (x) {
    res <- sapply(x, is.numeric)
    res <- any(res == TRUE)
    return (res)
  }
#' @rdname is.numint
#' @export
which.numeric <-
  function (x) {
    res <- sapply(x, is.numeric)
    res <- which(res == TRUE)
    return (res)
  }
# FUNCTION - IS ONE TYPE #######################################################
#' @rdname is.numint
#' @export
unique.class <-
  function (x) {
    res <- sapply(x, class)
    res <- length(unique(res))
    res <- ifelse(res == 1, TRUE, FALSE)
    return (res)
  }
