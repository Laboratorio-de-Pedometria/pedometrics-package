#' Extract spatial trend data
#' 
#' Extract spatial trend data from an object of class \code{likfit}.
#' 
#' @param x Object of class \code{likfit}.
#' 
#' @seealso \code{\link[geoR]{likfit}}
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @aliases trend.terms trend.matrix
#' @examples
#' reml <- likfit(s100, ini=c(0.5, 0.5), fix.nug = TRUE, lik.met = "REML")
#' trend.terms(reml)
#' head(trend.matrix(reml))
# FUNCTION - EXTRACT TREND TERMS ###############################################
# This function is similar to stats::terms
#' @export
#' @rdname terms
trend.terms <- 
  function (x) {
    cl <- class(x)
    if (all(cl == c("likGRF","variomodel"))) {
      res <- all.vars(x$trend)
    }
    return (res)
  }
# FUNCTION - EXTRACT TREND MATRIX ##############################################
# This function is similar to stats::model.frame
#' @export
#' @rdname terms
trend.matrix <-
  function (x) {
    cl <- class(x)
    if (all(cl == c("likGRF","variomodel"))) {
      res <- x$trend.matrix[, -1]
      colnames(res) <- trend.terms(x)
    }
    return (res)
  }
