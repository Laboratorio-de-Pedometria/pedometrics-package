#' Spatially correlated variance
#' 
#' Compute the proportion of the variance that is spatially correlated.
#' 
#' @param obj Variogram model fitted with available function in geostatistical
#' packages such as \pkg{gstat}, \pkg{geoR}, and \pkg{georob}.
#' 
#' @param digits Integer indicating the number of decimal places to be used.
#' 
#' @return Numeric value indicating the proportion of the variance that is 
#' spatially correlated.
#' 
#' @author Alessandro Samuel-Rosa <\email{alessandrosamuelrosa@@gmail.com}>
#' @seealso \code{\link[pedometrics]{vgmLags}}
#' @concept variogram
#' @export
# FUNCTION - MAIN ##############################################################
vgmSCV <- 
  function (obj, digits = 4) {
    
    if (is(obj, "variomodel")) { # geoR-package
      res <- obj$sigmasq / (obj$sigmasq + obj$tausq)
    }
    
    if (is(obj, "variogramModel")) { # gstat-package
      res <- obj$psill[2] / sum(obj$psill)
    }
    
    if (is(obj, "georob")) {
      res <- obj$param[1] / sum(obj$param[1:3])
    }
    
    # Output
    res <- round(unname(res), digits = digits)
    return (res)
  }
