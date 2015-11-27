#' Lag-distance classes for variogram estimation
#' 
#' Computation of lag-distance classes for varioram estimation.
#' 
#' @param obj Data frame or matrix with the projected x- and y-coordinates.
#' 
#' @param n Integer value defining the number of lag-distance classes that 
#' should be computed. Defaults to \code{n = 7}.
#' 
#' @param type Character value defining the type of lag-distance classes that
#' should be computed, with options \code{"equi"} (equidistant) and \code{"exp"}
#' (exponential). Defaults to \code{type = "exp"}.
#' 
#' @param cutoff Numeric value defining the fraction of the diagonal of the
#' rectangle that spans the data (bounding box) that should be used to set the
#' maximum distance up to which lag-distance classes should be computed. 
#' Defaults to \code{cutoff = 0.5}, i.e. half the diagonal of the bounding box.
#' 
#' @param base Numeric value defining the base of the exponential expression 
#' used to create exponentially spaced lag-distance classes. Used only when 
#' \code{type = "exp"}. Defaults to \code{base = 2}, i.e. the width of the 
#' rightmost lag-distance classes is equal to half the diagonal of 
#' \code{cutoff}, and so on.
#' 
#' @param zero Numeric value setting the minimum pair-wise separation distance 
#' that should be used to compute the lag-distance classes. Defaults to 
#' \code{zero = 0.0001}.
#' 
#' @return Vector of numeric values with the lower and upper boundaries of the 
#' lag-distance classes.
#' 
#' @author Alessandro Samuel-Rosa <\email{alessandrosamuelrosa@@gmail.com}>
#' @seealso \code{\link[spsann]{optimPPL}}
#' @concept variogram
#' @references
#' Truong, P. N.; Heuvelink, G. B. M.; Gosling, J. P. Web-based tool for expert
#' elicitation of the variogram. \emph{Computers and Geosciences}. v. 51, p.
#' 390-399, 2013.
#' @export
#' @examples
#' require(sp)
#' data(meuse.grid)
#' vgmLags(meuse.grid[, 1:2])
# FUNCTION - MAIN ##############################################################
vgmLags <-
  function (obj, n = 7, type = "exp", cutoff = 0.5, base = 2, zero = 0.001) {
    
    # Check if suggested packages are installed
    pkg <- c("sp")
    id <- !sapply(pkg, requireNamespace, quietly = TRUE)
    if (any(id)) {
      pkg <- paste(pkg[which(id)], collapse = " ")
      stop(paste("Package(s) needed for this function to work but not",
                 "installed: ", pkg, sep = ""), call. = FALSE)
    }
    
    # Compute cutoff
    
    cutoff <- max(stats::dist(obj)) * cutoff
    
    # Equidistant lag distance classes  
    if (type == "equi") {
      lags <- seq(zero, cutoff, length.out = n + 1)
    }
    
    # Exponential lag distance classes
    if (type == "exp") {
      idx <- base ^ c(1:n - 1)
      lags <- c(zero, rev(cutoff / idx))
    }
    
    # Output
    return (lags)
  }
