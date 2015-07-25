#' Lag-distance classes for variogram estimation
#' 
#' Computation of lag-distance classes for varioram estimation.
#' 
#' @param obj Data frame or matrix; the x and y coordinates (the coordinates 
#' must be projected).
#' 
#' @param n Integer value; the number of lag-distace classes. Defaults to 
#' \code{n = 7}.
#' 
#' @param type Caracter value; the type of lag-distance classes, with options
#' \code{equi} (equidistant) and \code{exp} (exponential). Defaults to 
#' \code{type = "exp"}.
#' 
#' @param cutoff Numeric value; the fraction of the maximum pair-wise sepation
#' distances that should be used to compute the lag-distance classes. 
#' Defaults to \code{cutoff = 0.5}.
#' 
#' @param base Numeric value; the base of the exponential expression used to
#' create exponentially spaced lag-distance classes. Used only when 
#' \code{type = "exp"}. Defaults to \code{base = 2}.
#' 
#' @param zero Numeric value; the minimum pair-wise separation distance that
#' should be used to compute the lag-distance classes. Defaults to 
#' \code{zero = 0.0001}.
#' 
#' @return Vector of numeric values with the breaks of the lag-distance classes.
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
#' id <- sample(nrow(meuse.grid), 100)
#' obj <- meuse.grid[id, 1:2]
#' vgmLags(obj)
# FUNCTION - MAIN ##############################################################
vgmLags <-
  function (obj, n = 7, type = "exp", cutoff = 0.5, base = 2, zero = 0.001) {
    
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
