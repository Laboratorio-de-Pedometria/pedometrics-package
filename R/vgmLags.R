#' Variogram binning
#' 
#' @description 
#' Computation of bins for sample (experimental) variograms.
#' 
#' @param coords Data frame or matrix with the projected x- and y-coordinates.
#' 
#' @param n.lags Integer value defining the number of variogram bins (distance classes) that should
#' be computed. Defaults to `n = 7`.
#' 
#' @param type Character value defining the type of variogram bins that should be computed,
#' with options `"equi"` (equidistant, equal-width) and `"exp"` (exponential, exponentially 
#' growing). Defaults to `type = "exp"`.
#' 
#' @param cutoff Numeric value defining the fraction of the diagonal of the rectangle that spans the
#' data (bounding box) that should be used to set the maximum distance up to which variogram bins
#' should be computed. Defaults to `cutoff = 0.5`, i.e. half the diagonal of the bounding box.
#' 
#' @param base Numeric value defining the base of the exponential expression used to create
#' exponentially growing variogram bins. Used only when `type = "exp"`. Defaults to `base = 2`,
#' i.e. the width of the rightmost bin is equal to half the diagonal of `cutoff`, and so on.
#' 
#' @param zero Numeric value setting the minimum pair-wise separation distance that should be used
#' to compute the variogram bins. Defaults to `zero = 0.0001`.
#' 
#' @param count Should the number of points (`"points"`) or point-pairs (`"pairs"`) per variogram
#' bin be computed? Defaults to `count = "pairs"`.
#' 
#' @return
#' Vector of numeric values with the lower and upper boundaries of the variogram bins. The
#' number of points or point-pairs per variogram bin is returned as an attribute.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @section Dependencies:
#' The __SpatialTools__ package, provider of tools for spatial data analysis in R, is required for
#' [pedometrics::variogramBins()] to work. The development version of the __SpatialTools__ package
#' is available on <https://github.com/jfrench/SpatialTools> while its old versions are available
#' on the CRAN archive at <https://cran.r-project.org/src/contrib/Archive/SpatialTools/>.
#' 
#' @section Reverse dependencies:
#' The __spsann__ package, provider of methods for the optimization of sample configurations using
#' spatial simulated annealing in R, requires [pedometrics::variogramBins()] for some of its
#' functions to work. The development version of the __spsann__ package is available on
#' <https://github.com/Laboratorio-de-Pedometria/spsann-package>.
#' 
#' @references
#' Truong, P. N.; Heuvelink, G. B. M.; Gosling, J. P. Web-based tool for expert elicitation of the
#' variogram. _Computers and Geosciences_. v. 51, p. 390-399, 2013.
#' 
#' @examples
#' if (all(c(require(SpatialTools), require(sp)))) {
#'   data(meuse, package = "sp")
#'   lags_points <- variogramBins(coords = meuse[, 1:2], count = "points")
#'   lags_pairs <- variogramBins(coords = meuse[, 1:2], count = "pairs")
#' }
#' @concept variogram
# FUNCTION #########################################################################################
#' @export
#' @rdname variogramBins
variogramBins <-
  function(coords, n.lags = 7, type = "exp", cutoff = 0.5, base = 2, zero = 0.001,
    count = "pairs") {
    # check if suggested packages are installed
    if (!requireNamespace("SpatialTools")) stop("SpatialTools package is missing")
    # compute cutoff
    if (inherits(coords, c("matrix", "data.frame", "array"))) {
      cutoff <- sqrt(sum(apply(apply(coords[, 1:2], 2, range), 2, diff) ^ 2)) * cutoff
    } else {
      message("'coords' should be a data frame with the projected coordinates")
    }
    # compute the boundaries of the lag-distance classes
    n_pts <- nrow(coords)
    lags <- switch(
      type,
      equi = { # Equidistant, equal-width
        seq(zero, cutoff, length.out = n.lags + 1)
      },
      exp = { # Exponentially growing
        idx <- base ^ c(1:n.lags - 1)
        c(zero, rev(cutoff / idx))
      }
    )
    # Count the number of points or point-pairs per lag-distance class
    dm <- SpatialTools::dist1(as.matrix(coords))
    ppl <- switch(
      count,
      pairs = { # Point-pairs per lag-distance class
        diff(sapply(seq_along(lags), function(i) length(which(dm <= lags[i]))) - n_pts) * 0.5
      },
      points = { # Points per lag-distance class
        sapply(1:n.lags, function(i)
          length(unique(c(which(dm > lags[i] & dm <= lags[i + 1], arr.ind = TRUE)))))
      })
    # Output with attributes
    a <- attributes(lags)
    a$count <- ppl
    attributes(lags) <- a
    return(lags)
  }
#' @export
#' @rdname variogramBins
vgmLags <- variogramBins
