#' Calculate module and azimuth
#'
#' @description
#' Calculate the module and azimuth of the difference on x and y coordinates between two sets of
#' ground control points (GCP). It is suited to perform calculations for topographical
#' coordinates only. The origin is set in the y coordinate, and rotation performed clockwise.
#'
#' @param dx Numeric vector containing the difference on the \sQuote{x} coordinate between two sets
#' of GCP.
#'
#' @param dy Numeric vector containing the difference on the \sQuote{y} coordinate between two sets
#' of GCP.
#'
#' @return
#' A data frame containing the module, its square and azimuth. These three columns are named
#' \sQuote{module}, \sQuote{sq.module} and \sQuote{azimuth}.
#'
#' @note This function was adapted from package's __VecStatGraphs2D__ function `LoadData()`.
#'
#' @author Juan Carlos Ruiz Cuetos \email{bilba_t@@hotmail.com}\cr Maria Eugenia Polo Garcia
#' \email{mepolo@@unex.es}\cr Pablo Garcia Rodriguez \email{pablogr@@unex.es}\cr Alessandro
#' Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @references
#' Ruiz-Cuetos J.C., Polo M.E. and Rodriguez P.G. (2012). _VecStatGraphs2D: Vector analysis using
#' graphical and analytical methods in 2D_. R package version 1.6.
#' <https://CRAN.R-project.org/package=VecStatGraphs2D>.
#'
#' @examples
#' x <- gcpVector(dx = rnorm(3, 5, 10), dy = rnorm(3, 5, 10))
#' 
# FUNCTION #########################################################################################
#' @export
gcpVector <-
  function(dx, dy) {
    vec <- sqrt((dx * dx) + (dy * dy))
    vec2 <- vec * vec
    azim1 <- (180 * (atan(dx / dy)) / pi)
    quad <- dy >= 0
    azim1[is.na(azim1)] <- 0
    azim2 <- azim1
    azim2[quad == FALSE] <- azim1[quad == FALSE] + 180
    quad2 <- azim2 >= 0
    azim3 <- azim2
    azim3[quad2 == FALSE] <- azim2[quad2 == FALSE] + 360
    azim <- rep(NA, length(dx))
    azim <- azim3
    res <- data.frame(vec, vec2, azim)
    colnames(res) <- c("module", "sq.module", "azimuth")
    return(res)
  }
