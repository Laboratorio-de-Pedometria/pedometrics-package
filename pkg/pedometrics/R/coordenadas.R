#  file pedometrics/R/coordenadas.R
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
# DOCUMENTATION ################################################################
#' Prepare object for argument \code{design} of \code{spsurvey.analysis()}
#' 
#' This function returns an object to feed the argument \code{design} when
#' creating an object of class \code{spsurvey.analysis}.
#' 
#' The argument \code{design} used to create object of class
#' \code{spsurvey.analysis} requires a series of inputs. However, it can be fed
#' with data about site ID and coordinates. \code{coordenadas()} returns a data
#' frame that provides this information, assuming that all other design
#' variables are provided manualy in the arguments list.
#' 
#' @param x Object of class \code{\linkS4class{SpatialPointsDataFrame}} from
#' which site ID and XY coordinates are to be returned.
#' @return An object of class \code{data.frame} containing three columns with
#' names \code{siteID}, \code{xcoord}, and \code{ycoord}.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[pedometrics]{gcpDiff}},
#' \code{\link[spsurvey]{cont.analysis}}.
#' @references Kincaid, T. M. and Olsen, A. R. (2013). spsurvey: Spatial Survey
#' Design and Analysis. R package version 2.6. URL:
#' <\url{http://www.epa.gov/nheerl/arm/}>.
#' @keywords methods
#' @export
#' @examples
#' 
#' \dontrun{
#' ## Create an spsurvey.analysis object
#' my.spsurvey <- 
#'   spsurvey.analysis(design = coordenadas(my.data),
#'                     data.cont = delta(ref.data, my.data),
#'                     popcorrect = TRUE, pcfsize = length(my.data$id),
#'                     support = rep(1, length(my.data$id)),
#'                     wgt = rep(1, length(my.data$id)), vartype = "SRS")
#' }
#' 
# FUNCTION #####################################################################
coordenadas <-
  function(x) {
    coo <- data.frame(x$siteID, coordinates(x))
    coo <- coo[order(as.numeric(x$siteID)), ]
    colnames(coo) <- c("siteID", "xcoord", "ycoord")
    row.names(coo) <- NULL
    return(coo)
  }
# End!