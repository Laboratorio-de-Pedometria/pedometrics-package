# Prepare object for argument \code{design} of \code{spsurvey.analysis()}
#'
# This function returns an object to feed the argument \code{design} when
# creating an object of class \code{spsurvey.analysis}.
#'
# The argument \code{design} used to create object of class
# \code{spsurvey.analysis} requires a series of inputs. However, it can be fed
# with data about site ID and coordinates. \code{coordenadas()} returns a data
# frame that provides this information, assuming that all other design
# variables are provided manually in the arguments list.
#'
# @param x Object of class `SpatialPointsDataFrame` from which site ID and XY coordinates are to be
# returned.
#'
# @return An object of class `data.frame` containing three columns with names `siteID`, `xcoord`,
# and `ycoord`.
#'
# @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
# @seealso \code{\link[pedometrics]{gcpDiff}}.
# @references Kincaid, T. M. and Olsen, A. R. (2013). spsurvey: Spatial Survey
# Design and Analysis. R package version 2.6. URL: \url{https://www.epa.gov/}.
# @keywords methods
# @export
# @examples
# \dontrun{
# if (require(spsurvey)) {
#   ## Create an spsurvey.analysis object
#   my.spsurvey <-
#     spsurvey::spsurvey.analysis(
#       design = coordenadas(my.data),
#       data.cont = delta(ref.data, my.data),
#       popcorrect = TRUE, pcfsize = length(my.data$id),
#       support = rep(1, length(my.data$id)),
#       wgt = rep(1, length(my.data$id)), vartype = "SRS")
# }
# }
# FUNCTION #########################################################################################
coordenadas <-
  function(x) {
    # Check if suggested packages are installed
    if (!requireNamespace("sp")) stop("sp package is missing")
    coo <- data.frame(x$siteID, sp::coordinates(x))
    coo <- coo[order(as.numeric(x$siteID)), ]
    colnames(coo) <- c("siteID", "xcoord", "ycoord")
    row.names(coo) <- NULL
    return(coo)
  }
