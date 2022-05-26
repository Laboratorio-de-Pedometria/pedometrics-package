#' Create a Spatial* object from a bounding box
#'
#' @description
#' Take the bounding box of a Spatial* object and create a SpatialPoints* or SpatialPolygons* object
#' from it.
#'
#' @param obj Object of class Spatial*.
#'
#' @param sp Class of the resulting object with options `"SpatialPolygons"` (default),
#' `"SpatialPoints"`, `"SpatialPointsDataFrame"`, and `"SpatialPolygonsDataFrame"`.
#'
#' @param keep.crs Logical for assigning the same coordinate reference system to the resulting
#' Spatial* object. Defaults to `keep.crs = TRUE`.
#'
#' @return An object of class SpatialPoints* or SpatialPolygons*.
#' 
#' @section Requires
#' The __sp__ package, provider of classes and methods for spatial data in R, is required for this
#' function to work. The development version of the __sp__ package is available on
#' <https://github.com/edzer/sp/>. Its old versions are available on the CRAN archive at
#' <https://cran.r-project.org/src/contrib/Archive/sp/>.
#' 
#' @note Some of the solutions used to build this function were found in the source code of the
#' R-package __intamapInteractive__. As such, the authors of that package, Edzer Pebesma
#' \email{edzer.pebesma@@uni-muenster.de} and Jon Skoien \email{jon.skoien@@gmail.com}, are entitled
#' \sQuote{contributors} to the R-package __pedometrics__.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @references
#' Edzer Pebesma, Jon Skoien with contributions from Olivier Baume, A. Chorti, D.T. Hristopulos,
#' S.J. Melles and G. Spiliopoulos (2013). _intamapInteractive: procedures for automated
#' interpolation - methods only to be used interactively, not included in intamap package._ R
#' package version 1.1-10. \url{https://CRAN.R-project.org/package=intamapInteractive}.
#'
#' @examples
#' if (require(sp)) {
#' data(meuse, package = "sp")
#' sp::coordinates(meuse) <- ~ x + y
#' bb <- bbox2sp(obj = meuse, keep.crs = FALSE)
#' }
#' @keywords misc spatial
#' @export
# FUNCTION #########################################################################################
bbox2sp <-
  function(obj, sp = "SpatialPolygons", keep.crs = TRUE) {
    # Check if suggested packages are installed
    if (!requireNamespace("sp")) stop("sp package is missing")
    # Check function arguments
    if (!inherits(obj, "Spatial")) {
      stop("'obj' should be of class Spatial")
    }
    if (keep.crs) {
      if (is.na(sp::proj4string(obj))) {
        stop("'obj' does not have a coordinate reference system")
      }
    }
    bb <- sp::bbox(obj)
    bbx <- c(bb[1, 1], bb[1, 2], bb[1, 2], bb[1, 1], bb[1, 1])
    bby <- c(bb[2, 1], bb[2, 1], bb[2, 2], bb[2, 2], bb[2, 1])
    if (sp == "SpatialPoints") {
      bb <- sp::SpatialPoints(data.frame(bbx, bby))
      bb <- bb[1:4, ]
    }
    if (sp == "SpatialPointsDataFrame") {
      bb <- data.frame(bbx, bby)
      bb <- bb[1:4, ]
      bb <- sp::SpatialPointsDataFrame(bb, data = data.frame(ID = c(1, 2, 3, 4)))
    }
    if (sp == "SpatialPolygons") {
      bb <- sp::SpatialPoints(data.frame(bbx, bby))
      bb <- sp::Polygons(list(sp::Polygon(bb)), ID = as.character(1))
      bb <- sp::SpatialPolygons(list(bb))
    }
    if (sp == "SpatialPolygonsDataFrame") {
      bb <- sp::SpatialPoints(data.frame(bbx, bby))
      bb <- sp::Polygons(list(sp::Polygon(bb)), ID = as.character(1))
      bb <- sp::SpatialPolygons(list(bb))
      bb <- sp::SpatialPolygonsDataFrame(bb, data = data.frame(ID = 1))
    }
    if (keep.crs) {
      sp::proj4string(bb) <- sp::proj4string(obj)
    }
    return(bb)
  }
