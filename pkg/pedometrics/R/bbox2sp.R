#  file pedometrics/R/bbox2sp.R
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
#  Purpose        : create Spatial object from a bounding box
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#
bbox2sp <- 
  function (obj, sp = "SpatialPolygons", keep.crs = TRUE) {
    if (!inherits(obj, "Spatial")) {
      stop ("obj should be of class Spatial")
    }
    if (keep.crs) {
      if (is.na(proj4string(obj))) {
        stop ("obj DOES NOT have a coordinate reference system")
      }
    }
    bb <- bbox(obj)
    bbx <- c(bb[1, 1], bb[1, 2], bb[1, 2], bb[1, 1], bb[1, 1])
    bby <- c(bb[2, 1], bb[2, 1], bb[2, 2], bb[2, 2], bb[2, 1])
    if (sp == "SpatialPoints") {
      bb <- SpatialPoints(data.frame(bbx, bby))
      bb <- bb[1:4, ]
    }
    if (sp == "SpatialPointsDataFrame") {
      bb <- data.frame(bbx, bby)
      bb <- bb[1:4, ]
      bb <- SpatialPointsDataFrame(bb, data = data.frame(ID = c(1, 2, 3, 4)))
    }
    if (sp == "SpatialPolygons") {
      bb <- SpatialPoints(data.frame(bbx, bby))
      bb <- Polygons(list(Polygon(bb)), ID = as.character(1))
      bb <- SpatialPolygons(list(bb))
    }
    if (sp == "SpatialPolygonsDataFrame") {
      bb <- SpatialPoints(data.frame(bbx, bby))
      bb <- Polygons(list(Polygon(bb)), ID = as.character(1))
      bb <- SpatialPolygons(list(bb))
      bb <- SpatialPolygonsDataFrame(bb, data = data.frame(ID = 1))
    }
    if (keep.crs) {
      proj4string(bb) <- proj4string(obj)
    }
    return (bb)
  }
# End!