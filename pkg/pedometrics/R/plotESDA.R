#  file pedometrics/R/plotESDA.R
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

#  Purpose        : plots for exploratory spatial data analysis (ESDA)
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Depends on     : lattice, sp, gstat
#
plotESDA <- 
  function (z, lat, lon, cutoff, width = c(cutoff / 20)) {
    if (missing(z)) {
      stop("<z> is a mandatory argument")
    }
    if (missing(lon)) {
      stop("<lon> is a mandatory argument")
    }
    if (missing(lat)) {
      stop("<lat> is a mandatory argument")
    }
    if (!any(class(z) == c("numeric", "integer"))) {
      stop("<z> should be of class numeric or integer")
    }
    if (!any(class(lat) == c("numeric", "integer"))) {
      stop("<lat> should be of class numeric or integer")
    }
    if (!any(class(lon) == c("numeric", "integer"))) {
      stop("<lon> should be of class numeric or integer")
    }
    if (length(unique(c(length(z), length(lat), length(lon)))) > 1) {
      stop("<z>, <lat> and <lon> must have the same length")
    }
    db <- data.frame(lon = lon, lat = lat, z = z)
    coordinates(db) <- ~ lon + lat
    if (missing(cutoff)) {
      cutoff <- max(variogram(z ~ 1, loc = db)$dist) 
    }
    v1 <- bubble(db, zcol = "z", fill = FALSE, main = "", maxsize = 1)
    v2 <- variogram(z ~ 1, loc = db, map = TRUE, cutoff = cutoff, width = width)
    v2 <- spplot(v2$map[2], col.regions = bpy.colors(64))
    v3 <- variogram(z ~ 1, loc = db, cutoff = cutoff, width = width)
    v3 <- plot(v3, cex = 0.5, type = "b", pch = 20, asp = 1)
    v4 <- plotHD(z, HD = "over", stats = FALSE, asp = 1, xlab = "z",
                 col = c("skyblue", "red"))
    print(v4, split = c(1, 1, 2, 2), more = TRUE)
    print(v3, split = c(1, 2, 2, 2), more = TRUE)
    print(v1, split = c(2, 1, 2, 2), more = TRUE)
    print(v2, split = c(2, 2, 2, 2), more = FALSE)
  }
# End!