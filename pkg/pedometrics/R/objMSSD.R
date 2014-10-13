#  file pedometrics/R/objMSSD.R
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
#  Purpose        : random perturbation of coordinates
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : G. Heuvelink (gerard.heuvelink@wur.nl)
#                   R. Hijmans (r.hijmans@gmail.com)
#
.distMat <- 
  function (obj, exponent = 1, diagonal = 0) {
    if (inherits(obj, what = "Spatial")) {
      obj <- coordinates(obj)
    }
    dist_mat <- dist(obj)
    dist_mat <- as.matrix(dist_mat)
    dist_mat <- dist_mat ^ exponent
    diag(dist_mat) <- diagonal
    return (dist_mat)
  }
.distanceToNearestPoint <- 
  function (obj, points) {
    if (dim(obj)[1] != dim(obj)[2] || !is.matrix(obj)) {
      stop ("'obj' should be a n x n matrix")
    }
    if (!is.vector(points) || length(points) >= dim(obj)[2]) {
      stop (paste("'points' should be a vector of length smaller than ", 
                  dim(obj)[2], sep = ""))
    }
    sub_dist_mat <- obj[, points]
    min_dist <- apply(sub_dist_mat, MARGIN = 1, FUN = min)
    return (min_dist)
  }
.nearestPoint <- 
  function (obj, points) {
    if (dim(obj)[1] != dim(obj)[2] || !is.matrix(obj)) {
      stop ("'obj' should be a n x n matrix")
    }
    if (!is.vector(points) || length(points) >= dim(obj)[2]) {
      stop (paste("'points' should be a vector of length smaller than ", 
                  dim(obj)[2], sep = ""))
    }
    sub_dist_mat <- obj[, points]
    min_dist_id <- apply(sub_dist_mat, MARGIN = 1, FUN = which.min)
    return (min_dist_id)
  }
objMSSD <-
  function (obj, pred.grid, mask, dist.mat, points, finite) {
    if (missing(finite)) {
      stop("you must provide a 'finite' argument (TRUE/FALSE)")
    }
    stopifnot(is.logical(finite))
    if (!finite) {
      res <- distanceFromPoints(object = pred.grid, xy = obj) ^ 2
      if (!missing(mask)) {
        res <- raster::mask(x = res, mask = mask)  
      }
      res <- mean(res@data@values, na.rm = TRUE)
      return (res)
    } else {
      if (dim(dist.mat)[1] != dim(dist.mat)[2] || !is.matrix(dist.mat)) {
        stop ("'dist.mat' should be a n x n matrix")
      }
      if (!is.vector(points) || length(points) >= dim(dist.mat)[2]) {
        stop (paste("'points' should be a vector of length smaller than ", 
                    dim(dist.mat)[2], sep = ""))
      }
      res <- .distanceToNearestPoint(obj = dist.mat, points = points)
      res <- mean(res, na.rm = TRUE)
      return (res)
    }
  }
# End!