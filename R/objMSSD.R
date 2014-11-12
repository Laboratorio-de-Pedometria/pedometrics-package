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
distMat <- 
  function (candidates, exponent = 1, diagonal = 0) {
    dist_mat <- dist(candidates)
    dist_mat <- as.matrix(dist_mat)
    dist_mat <- dist_mat ^ exponent
    diag(dist_mat) <- diagonal
    return (dist_mat)
  }
distToNearestPoint <-
  function (dist.mat, which.pts) {
    if (dim(dist.mat)[1] != dim(dist.mat)[2] || !is.matrix(dist.mat)) {
      stop ("'dist.mat' should be a n x n matrix")
    }
    if (length(which.pts) >= dim(dist.mat)[2]) {
      stop (paste("'which.pts' should be a vector of length smaller than ", 
                  dim(dist.mat)[2], sep = ""))
    }
    sub_dist_mat <- dist.mat[, which.pts]
    min_dist <- as.numeric(apply(sub_dist_mat, MARGIN = 1, FUN = min))
    return (min_dist)
  }
nearestPoint <-
  function (dist.mat, which.pts) {
    if (dim(dist.mat)[1] != dim(dist.mat)[2] || !is.matrix(dist.mat)) {
      stop ("'dist.mat' should be a n x n matrix")
    }
    if (!is.vector(which.pts) || length(which.pts) >= dim(dist.mat)[2]) {
      stop (paste("'which.pts' should be a vector of length smaller than ", 
                  dim(dist.mat)[2], sep = ""))
    }
    sub_dist_mat <- dist.mat[, which.pts]
    min_dist_id <- apply(sub_dist_mat, MARGIN = 1, FUN = which.min)
    return (min_dist_id)
  }
objMSSD <-
  function (points, pred.grid, dist.mat) {
    which_pts <- duplicated(rbind(points, pred.grid))
    which_pts <- which(which_pts[-c(1:dim(points)[1])])
    if (dim(dist.mat)[1] != dim(dist.mat)[2] || !is.matrix(dist.mat)) {
      stop ("'dist.mat' should be a n x n matrix")
    }
    res <- distToNearestPoint(dist.mat = dist.mat, which.pts = which_pts)
    res <- mean(res, na.rm = TRUE)
    return (res)
  }
# End!