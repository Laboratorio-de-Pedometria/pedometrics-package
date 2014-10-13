#  file pedometrics/R/spJitter.R
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
#                   E. Pebesma (edzer.pebesma@uni-muenster.de)
#                   J. Skoien (jon.skoien@gmail.com)
#
spJitter <- 
  function (points, candidates, where, which, finite,
            x.coord = list(min = NULL, max = NULL), 
            y.coord = list(min = NULL, max = NULL), 
            zero = 1, iterations = 10000, verbose = TRUE) {
    if (missing(finite)) {
      stop("you must provide a 'finite' argument (TRUE/FALSE)")
    }
    stopifnot(is.logical(finite))
    stopifnot(is.logical(verbose))
    if (missing(points)) {
      stop ("'points' is a mandatory argument")
    }
    if (!is.list(x.coord) || length(x.coord) != 2) {
      stop ("'x.coord' should be a list with 2 subarguments")
    } else {
      if (is.null(x.coord$min) || is.null(x.coord$max)) {
        stop ("'min' and 'max' are mandatory subarguments for 'x.coord")
      }
    }
    if (!is.list(y.coord) || length(y.coord) != 2) {
      stop ("'y.coord' should be a list with 2 subarguments")
    } else {
      if (is.null(y.coord$min) || is.null(y.coord$max)) {
        stop ("'min' and 'max' are mandatory subarguments for 'y.coord")
      }
    }
    if (finite) {
      if (inherits(points, "SpatialPoints")) {
        stop ("'points' should be a vector of indexes")
      }
      if (missing(candidates)) {
        stop ("'candidates' is a mandatory argument")
      }
      if (!inherits(candidates, what = "data.frame")) {
        stop ("'candidates' should be a data.frame")
      } else {
        if (dim(candidates)[2] != 2 || 
              any(colnames(candidates) != c("x", "y"))) {
          stop ("'candidates' should have 2 columns named 'x' and 'y'")
        }
      }
      if (unique(which == "all")) {
        stop ("this option is not functional yet")
        #res <- candidates[sample(c(1:length(candidates)), length(points)), ]
        } else {
          if (length(which) > 1) {
            stop ("this option is not functional yet")
            #pt0 <- coordinates(points[which, ])
            #res <- list()
            #for (i in 1:length(which)) {
            #  cand <- coordinates(candidates)
            #  d_x <- x.coord$max + x.coord$min
            #  d_y <- y.coord$max + y.coord$min
            #  d_x <- c(pt0[i, "x"] - d_x, pt0[i, "x"] + d_x)
            #  d_y <- c(pt0[i, "y"] - d_y, pt0[i, "y"] + d_y)
            #  cand <- which(cand[, "x"] >= d_x[1] & cand[, "x"] <= d_x[2] &
            #                  cand[, "y"] >= d_y[1] & cand[, "y"] <= d_y[2])
            #  res[i] <- candidates[sample(cand, 1), ]
            #}
            #res <- data.frame(t(sapply(res, coordinates)))
            #colnames(res) <- c("x", "y")
            #coordinates(res) <- ~ x + y
            #proj4string(res) <- proj4string(points)
            #res <- rbind(points[-which, ], res)
          } else {
            pts <- candidates[points, ]
            d_x <- x.coord$max + x.coord$min
            d_y <- y.coord$max + y.coord$min
            pt0 <- pts[rownames(pts) == which, ]
            d_x <- unlist(c(pt0["x"] - d_x, pt0["x"] + d_x))
            d_y <- unlist(c(pt0["y"] - d_y, pt0["y"] + d_y))
            pt1 <- which(candidates[, "x"] >= d_x[1] &
                           candidates[, "x"] <= d_x[2] &
                           candidates[, "y"] >= d_y[1] & 
                           candidates[, "y"] <= d_y[2])
            pt2 <- sample(pt1, 1)
            while (any(pt2 == points)) {
              pt2 <- sample(pt1, 1)
            }
            a <- pts[rownames(pts) != which, ]
            b <- candidates[rownames(candidates) == pt2, ]
            res <- rbind(a, b)
          }
        }
      return (res)
      } else {
        if (missing(where)) {
          stop ("'where' is a mandatory argument")
        } else {
          if (!inherits(where, what = "SpatialPolygons")) {
            stop ("'where' should be of class SpatialPolygons")
          } else {
            where <- as(where, "SpatialPolygons")
          }
        }
        if (!is.numeric(zero)) {
          stop ("'zero' should be a numeric value")
        }
        if (!is.numeric(iterations)) {
          stop ("'iterations' should be a numeric value")
        }
        if (!inherits(points, "SpatialPoints") || is.na(proj4string(points)) || 
              !is.projected(points)) {
          stop ("'points' should be of class SpatialPoints with projected CRS")
        } else {
          points <- as(points, "SpatialPoints")
          colnames(points@coords) <- c("x", "y")
          rownames(points@bbox) <- c("x", "y")
        }
        if (unique(which == "all")) {
          x0 <- coordinates(points)[, "x"]
          y0 <- coordinates(points)[, "y"]
        }
        if (is.numeric(which)) {
          x0 <- coordinates(points)[which, "x"]
          y0 <- coordinates(points)[which, "y"]
        }
        x1 <- jitter(x = x0, amount = x.coord$max)
        dx <- abs(x0 - x1)
        dx <- which(dx < x.coord$min)
        x1[dx] <- x0[dx] + sign(x0[dx] - x1[dx]) * x.coord$min
        y1 <- jitter(x = y0, amount = y.coord$max)
        dy <- abs(y0 - y1)
        dy <- which(dy < y.coord$min)
        y1[dy] <- y0[dy] + sign(y0[dy] - y1[dy]) * y.coord$min
        res <- data.frame(x = x1, y = y1)
        coordinates(res) <- ~ x + y
        proj4string(res) <- proj4string(points)
        if (is.numeric(which)) {
          res <- rbind(points[-which, ], res)
        }
        if (!is.null(where)) {
          out <- which(gContains(where, res, byid = TRUE) == FALSE)
          n_out <- length(out)
          n_iter <- 1
          while (n_out >= 1) {
            res_out <- points[out, ]
            res <- res[-out, ]
            x0 <- coordinates(res_out)[, "x"]
            y0 <- coordinates(res_out)[, "y"]
            x1 <- jitter(x = x0, amount = x.coord$max)
            dx <- abs(x0 - x1)
            dx <- which(dx < x.coord$min)
            x1[dx] <- x0[dx] + sign(x0[dx] - x1[dx]) * x.coord$min
            y1 <- jitter(x = y0, amount = y.coord$max)
            dy <- abs(y0 - y1)
            dy <- which(dy < y.coord$min)
            y1[dy] <- y0[dy] + sign(y0[dy] - y1[dy]) * y.coord$min
            res_out <- data.frame(x = x1, y = y1)
            coordinates(res_out) <- ~ x + y
            proj4string(res_out) <- proj4string(res)
            res <- rbind(res, res_out)
            while(length(zerodist(res, zero = zero))[1] > 0){
              dup <- unique(c(zerodist(res, zero = zero)))
              res_dup <- res[dup, ]
              res <- res[-dup]
              x0 <- coordinates(res_dup)[, "x"]
              y0 <- coordinates(res_dup)[, "y"]
              x1 <- jitter(x = x0, amount = x.coord$min)
              y1 <- jitter(x = y0, amount = y.coord$min)
              res_dup <- data.frame(x = x1, y = y1)
              coordinates(res_dup) <- ~ x + y
              proj4string(res_dup) <- proj4string(res)
              res <- rbind(res, res_dup)
            }
            out <- which(gContains(where, res, byid = TRUE) == FALSE)
            n_out <- length(out)
            n_iter <- n_iter + 1
            if (n_iter == iterations) {
              break
            }
          }
        }
        if (!is.null(where)) {
          if (n_out >= 1) {
            message(paste("spJitter DID NOT converge after ", n_iter, 
                          " iterations", sep = ""))
            if (n_out > 1) {
              stop(paste("there are ", n_out, 
                         " points outside the spatial domain", sep = ""))
            } else {
              stop(paste("there is ", n_out, 
                         " point outside the spatial domain", sep = ""))
            }          
          } else {
            if (verbose) {
              message(paste("spJitter converged after ", n_iter,
                            " iterations", sep = ""))
            }
          }
        }
        return (res)
      }
  }
# controls for spJitter inside spSANN
spJitter.control <-
  function (candidates, where, finite,
            x.coord = list(min = NULL, max = NULL),
            y.coord = list(min = NULL, max = NULL),
            size = 1, size.factor = 1000,
            zero = 1, iterations = 10000, verbose = FALSE) {
    if (missing(finite)) {
      stop("you must provide a 'finite' argument (TRUE/FALSE)")
    }
    stopifnot(is.logical(finite))
    stopifnot(is.logical(verbose))
    if (!is.list(x.coord) || length(x.coord) != 2) {
      stop ("'x.coord' should be a list with 2 subarguments")
    } else {
      if (is.null(x.coord$min) || is.null(x.coord$max)) {
        stop ("'min' and 'max' are mandatory subarguments for 'x.coord")
      }
    }
    if (!is.list(y.coord) || length(y.coord) != 2) {
      stop ("'y.coord' should be a list with 2 subarguments")
    } else {
      if (is.null(y.coord$min) || is.null(y.coord$max)) {
        stop ("'min' and 'max' are mandatory subarguments for 'y.coord")
      }
    }
    if (finite) {
      if (missing(candidates)) {
        stop ("'candidates' is a mandatory argument")
      }
      if (!inherits(candidates, what = "data.frame")) {
        stop ("'candidates' should be a data.frame")
      } else {
        if (dim(candidates)[2] != 2 || 
              any(colnames(candidates) != c("x", "y"))) {
          stop ("'candidates' should have 2 columns named 'x' and 'y'")
        }
      }
      if (unique(which == "all")) {
        stop ("this option is not functional yet")
      } else {
        if (length(which) > 1) {
          stop ("this option is not functional yet")
        }
      }
    } else {
      if (missing(where)) {
        stop ("'where' is a mandatory argument")
      } else {
        if (!inherits(where, what = "SpatialPolygons")) {
          stop ("'where' should be of class SpatialPolygons")
        } else {
          where <- as(where, "SpatialPolygons")
        }
      }
      if (!is.numeric(zero)) {
        stop ("'zero' should be a numeric value")
      }
      if (!is.numeric(iterations)) {
        stop ("'iterations' should be a numeric value")
      }
    }
    res <- list(finite = finite, candidates = candidates, where = where,
                x.coord = x.coord, y.coord = y.coord, zero = zero,  
                iterations = iterations, verbose = verbose)
    return (res)
  }
# End!