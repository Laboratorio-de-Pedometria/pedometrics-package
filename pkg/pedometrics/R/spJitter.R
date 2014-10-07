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
  function (obj, candidates = NULL, 
            x.coord = list(min = 1, max = NULL, factor = 0.5),
            y.coord = list(min = 1, max = NULL, factor = 0.5), 
            zero = 1, which = "all", where  = NULL, iterations = 10000, 
            verbose = TRUE) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints") || is.na(proj4string(obj)) || 
            !is.projected(obj)) {
        stop ("'obj' should be of class SpatialPoints with a projected CRS")
      }
    }
    if (!is.null(candidates)) {
      if (!inherits(candidates, what = "SpatialPoints")) {
        stop ("'candidates' should be of class SpatialPoints")
      } else {
        candidates <- as(candidates, "SpatialPoints")
      }
    }
    if (!is.list(x.coord) || length(x.coord) != 3) {
      stop ("'x.coord' should be a list with 3 subarguments")
    } else {
      if (is.null(x.coord$max)) {
        dx <- bbox(obj)["x", "max"] - bbox(obj)["x", "min"]
        x.coord$max <- dx * x.coord$factor
      }
    }
    if (!is.list(y.coord) || length(y.coord) != 3) {
      stop ("'y.coord' should be a list with 3 subarguments")
    } else {
      if (is.null(y.coord$max)) {
        dy <- bbox(obj)["y", "max"] - bbox(obj)["y", "min"]
        y.coord$max <- dy * y.coord$factor
      } 
    }
    if (!is.numeric(zero)) {
      stop ("'zero' should be a numeric value")
    }
    if (!is.numeric(iterations)) {
      stop ("'iterations' should be a numeric value")
    }
    if (!is.logical(verbose)) {
      stop ("'verbose' should be a logical value")
    }
    if (!is.null(where)) {
      if (!inherits(where, what = "SpatialPolygons")) {
        stop ("'where' should be of class SpatialPolygons")
      } else {
        where <- as(where, "SpatialPolygons")
      }
    }
    if (!is.null(candidates)) {
      if (unique(which == "all")) {
        res <- candidates[sample(c(1:length(candidates)), length(obj)), ]
        message("distances were not taken into account")
      } else {
        if (length(which) > 1) {
          pt0 <- coordinates(obj[which, ])
          res <- list()
          for (i in 1:length(which)) {
            cand <- coordinates(candidates)
            d_x <- x.coord$max + x.coord$min
            d_y <- y.coord$max + y.coord$min
            d_x <- c(pt0[i, "x"] - d_x, pt0[i, "x"] + d_x)
            d_y <- c(pt0[i, "y"] - d_y, pt0[i, "y"] + d_y)
            cand <- which(cand[, "x"] >= d_x[1] & cand[, "x"] <= d_x[2] &
                            cand[, "y"] >= d_y[1] & cand[, "y"] <= d_y[2])
            res[i] <- candidates[sample(cand, 1), ]
          }
          res <- data.frame(t(sapply(res, coordinates)))
          colnames(res) <- c("x", "y")
          coordinates(res) <- ~ x + y
          proj4string(res) <- proj4string(obj)
          res <- rbind(obj[-which, ], res)
          return (res)
        } else {
          cand <- coordinates(candidates)
          d_x <- x.coord$max + x.coord$min
          d_y <- y.coord$max + y.coord$min
          pt0 <- coordinates(obj[which, ])
          d_x <- c(pt0[, "x"] - d_x, pt0[, "x"] + d_x)
          d_y <- c(pt0[, "y"] - d_y, pt0[, "y"] + d_y)
          cand <- which(cand[, "x"] >= d_x[1] & cand[, "x"] <= d_x[2] &
                          cand[, "y"] >= d_y[1] & cand[, "y"] <= d_y[2])
          cand <- candidates[sample(cand, 1), ]
          res <- rbind(obj[-which, ], cand)        
        } 
      }
      return (res)
    } else {
      if (unique(which == "all")) {
        x0 <- coordinates(obj)[, "x"]
        y0 <- coordinates(obj)[, "y"]
      }
      if (is.numeric(which)) {
        x0 <- coordinates(obj)[which, "x"]
        y0 <- coordinates(obj)[which, "y"]
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
      proj4string(res) <- proj4string(obj)
      if (is.numeric(which)) {
        res <- rbind(obj[-which, ], res)
      }
      if (!is.null(where)) {
        out <- which(gContains(where, res, byid = TRUE) == FALSE)
        n_out <- length(out)
        n_iter <- 1
        while (n_out >= 1) {
          res_out <- obj[out, ]
          res <- res[-out, ]
          x0 <- coordinates(res_out)[, "x"]
          y0 <- coordinates(res_out)[, "y"]
          #x.max <- x.coord$max - n_iter / iterations * (x.coord$max - x.coord$min)
          #x1 <- jitter(x = x0, amount = x.max)
          x1 <- jitter(x = x0, amount = x.coord$max)
          dx <- abs(x0 - x1)
          dx <- which(dx < x.coord$min)
          x1[dx] <- x0[dx] + sign(x0[dx] - x1[dx]) * x.coord$min
          #y.max <- y.coord$max - n_iter / iterations * (y.coord$max - y.coord$min)
          #y1 <- jitter(x = y0, amount = y.max)
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
            message(paste("spJitter converged after ", n_iter, " iterations",
                        sep = ""))
          }
        }
      }
      return (res)
    }
  }
# End!