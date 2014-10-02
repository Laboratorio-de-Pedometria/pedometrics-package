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
#  Contributions  : 
#
spJitter <- 
  function (obj, x.coord = list(min = 1, max = NULL, factor = 0.5),
            y.coord = list(min = 1, max = NULL, factor = 0.5), zero = 1, 
            which = "all", where  = NULL, iterations = 100, verbose = TRUE) {
    if (!inherits(obj, what = "SpatialPoints")) {
      stop ("obj should be of class SpatialPoints")
    }
    if (inherits(obj, what = "SpatialPointsDataFrame")) {
      obj_df <- obj@data
      obj <- as(obj, "SpatialPoints")
    }
    if (!is.list(x.coord)) {
      stop ("x.coord should be a list")
    }
    if (!is.list(y.coord)) {
      stop ("y.coord should be a list")
    }
    if (!is.numeric(zero)) {
      stop ("zero should be a numeric value")
    }
    if (!is.numeric(iterations)) {
      stop ("iterations should be a numeric value")
    }
    if (!is.logical(verbose)) {
      stop ("verbose should be a logical value")
    }
    if (!is.null(where)) {
      if (!inherits(where, what = "SpatialPolygons")) {
        stop ("where should be of class SpatialPolygons*")
      }
    }
    if (unique(which == "all")) {
      x0 <- coordinates(obj)[, "x"]
      y0 <- coordinates(obj)[, "y"]
    }
    if (is.numeric(which)) {
      x0 <- coordinates(obj)[which, "x"]
      y0 <- coordinates(obj)[which, "y"]
    }
    if (is.null(x.coord$max)) {
      dx <- bbox(obj)["x", "max"] - bbox(obj)["x", "min"]
      x.coord$max <- dx / (2 / x.coord$factor)
    } else {
      x.coord$max <- x.coord$max / 2
    }
    if (is.null(y.coord$max)) {
      dy <- bbox(obj)["y", "max"] - bbox(obj)["y", "min"]
      y.coord$max <- dy / (2 / y.coord$factor)
    } else {
      y.coord$max <- y.coord$max / 2
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
       x.coord$max <- x.coord$max - n_iter / iterations * (x.coord$max - x.coord$min)
       x1 <- jitter(x = x0, amount = x.coord$max)
       dx <- abs(x0 - x1)
       dx <- which(dx < x.coord$min)
       x1[dx] <- x0[dx] + sign(x0[dx] - x1[dx]) * x.coord$min
       y.coord$max <- y.coord$max - n_iter / iterations * (y.coord$max - y.coord$min)
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
    if (verbose) {
      if (!is.null(where)) {
        if (n_out >= 1) {
          message(paste("spJitter DID NOT converge after ", n_iter, 
                        " iterations", sep = ""))
          if (n_out > 1) {
            message(paste("there are ", n_out, 
                          " points outside the spatial domain", sep = ""))
          } else {
            message(paste("there is ", n_out, 
                          " point outside the spatial domain", sep = ""))
          }          
        } else {
          message(paste("spJitter converged after ", n_iter, " iterations",
                        sep = ""))
        }
      }
    }
    if (inherits(obj, what = "SpatialPointsDataFrame")) {
      res <- SpatialPointsDataFrame(res, obj_df)
    }
    return (res)
  }
# End!