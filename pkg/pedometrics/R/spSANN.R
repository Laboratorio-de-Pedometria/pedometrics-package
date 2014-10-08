#  file pedometrics/R/spSANN.R
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
#  Purpose        : spatial simulated annealing
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : G. Heuvelink (gerard.heuvelink@wur.nl)
#                   E. Pebesma (edzer.pebesma@uni-muenster.de)
#                   J. Skoien (jon.skoien@gmail.com)
#
# get objective function value
energyState <- 
  function (fun, obj, ...) {
    if (missing(fun) || missing(obj)) {
      stop ("'fun' and 'obj' are mandatory arguments")
    }
    return (do.call(fun, list(obj, ...)))
  }
# controls for spJitter
spJitter.control <-
  function (x.coord = list(min = 1, max = NULL, factor = 0.5),
            y.coord = list(min = 1, max = NULL, factor = 0.5),
            candidates = NULL, localGrid = TRUE,
            zero = 1, where = NULL, iterations = 10000, verbose = FALSE) {
    if (!is.list(x.coord) && length(x.coord) != 3) {
      stop ("'x.coord' should be a list with 3 subarguments")
    }
    if (!is.list(y.coord) && length(y.coord) != 3) {
      stop ("'y.coord' should be a list with 3 subarguments")
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
      if (!inherits(candidates, "SpatialPoints") || 
          is.na(proj4string(candidates)) || !is.projected(candidates)) {
        stop ("'candidates' should be of class SpatialPoints with a projected CRS")
      } else {
          candidates <- as(candidates, "SpatialPoints")
          colnames(candidates@coords) <- c("x", "y")
          rownames(candidates@bbox) <- c("x", "y")
        }
    }
    res <- list(x.coord = x.coord, y.coord = y.coord, candidates = candidates,
                zero = zero, where = where, iterations = iterations, 
                verbose = verbose, localGrid = localGrid)
    return (res)
  }
# spatial simulated annealing
spSANN <-
  function (obj, fun, iterations = 10000, spJitter.ctrl = spJitter.control(),
            max.count = 200, initial.prob = 0.2, size = 1, size.factor = 10,
            cooling.factor = iterations / 10,
            progress = TRUE, plotit = TRUE, verbose = TRUE, ...) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints") || is.na(proj4string(obj)) || 
            !is.projected(obj)) {
        stop ("'obj' should be of class SpatialPoints with a projected CRS")
      }  
    }
    if (missing(fun)) {
      stop ("'fun' is a mandatory argument")
    } else {
      if (!is.function(fun)) {
        stop ("'fun' should be a function")
      }  
    }
    if (!is.numeric(iterations) || length(iterations) > 1) {
      stop ("'iterations' should be a numeric value")
    }
    if (is.null(spJitter.ctrl$x.coord$max)) {
      dx <- bbox(obj)["x", "max"] - bbox(obj)["x", "min"]
      spJitter.ctrl$x.coord$max <- dx * spJitter.ctrl$x.coord$factor
    } else {
      spJitter.ctrl$x.coord$max <- spJitter.ctrl$x.coord$max
    }
    if (is.null(spJitter.ctrl$y.coord$max)) {
      dy <- bbox(obj)["y", "max"] - bbox(obj)["y", "min"]
      spJitter.ctrl$y.coord$max <- dy * spJitter.ctrl$y.coord$factor
    } else {
      spJitter.ctrl$y.coord$max <- spJitter.ctrl$y.coord$max
    }
    if (!is.numeric(max.count) || length(max.count) > 1) {
      stop ("'max.count' should be a numeric value")
    }
    if (!is.numeric(initial.prob) || length(initial.prob) > 1 || initial.prob <= 0) {
      stop ("'initial.prob' should be a positive numeric value")
    }
    if (!is.numeric(cooling.factor) || length(cooling.factor) > 1 || cooling.factor < 1) {
      stop ("'cooling.factor' should be a positive numeric value")
    }
    if (!is.numeric(size) || length(size) > 1 || size < 1) {
      stop ("'size' should be a positive numeric value")
    }
    if (!is.numeric(size.factor) || length(size.factor) > 1 || size.factor < 1) {
      stop ("'cooling.factor' should be a positive numeric value")
    }
    if (!is.logical(progress)) {
      stop ("'progress' should be a logical value")
    }
    if (!is.logical(plotit)) {
      stop ("'plotit' should be a logical value")
    }
    if (!is.logical(verbose)) {
      stop ("'verbose' should be a logical value")
    }
    time0 <- proc.time()
    n_pts <- length(obj)
    sys_config0 <- obj
    old_sys_config <- sys_config0
    energy_state0 <- energyState(fun = fun, obj = old_sys_config, ...)
    old_energy_state <- energy_state0
    count <- 0
    nr_designs <- 1
    best_energy_state <- Inf
    energy_states <- vector()
    if (progress) {
      pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    }
    for (k in 1:iterations) {
      if (size > 1) {
        new_size <- round(c(size - 1) * exp(-k / size.factor) + 1)
        #new_size <- round(size - k / iterations * (size - 1))
      } else {
        new_size <- size
      }
      id <- sample(c(1:n_pts), size = new_size)
      new_sys_config <- spJitter(old_sys_config, which = id,
                                 candidates = spJitter.ctrl$candidates,
                                 x.coord = spJitter.ctrl$x.coord,
                                 y.coord = spJitter.ctrl$y.coord,
                                 zero = spJitter.ctrl$zero,
                                 where = spJitter.ctrl$where,
                                 iterations = spJitter.ctrl$iterations,
                                 verbose = spJitter.ctrl$verbose)
      a <- spJitter.ctrl$x.coord$max
      b <- spJitter.ctrl$x.coord$min
      spJitter.ctrl$x.coord$max <- a - k / iterations * (a - b)
      a <- spJitter.ctrl$y.coord$max
      b <- spJitter.ctrl$y.coord$min
      spJitter.ctrl$y.coord$max <- a - k / iterations * (a - b)
      new_energy_state <- energyState(fun = fun, obj = new_sys_config, ...)
      random_prob <- runif(1)
      actual_prob <- initial.prob * exp(-k / cooling.factor)
      if (new_energy_state <= old_energy_state) {
        old_sys_config <- new_sys_config
        old_energy_state <- new_energy_state
        nr_designs <- nr_designs + 1
        count <- 0
        } else {
          if (new_energy_state > old_energy_state & random_prob <= actual_prob) {
            old_sys_config <- new_sys_config
            old_energy_state <- new_energy_state
            nr_designs <- nr_designs + 1
            count <- count + 1
            if (verbose) {
              rp <- random_prob
              if (count == 1) {
                cat("\n", count, "iteration with no improvement... p = ", rp, "\n")
              } else {
                cat("\n", count, "iterations with no improvement... p = ", rp, "\n")
                }
            }
            } else {
              new_energy_state <- old_energy_state
              new_sys_config <- old_sys_config
              nr_designs <- nr_designs
              count <- count + 1
              if (verbose) {
                mc <- max.count
                if (count == 1) {
                  cat("\n", count, "iteration with no improvement... stops at", mc, "\n")
                } else {
                  cat("\n", count, "iterations with no improvement... stops at", mc, "\n")
                }
              }
            }
        }
      energy_states[k] <- new_energy_state
      if (new_energy_state < best_energy_state / 1.0000001) {
        best_k <- k
        best_sys_config <- new_sys_config
        best_energy_state <- new_energy_state
        best_old_energy_state <- old_energy_state
        old_sys_config <- old_sys_config
      }
      if (plotit){
        par0 <- par()
        par(mfrow = c(1, 2))
        if (any(round(seq(1, iterations, 10)) == k)) {
          a <- c(energy_state0, energy_states[1:k])
          plot(a ~ c(0:k), type = "l", xlab = "iteration", ylab = "energy state")
          abline(h = energy_state0, col = "red")
          if (is.null(spJitter.ctrl$where)) {
            plot(new_sys_config, pch = 19, cex = 0.5)
          } else {
            plot(spJitter.ctrl$where)
            points(new_sys_config, pch = 19, cex = 0.5)
          }
        }
      }
      if (count == max.count) {
        if (new_energy_state > best_energy_state * 1.000001) {
          old_sys_config <- old_sys_config
          new_sys_config <- best_sys_config
          old_energy_state <- best_old_energy_state
          new_energy_state <- best_energy_state
          nr_designs <- nr_designs + 1
          count <- 0
          cat("\n", "reached 'max.count' with suboptimal system configuration\n")
          cat("\n", "restarting with previously best system configuration\n")
          mc <- max.count
          if (count == 1) {
            cat("\n", count, "iteration with no improvement... stops at", mc, "\n")
          } else {
            cat("\n", count, "iterations with no improvement... stops at", mc, "\n")
          }
        } else {
          break
        }
      }
      if (progress) {
        setTxtProgressBar(pb, k)
      }
    }
    if (progress) {
      close(pb)
    }
    if (plotit) {
      par <- par0
    }
    res <- list(object = new_sys_config, criterion = c(energy_state0, energy_states))
    running_time <- (proc.time() - time0) / 60
    cat("running time = ", round(running_time[3], 2), " minutes", sep = "")
    return (res)
  }
# End!
