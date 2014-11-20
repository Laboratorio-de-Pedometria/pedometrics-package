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
.energyState <- 
  function (fun, points, candidates, ...) {
    return (do.call(fun, list(points, candidates, ...)))
  }
.updater <-
  function (fun, points, candidates, ...) {
    return (do.call(fun, list(points, candidates, ...)))
  }
# plotting
.spSANNplot <-
  function (energy0, energy_states, k, acceptance, accept_probs, 
            boundary, new_points, points0, y_max0, y_max, x_max0, 
            x_max) {
    par(mfrow = c(1, 2))
    a <- c(energy0, energy_states[1:k])
    plot(a ~ c(0:k), type = "l", xlab = "iteration", ylab = "energy state")
    abline(h = energy0, col = "red")
    a <- c(acceptance[[1]], accept_probs[1:k])
    par(new = TRUE)
    plot(a ~ c(0:k), type = "l", axes = FALSE, bty = "n", xlab = "", 
         ylab = "", col = "blue", ylim = c(0, acceptance[[1]]))
    axis(side = 4, at = pretty(range(a)))
    mtext("acceptance probability", side = 4, line = 3)
    bb <- bbox(boundary)
    plot(boundary)
    points(points0[, 1], points0[, 2], pch = 1, cex = 0.5, 
           col = "lightgray")
    lines(x = c(bb[1, 1], bb[1, 2]), y = rep(bb[2, 1], 2) - 0.02 * y_max0, 
          col = "gray", lwd = 12)
    lines(y = c(bb[2, 1], bb[2, 2]), x = rep(bb[1, 1], 2) - 0.02 * x_max0,
          col = "gray", lwd = 12)
    points(new_points[, 1], new_points[, 2], pch = 20, cex = 0.5)
    #lines(x = c(bb[1, 1], bb[1, 1] + x_max[k]),
    lines(x = c(bb[1, 1], bb[1, 1] + x.max),     
          y = rep(bb[2, 1], 2) - 0.02 * y_max0, col = "orange", lwd = 12)
    text(x = bb[1, 1] + (bb[1, 2] - bb[1, 1]) / 2, y = bb[2, 1] - 0.02 * y_max0,
         labels = "maximum shift in the X axis")
    #lines(y = c(bb[2, 1], bb[2, 1] + y_max[k]), 
    lines(y = c(bb[2, 1], bb[2, 1] + y.max),     
          x = rep(bb[1, 1], 2) - 0.02 * x_max0, col = "orange", lwd = 12)
    text(y = bb[2, 1] + (bb[2, 2] - bb[2, 1]) / 2, 
         x = bb[1, 1] - 0.02 * x_max0, 
         srt = 90, labels = "maximum shift in the Y axis")
  }
# spatial simulated annealing
spSANN <-
  function (points, candidates, x.max, x.min, y.max, y.min, fun, ...,
            iterations = 10000, plotit = TRUE, boundary,
            acceptance = list(initial = 0.99, cooling = iterations / 10),
            stopping = list(max.count = 200), progress = TRUE, 
            verbose = TRUE) {
    if (plotit){
      par0 <- par()
    }
    n_pts         <- length(points)
    points0       <- points
    old_points    <- points0
    energy0       <- .energyState(fun = fun, points = old_points, 
                                  candidates = candidates, ...)
    old_energy    <- energy0
    count         <- 0
    best_energy   <- Inf
    energy_states <- vector()
    accept_probs  <- vector()
    x_max0        <- x.max
    y_max0        <- y.max
    if (progress) {
      pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    }
    time0 <- proc.time()
    for (k in 1:iterations) {
      new_points <- spJitterFinite(old_points, candidates = candidates,
                                   x.max = x.max, x.min = x.min, y.max = y.max,
                                   y.min = y.min, 
                                   which.point = old_points[1])
      x.max <- x_max0 - (k / iterations) * (x_max0 - x.min)
      y.max <- y_max0 - (k / iterations) * (y_max0 - y.min)
      new_energy <- .energyState(fun = fun, points = new_points, 
                                 candidates = candidates, ...)
      random_prob <- runif(1)
      actual_prob <- acceptance[[1]] * exp(-k / acceptance[[2]])
      accept_probs[k] <- actual_prob
      if (new_energy <= old_energy) {
        old_points <- new_points
        old_energy <- new_energy
        count <- 0
      } else {
        if (new_energy > old_energy & random_prob <= actual_prob) {
          old_points <- new_points
          old_energy <- new_energy
          count <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... p = ", 
                random_prob, "\n")
          }
        } else {
          new_energy <- old_energy
          new_points <- old_points
          count <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... stops at",
                stopping[[1]], "\n")
          }
        }
      }
      energy_states[k] <- new_energy
      if (new_energy < best_energy / 1.0000001) {
        best_k <- k
        best_points <- new_points
        best_energy <- new_energy
        best_old_energy <- old_energy
        old_points <- old_points
      }
      if (any(round(seq(1, iterations, 10)) == k)) {
        if (plotit){
          .spSANNplot(energy0, energy_states, k, acceptance, accept_probs, 
                      boundary, new_points, points0, y_max0, y_max, 
                      x_max0, x_max)
        } 
      }
      if (count == stopping[[1]]) {
        if (new_energy > best_energy * 1.000001) {
          old_points <- old_points
          new_points <- best_points
          old_energy <- best_old_energy
          new_energy <- best_energy
          count <- 0
          cat("\n", "reached maximum count with suboptimal points\n")
          cat("\n", "restarting with previously best points\n")
          cat("\n", count, "iteration(s) with no improvement... stops at",
              stopping[[1]], "\n") 
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
    if (plotit){
      par(par0)
    }
    res <- new_points
    criterion <- c(energy0, energy_states)
    a <- attributes(res)
    a$energy.state <- criterion
    running_time <- (proc.time() - time0) / 60
    a$running.time <- running_time
    attributes(res) <- a
    cat("running time = ", round(running_time[3], 2), " minutes", sep = "")
    return (res)
  }
# spSANN <-
#   function (points, fun, iterations = 10000, spJitter.ctrl = spJitter.control(),
#             acceptance = list(initial = 0.99, cooling = iterations / 10),
#             stopping = list(max.count = 200), progress = TRUE, verbose = TRUE,
#             plotit = list(prob = TRUE, starting = TRUE, x.max = FALSE, 
#                           y.max = FALSE),
#             ...) {
#     if (missing(points)) {
#       stop ("'points' is a mandatory argument")
#     }
#     if (missing(fun) || !is.function(fun)) {
#       stop ("'fun' is a mandatory argument of class function")
#     }
#     if (!is.numeric(iterations) || length(iterations) > 1) {
#       stop ("'iterations' should be a numeric value") 
#     }
#     if (!is.numeric(stopping$max.count) || length(stopping$max.count) > 1) {
#       stop ("'max.count' should be a numeric value")
#     }
#     if (!is.list(acceptance) || length(acceptance) != 2) {
#       stop ("'acceptance' should be a list with 2 subarguments")
#     }
#     if (!is.logical(progress)) {
#       stop ("'progress' should be a logical value")
#     }
#     if (!is.logical(verbose)) {
#       stop ("'verbose' should be a logical value")
#     }
#     n_pts             <- length(points)
#     points0       <- points
#     old_points    <- points0
#     energy0     <- .energyState(fun = fun, points = old_points, ...)
#     old_energy  <- energy0
#     count             <- 0
#     best_energy <- Inf
#     energy_states     <- vector()
#     accept_probs      <- vector()
#     x_max             <- vector()
#     y_max             <- vector()
#     x_max0            <- spJitter.ctrl$x.coord$max
#     y_max0            <- spJitter.ctrl$y.coord$max
#     size              <- spJitter.ctrl$size
#     size.factor       <- spJitter.ctrl$size.factor
#     finite            <- spJitter.ctrl$finite
#     if (progress) {
#       pb <- txtProgressBar(min = 1, max = iterations, style = 3)
#     }
#     time0             <- proc.time()
#     for (k in 1:iterations) {
#       if (size > 1) {
#         new_size <- round(c(size - 1) * exp(-k / size.factor) + 1)
#       } else {
#         new_size <- size
#       }
#       if (finite) {
#         id <- sample(old_points, new_size)
#       } else {
#         id <- sample(c(1:n_pts), size = new_size) 
#       }
#       new_points <- spJitter(old_points, which = id,
#                                  finite = spJitter.ctrl$finite,
#                                  candidates = spJitter.ctrl$candidates,
#                                  x.coord = spJitter.ctrl$x.coord,
#                                  y.coord = spJitter.ctrl$y.coord,
#                                  zero = spJitter.ctrl$zero,
#                                  where = spJitter.ctrl$where,
#                                  iterations = spJitter.ctrl$iterations,
#                                  verbose = spJitter.ctrl$verbose)
#       b <- spJitter.ctrl$x.coord$min
#       spJitter.ctrl$x.coord$max <- x_max0 - (k / iterations) * (x_max0 - b)
#       x_max[k] <- spJitter.ctrl$x.coord$max
#       b <- spJitter.ctrl$y.coord$min
#       spJitter.ctrl$y.coord$max <- y_max0 - (k / iterations) * (y_max0 - b)
#       y_max[k] <- spJitter.ctrl$y.coord$max
#       new_energy <- .energyState(fun = fun, points = new_points, ...)
#       random_prob <- runif(1)
#       actual_prob <- acceptance$initial * exp(-k / acceptance$cooling)
#       accept_probs[k] <- actual_prob
#       if (new_energy <= old_energy) {
#         old_points <- new_points
#         old_energy <- new_energy
#         count <- 0
#         } else {
#           if (new_energy > old_energy & random_prob <= actual_prob) {
#             old_points <- new_points
#             old_energy <- new_energy
#             count <- count + 1
#             if (verbose) {
#               if (count == 1) {
#                 cat("\n", count, "iteration with no improvement... p = ", 
#                     random_prob, "\n")
#                 } else {
#                   cat("\n", count, "iterations with no improvement... p = ", 
#                       random_prob, "\n")
#                 }
#             }
#             } else {
#               new_energy <- old_energy
#               new_points <- old_points
#               count <- count + 1
#               if (verbose) {
#                 if (count == 1) {
#                   cat("\n", count, "iteration with no improvement... stops at",
#                       stopping$max.count, "\n")
#                   } else {
#                     cat("\n", count, "iterations with no improvement... stops at",
#                         stopping$max.count, "\n")
#                   }
#               }
#             }
#         }
#       energy_states[k] <- new_energy
#       if (new_energy < best_energy / 1.0000001) {
#         best_k <- k
#         best_points <- new_points
#         best_energy <- new_energy
#         best_old_energy <- old_energy
#         old_points <- old_points
#       }
#       if (!is.null(plotit)){
#         par0 <- par()
#         par(mfrow = c(1, 2))
#         if (any(round(seq(1, iterations, 10)) == k)) {
#           a <- c(energy0, energy_states[1:k])
#           plot(a ~ c(0:k), type = "l", xlab = "iteration", 
#                ylab = "energy state")
#           abline(h = energy0, col = "red")
#           a <- c(acceptance$initial, accept_probs[1:k])
#           par(new = TRUE)
#           plot(a ~ c(0:k), type = "l", axes = FALSE, bty = "n", 
#                xlab = "", ylab = "", col = "blue", 
#                ylim = c(0, acceptance$initial))
#           axis(side = 4, at = pretty(range(a)))
#           mtext("acceptance probability", side = 4, line = 3) 
#           if (is.null(spJitter.ctrl$where)) {
#             plot(new_points, pch = 20, cex = 0.5)
#             if (plotit[["starting"]]) {
#               points(points0, pch = 1, cex = 0.5, col = "lightgray") 
#             }
#             } else {
#               plot(spJitter.ctrl$where)
#               points(new_points, pch = 20, cex = 0.5)
#               if (plotit[["starting"]]) {
#                 points(points0, pch = 1, cex = 0.5, col = "lightgray")
#               }
#               bb <- bbox(spJitter.ctrl$where)
#               lines(x = c(bb[1, 1], bb[1, 2]),
#                     y = rep(bb[2, 1], 2) - 0.02 * y_max0,
#                     col = "gray", lwd = 12)
#               lines(x = c(bb[1, 1], bb[1, 1] + x_max[k]), 
#                     y = rep(bb[2, 1], 2) - 0.02 * y_max0, 
#                     col = "orange", lwd = 12)
#               text(x = bb[1, 1] + (bb[1, 2] - bb[1, 1]) / 2,
#                    y = bb[2, 1] - 0.02 * y_max0,
#                    labels = "maximum shift in the X axis")
#               lines(y = c(bb[2, 1], bb[2, 2]),
#                     x = rep(bb[1, 1], 2) - 0.02 * x_max0,
#                     col = "gray", lwd = 12)
#               lines(y = c(bb[2, 1], bb[2, 1] + y_max[k]),
#                     x = rep(bb[1, 1], 2) - 0.02 * x_max0,
#                     col = "orange", lwd = 12)
#               text(y = bb[2, 1] + (bb[2, 2] - bb[2, 1]) / 2, 
#                    x = bb[1, 1] - 0.02 * x_max0, 
#                    srt = 90, labels = "maximum shift in the Y axis")
#             }
#         }
#       }
#       if (count == stopping$max.count) {
#         if (new_energy > best_energy * 1.000001) {
#           old_points <- old_points
#           new_points <- best_points
#           old_energy <- best_old_energy
#           new_energy <- best_energy
#           count <- 0
#           cat("\n", "reached 'max.count' with suboptimal system configuration\n")
#           cat("\n", "restarting with previously best system configuration\n")
#           if (count == 1) {
#             cat("\n", count, "iteration with no improvement... stops at",
#                 stopping$max.count, "\n")
#           } else {
#             cat("\n", count, "iterations with no improvement... stops at",
#                 stopping$max.count, "\n")
#           }
#         } else {
#           break
#         }
#       }
#       if (progress) {
#         setTxtProgressBar(pb, k)
#       }
#     }
#     if (progress) {
#       close(pb)
#     }
#     if (!is.null(plotit)) {
#       par <- par0
#     }
#     res <- list(object = new_points, 
#                 criterion = c(energy0, energy_states))
#     running_time <- (proc.time() - time0) / 60
#     cat("running time = ", round(running_time[3], 2), " minutes", sep = "")
#     return (res)
#   }
# End!