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
# DOCUMENTATION ################################################################
#' 
#' @title Optimization of spatial samples for spatial interpolation
#' 
#' @description
#' This funtion optimizes spatial samples for spatial interpolation using 
#' spatial simulated annealing. The criterion used in the optimization is the 
#' mean squared shortest distance (MSSD).
#' Function to calculate the distance matrix between all grid cells. Function 
#' to calculate the distance to the nearest point. Function to identify the 
#' nearest point. Function to calculate the mean (squared) shortest distance 
#' between a set of points and all grid cells.
#' 
#' @keywords spatial
#' 
#' @template spJitter_doc
#' @template spSANN_doc
#' 
#' @details
#' \subsection{Distances}{
#' Euclidean distances between points are calculated using the function 
#' \code{\link[stats]{dist}}. This computation requires the coordinates to be 
#' projected. The user is responsible for making sure that this requirement is 
#' attained.
#' }
#' \subsection{Mean squared shortest distance}{
#' The function \code{objMSSD} is used in the optimization of spatial points 
#' for sampling. In a previous implementation, \code{objMSSD} would calculate 
#' the distance matrix at each iteration of the optimization algorithm. This is
#' computationally expensive. Thus, we decided to separate the calculation of 
#' the distance matrix, which is done using the function \code{distMat}. The 
#' user has to square the distance values to get the mean squared shortest 
#' distance -- this is done setting \code{exponent = 2} in \code{distMat}.
#' 
#' Once the distance matrix has been calculated, the algorithm has only to 
#' identify the subset of points in the prediction grid. Both 
#' \code{distToNearestPoint} and \code{nearestPoint} perform this operation. 
#' The calculation of the mean (squared) shortest distance does not require to 
#' know which is the nearest point -- we are only interested in knowing the 
#' distance to the nearest point. As such, \code{distToNearestPoint} is called 
#' internally by \code{objMSSD}.
#' 
#' \code{distToNearestPoint} and \code{nearestPoint} were constructed separately
#' because they are useful for other operations. For instance, 
#' \code{distToNearestPoint} can be used to build a map of distances to the 
#' nearest point, while \code{nearestPoint} can be used to define geographic 
#' strata.
#' }
#' \subsection{Utopia and nadir points}{
#' Knowledge of the utopia and nadir points can help in the construction of 
#' multi-objective optimization problems.
#' 
#' \code{objMSSD} is a bi-dimensional criterion because it explicitly takes 
#' into account both y and x coordinates. It aims at the spread of points in 
#' the geographic space. This is completely different from \code{objPairs} and 
#' \code{objPoints} which are uni-dimensional objective functions. They aim at 
#' the spread on points in the variogram space. It is more difficult to 
#' calculate the utopia and nadir points of a bi-dimensional criterion.
#' 
#' The \strong{utopia} (\eqn{f^{\circ}_{i}}) point of \code{objMSSD} is only 
#' known to be larger than zero. The \strong{nadir} (\eqn{f^{max}_{i}}) point 
#' is obtained when all points are clustered in one of the \dQuote{corners} of 
#' the spatial domain. This cannot be calculated and has to be simulated.
#' 
#' One strategy is to first optimize the set of points using \code{objMSSD} and
#' then create geographic strata. For the multi-objective optimization one would
#' then have to define an unidimensional criterion aiming at matching the 
#' optimal solution obtained by minimizing \code{objMSSD}.
#' 
#' One such unidimensional criterion would be the difference between the 
#' expected distribution and the observed distribution of points per geographic 
#' strata. This criterion would aim at having at least one point per geographic
#' strata. This is similar to what is done when using \code{objPairs} or 
#' \code{objPoints} -- there we use lag distance classes.
#' 
#' A second uni-dimensional criterion would be the difference between the 
#' expected MSSD and the observed MSSD. This criterion would aim at having the
#' points coinciding with the optimal solution obtained by minimizing 
#' \code{objMSSD}. In both cases the utopia point would be exactly zero 
#' (\eqn{f^{\circ}_{i} = 0}). The nadir point could be easily calculated for 
#' the first, but not for the second.
#' }
#' @return
#' \code{objMSSD} returns a numeric value: the mean (squared) shortest distance
#' between a set of points and all grid cells. \code{distMat} returns a square 
#' matrix. \code{distToNearestPoint} and \code{nearestPoint} return a matrix or
#' data.frame.
#' 
#' @references
#' Brus, D. J.; de Gruijter, J. J.; van Groenigen, J. W. Designing spatial 
#' coverage samples using the k-means clustering algorithm. In: P. Lagacherie, 
#' A. M.; Voltz, M. (Eds.) \emph{Digital soil mapping - an introductory 
#' perspective}. Elsevier, v. 31, p. 183-192, 2006.
#' 
#' de Gruijter, J. J.; Brus, D.; Bierkens, M.; Knotters, M. \emph{Sampling for 
#' natural resource monitoring}. Berlin: Springer, p. 332, 2006.
#' 
#' Walvoort, D. J. J.; Brus, D. J.; de Gruijter, J. J. An R package for spatial
#' coverage sampling and random sampling from compact geographical strata by 
#' k-means. \emph{Computers and Geosciences}. v. 36, p. 1261-1267, 2010.
#' 
#' @author
#' Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
#' 
#' Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
#' 
#' @note
#' The previous version included the possibility of using an infinite spatial 
#' domain. Some of the solutions were found in the source code of the R-package
#' \pkg{raster} from Robert J. Hijmans \email{r.hijmans@gmail.com}. Due to
#' operational issues, the use of an infinite spatial domain is no longer
#' supported.
#' 
#' @seealso \code{\link[raster]{distanceFromPoints}}, 
#' \code{\link[spcosa]{stratify}}.
#' 
#' @examples
#' require(sp)
#' data(meuse.grid)
#' meuse.grid <- meuse.grid[, 1:2]
#' #
#' # Distance matrix
#' d <- .distMat(meuse.grid, exponent = 2)
#' obj <- sample(dim(meuse.grid)[1], 15)
#' pts <- meuse.grid[obj, ]
#' plot(meuse.grid, asp = 1, pch = 15, col = "gray")
#' points(pts, pch = 19, col = 20, cex = 0.5)
#' #
#' # Means squared shortest distance
#' #
#' a <- .objMSSD(points = pts, dist.mat = d, pred.grid = meuse.grid)
#' #
#' # Distance to the nearest point
#' #
#' b <- .distToNearestPoint(dist.mat = d, which.pts = obj)
#' b <- cbind(meuse.grid, b)
#' coordinates(b) <- ~ x + y
#' gridded(b) <- TRUE
#' image(b)
#' points(pts, pch = 19, cex = 0.5)
#' #
#' # Nearest point (geographic strata)
#' #
#' e <- .nearestPoint(dist.mat = d, which.pts = obj)
#' e <- cbind(meuse.grid, e)
#' coordinates(e) <- ~ x + y
#' gridded(e) <- TRUE
#' image(e)
#' points(pts, pch = 19, cex = 0.5)
#'  
# FUNCTION - MAIN ##############################################################
spsannMeanSquareShortDist <-
  function (points, candidates, x.max, x.min, y.max, y.min, iterations = 10000,
            acceptance = list(initial = 0.99, cooling = iterations / 10),
            stopping = list(max.count = iterations / 10), plotit = TRUE,
            boundary, progress = TRUE, verbose = TRUE) {
    if (plotit){
      par0 <- par()
    }
    n_pts <- dim(points)[1]
    sys_config0 <- points
    old_sys_config <- sys_config0
    # calculate the initial energy state
    dist_mat <- rdist(candidates, sys_config0)
    #min_dist <- apply(dist_mat, 1, min)
    #min_dist <- rowMins(dist_mat)
    #energy_state0 <- mean(min_dist ^ 2)
    energy_state0 <- MSSDCpp(dist_mat)
    # other settings for the simulated annealing algorithm
    old_dist_mat <- dist_mat
    new_dist_mat <- dist_mat
    best_dist_mat <- dist_mat
    count <- 0
    old_energy_state <- energy_state0
    best_energy_state <- Inf
    energy_states <- vector()
    accept_probs <- vector()
    x_max0 <- x.max
    y_max0 <- y.max
    if (progress) {
      pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    }
    time0 <- proc.time()
    # begin the main loop
    for (k in 1:iterations) {
      # jitter one of the points and update x.max and y.max
      which_point <- sample(c(1:n_pts), 1)
      new_sys_config <- spJitterFiniteBeta(old_sys_config, candidates, x.max, 
                                           x.min, y.max, y.min, which_point)
      x.max <- x_max0 - (k / iterations) * (x_max0 - x.min)
      y.max <- y_max0 - (k / iterations) * (y_max0 - y.min)
      # update the distance matrix and calculate the new energy state
      #d <- rdist(candidates, matrix(new_sys_config[which_point, ], nrow = 1))
      #new_dist_mat <- old_dist_mat
      #new_dist_mat[, which_point] <- d
      x2 <- matrix(new_sys_config[which_point, ], nrow = 1)
      new_dist_mat <- updateMSSDCpp(candidates, x2, old_dist_mat, which_point)
      #min_dist <- apply(new_dist_mat, 1, min)
      #min_dist <- rowMins(new_dist_mat)
      #new_energy_state <- mean(min_dist ^ 2)
      new_energy_state <- MSSDCpp(new_dist_mat)
      # evaluate the new system configuration
      random_prob <- runif(1)
      actual_prob <- acceptance[[1]] * exp(-k / acceptance[[2]])
      accept_probs[k] <- actual_prob
      if (new_energy_state <= old_energy_state) {
        old_sys_config   <- new_sys_config
        old_energy_state <- new_energy_state
        old_dist_mat     <- new_dist_mat
        count <- 0
      } else {
        if (new_energy_state > old_energy_state & random_prob <= actual_prob) {
          old_sys_config   <- new_sys_config
          old_energy_state <- new_energy_state
          old_dist_mat     <- new_dist_mat
          count <- count + 1
          if (verbose) {
            if (count == 1) {
              cat("\n", count, "iteration with no improvement... p = ", 
                  random_prob, "\n")
            } else {
              cat("\n", count, "iterations with no improvement... p = ", 
                  random_prob, "\n")
            }
          }
        } else {
          new_energy_state <- old_energy_state
          new_sys_config   <- old_sys_config
          new_dist_mat     <- old_dist_mat
          count <- count + 1
          if (verbose) {
            if (count == 1) {
              cat("\n", count, "iteration with no improvement... stops at",
                  stopping[[1]], "\n")
            } else {
              cat("\n", count, "iterations with no improvement... stops at",
                  stopping[[1]], "\n")
            }
          }
        }
      }
      energy_states[k] <- new_energy_state
      if (new_energy_state < best_energy_state / 1.0000001) {
        best_k <- k
        best_sys_config       <- new_sys_config
        best_energy_state     <- new_energy_state
        best_old_energy_state <- old_energy_state
        old_sys_config        <- old_sys_config
        best_dist_mat         <- new_dist_mat
        best_old_dist_mat     <- old_dist_mat
      }
      
      # plotting
      if (any(round(seq(1, iterations, 10)) == k)) {
        if (plotit){
          .spSANNplot(energy_state0, energy_states, k, acceptance, 
                      accept_probs, boundary, new_sys_config, sys_config0,
                      y_max0, y.max, x_max0, x.max)
        } 
      }
      if (count == stopping[[1]]) {
        if (new_energy_state > best_energy_state * 1.000001) {
          old_sys_config   <- old_sys_config
          new_sys_config   <- best_sys_config
          new_dist_mat     <- best_dist_mat
          old_energy_state <- best_old_energy_state
          new_energy_state <- best_energy_state
          old_dist_mat     <- best_old_dist_mat
          count <- 0
          cat("\n", "reached maximum count with suboptimal system configuration\n")
          cat("\n", "restarting with previously best system configuration\n")
          if (count == 1) {
            cat("\n", count, "iteration with no improvement... stops at",
                stopping[[1]], "\n")
          } else {
            cat("\n", count, "iterations with no improvement... stops at",
                stopping[[1]], "\n")
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
    if (plotit){
      par(par0)
    }
    res <- new_sys_config
    criterion <- c(energy_state0, energy_states)
    a <- attributes(res)
    a$energy.state <- criterion
    running_time <- (proc.time() - time0) / 60
    a$running.time <- running_time
    attributes(res) <- a
    cat("running time = ", round(running_time[3], 2), " minutes", sep = "")
    return (res)
  }
# FUNCTION - DISTANCE MATRIX ###################################################
.distMat <- 
  function (candidates, exponent = 1, diagonal = 0) {
    dist_mat <- dist(candidates)
    dist_mat <- as.matrix(dist_mat)
    dist_mat <- dist_mat ^ exponent
    diag(dist_mat) <- diagonal
    return (dist_mat)
  }
# FUNCTION - DISTANCE TO NEAREST POINT #########################################
.distToNearestPoint <-
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
# FUNCTION - NEAREST POINT #####################################################
.nearestPoint <-
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
# FUNCTION - CALCULATE THE CRITERION VALUE #####################################
.objMSSD <-
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