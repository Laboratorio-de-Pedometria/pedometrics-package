#' Optimization of spatial samples for trend estimation
#' 
#' Optimize spatial samples for trend estimaton using spatial simulated
#' annealing.
#' 
#' @template spJitter_doc
#' @template spSANN_doc
#' @param covars Data frame or matrix with the covariates in the columns.
#' @param continuous Logical informing if the covariates are of type 
#' \sQuote{continuous} or \sQuote{categorical}. Defaults to 
#' \code{continuous = TRUE}.
#' @param pre.distri Numeric value setting the distribution of points throughout
#' the strata of the continuous covariates. Available options are 
#' \code{pre.distri = 1}, for a uniform distribution, and 
#' \code{pre.distri = 0.5} for sample points covering the extremes of the
#' distribution. Alternativelly, a vector of length equal to the number of
#' points setting the desired distribution. Defaults to \code{pre.distri = 1}.
#' @param weights List with two components setting the weights assigned to the
#' sampling strata/classes and the correlation/association measure. The weights
#' must sum to unity. Defaults to 
#' \code{weights = list(strata = 0.5, correl = 0.5)}.
#' @param use.coords Logical for using the geographic coordinates as covariates.
#' Defaults to \code{use.coords = FALSE}.
#' @param strata.type Character setting the type of strata to be used with 
#' continuous covariates. Available options are \code{"equal.area"} and 
#' \code{"equal.range"}. Defaults to \code{strata.type = "equal.area"}. See
#' \sQuote{Details} for more information.
#' @param sim.nadir Number of random realizations to estimate the nadir point.
#' Defaults to \code{sim.nadir = NULL}, that is, the maximum absolute value is
#' used as nadir point. \sQuote{Details} for more information.
#' 
#' @details
#' 
#' @return A matrix (the optimized sample points) with attributes (the evolution
#' of the energy state and the running time).
#' 
#' @references
#' Minasny, B.; McBratney, A. B. A conditioned Latin hypercube method for
#' sampling in the presence of ancillary information. \emph{Computers &
#' Geosciences}, v. 32, p. 1378-1388, 2006.
#' 
#' Minasny, B.; McBratney, A. B. Conditioned Latin Hypercube Sampling for
#' calibrating soil sensor data to soil properties. Chapter 9. Viscarra Rossel,
#' R. A.; McBratney, A. B.; Minasny, B. (Eds.) \emph{Proximal Soil Sensing}.
#' Amsterdam: Springer, p. 111-119, 2010.
#' 
#' Mulder, V. L.; de Bruin, S.; Schaepman, M. E. Representing major soil
#' variability at regional scale by constrained Latin hypercube sampling of
#' remote sensing data. \emph{International Journal of Applied Earth Observation
#' and Geoinformation}, v. 21, p. 301-310, 2013.
#' 
#' Roudier, P.; Beaudette, D.; Hewitt, A. A conditioned Latin hypercube sampling
#' algorithm incorporating operational constraints. 5th Global Workshop on
#' Digital Soil Mapping. Sydney: p. 227-231, 2012.
#' 
#' @author
#' Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' with contributions of Gerard Heuvelink \email{gerard.heuvelink@@wur.nl} and
#' Dick Brus \email{dick.brus@@wur.nl}
#' 
#' @note
#' Some of the solutions used here were found in the source code of other
#' R-packages. As such, the authors of those packages (\pkg{intamapInteractive}
#' - Edzer Pebesma <\email{edzer.pebesma@@uni-muenster.de}> and Jon Skoien
#' <\email{jon.skoien@@gmail.com}>; \pkg{clhs} - Pierre Roudier
#' <\email{roudierp@@landcareresearch.co.nz}>) are entitled 
#' \sQuote{contributors} to the R-package \pkg{pedometrics}.
#' 
#' @seealso
#' \code{\link[clhs]{clhs}}
#' @keywords spatial optimize
#' @concept simulated annealing
#' @examples
#' require(sp)
#' require(rgeos)
#' data(meuse.grid)
#' candidates <- meuse.grid[, 1:2]
#' coordinates(candidates) <- ~ x + y
#' gridded(candidates) <- TRUE
#' boundary <- as(candidates, "SpatialPolygons")
#' boundary <- gUnionCascaded(boundary)
#' candidates <- coordinates(candidates)
#' candidates <- matrix(cbind(c(1:dim(candidates)[1]), candidates), ncol = 3)
#' covars <- meuse.grid[, c(1, 2, 3, 4, 5)]
#' points <- 100
#' x.max <- diff(bbox(boundary)[1, ])
#' y.min <- x.min <- 40
#' y.max <- diff(bbox(boundary)[2, ])
#' res <- spsannTrend(points, candidates, covars, x.max = x.max, 
#'                    x.min = x.min, y.max = y.max, y.min = y.min, 
#'                    boundary = boundary, sim.nadir = 1000)
#' 
# MAIN FUNCTION ################################################################
spsannCLHS <-
  function (points, candidates, covars, continuous = TRUE, pre.distri = 1,
            weights = list(strata = 0.5, correl = 0.5), use.coords = FALSE,
            strata.type = "equal.area", sim.nadir = NULL,
            x.max, x.min, y.max, y.min, iterations,
            acceptance = list(initial = 0.99, cooling = iterations / 10),
            stopping = list(max.count = iterations / 10), plotit = TRUE,
            boundary, progress = TRUE, verbose = TRUE) {
    
    # Initial checks
    if (ncol(candidates) != 3) stop ("'candidates' must have three columns")
    if (nrow(candidates) != nrow(covars))
      stop ("'candidates' and 'covars' must have the same number of rows")
    if (sum(unlist(weights)) != 1) stop ("the 'weights' must sum to 1")
    if (plotit) par0 <- par()
    
    # Prepare sample points
    if (length(points) == 1) {
      n_pts <- points
      points <- sample(c(1:dim(candidates)[1]), n_pts)
      points <- candidates[points, ]
    } else {
      n_pts <- nrow(points)
    }
    sys_config0 <- points
    old_sys_config <- sys_config0
    
    # Prepare covars and create the starting 'sample matrix'
    if (use.coords) {
      if (!continuous) {
        coords <- data.frame(candidates[, 2:3])
        breaks <- .contStrata(n_pts, coords, strata.type)
        coords <- .cont2cat(coords, breaks)
        covars <- data.frame(covars, coords)
      } else {
        covars <- data.frame(covars, candidates[, 2:3])
      }
    }
    if (!is.data.frame(covars)) covars <- as.data.frame(covars)
    n_cov <- ncol(covars)
    samp_mat <- covars[points[, 1], ]
    if (n_cov == 1) {
      samp_mat <- data.frame(samp_mat)
    }
    
    # Base data and initial energy state
    if (continuous) { # Continuous covariates
      # ASR: we should compute the true population correlation matrix
      pop_cor_mat <- cor(covars, use = "complete.obs")
      strata <- .contStrata(n_pts, covars, strata.type)
      cont_distri <- .contDistri(pre.distri, n_pts)
      nadir <- .contNadir(pre.distri, n_pts, n_cov, pop_cor_mat, sim.nadir,
                          candidates, covars, strata)
      samp_cor_mat <- cor(samp_mat, use = "complete.obs")
      energy_state0 <- .objCont(samp_mat, strata, n_cov, pre.distri, 
                                pop_cor_mat, samp_cor_mat, nadir, weights)
      
    } else { # Categorical covariates
      pop_cor_mat <- cramer(covars)
      pop_prop <- sapply(covars, function(x) table(x) / nrow(covars))
      nadir <- .catNadir(sim.nadir, candidates, n_pts, covars, pop_prop,
                         pop_cor_mat, n_cov)
      samp_cor_mat <- cramer(samp_mat)
      energy_state0 <- .objCat(samp_mat, pop_prop, nadir, weights, pop_cor_mat,
                               samp_cor_mat, n_pts)
    }
    
    # Other settings for the simulated annealing algorithm
    old_samp_cor_mat <- samp_cor_mat
    new_samp_cor_mat <- samp_cor_mat
    best_samp_cor_mat <- samp_cor_mat
    old_samp_mat <- samp_mat
    new_samp_mat <- samp_mat
    best_samp_mat <- samp_mat
    count <- 0
    old_energy_state <- energy_state0
    best_energy_state <- Inf
    energy_states <- vector()
    accept_probs <- vector()
    x_max0 <- x.max
    y_max0 <- y.max
    if (progress) pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    time0 <- proc.time()
    
    # Begin the main loop
    for (k in 1:iterations) {
      
      # Jitter one of the points and update x.max and y.max
      which_point <- sample(c(1:n_pts), 1)
      new_sys_config <- spJitterFinite(old_sys_config, candidates, x.max, 
                                       x.min, y.max, y.min, which_point)
      x.max <- x_max0 - (k / iterations) * (x_max0 - x.min)
      y.max <- y_max0 - (k / iterations) * (y_max0 - y.min)
      
      # Update sample and correlation matrices, and energy state
      if (continuous) { # Continuous covariates
        new_row <- covars[new_sys_config[which_point, 1], ]
        new_samp_mat[which_point, ] <- new_row
        new_samp_cor_mat <- cor(new_samp_mat, use = "complete.obs")
        new_energy_state <- .objCont(new_samp_mat, strata, n_cov, pre.distri, 
                                     pop_cor_mat, new_samp_cor_mat, nadir,
                                     weights)
        
      } else { # Categorical covariates
        new_row <- covars[new_sys_config[which_point, 1], ]
        new_samp_mat[which_point, ] <- new_row
        new_samp_cor_mat <- cramer(new_samp_mat)
        new_energy_state <- .objCat(new_samp_mat, pop_prop, nadir, weights,
                                    pop_cor_mat, new_samp_cor_mat, n_pts)
      }
      
      # Evaluate the new system configuration
      random_prob <- runif(1)
      actual_prob <- acceptance[[1]] * exp(-k / acceptance[[2]])
      accept_probs[k] <- actual_prob
      if (new_energy_state <= old_energy_state) {
        old_sys_config   <- new_sys_config
        old_energy_state <- new_energy_state
        old_samp_mat   <- new_samp_mat
        old_samp_cor_mat <- new_samp_cor_mat
        count <- 0
      } else {
        if (new_energy_state > old_energy_state & random_prob <= actual_prob) {
          old_sys_config   <- new_sys_config
          old_energy_state <- new_energy_state
          old_samp_mat   <- new_samp_mat
          old_samp_cor_mat <- new_samp_cor_mat
          count            <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... p = ", 
                random_prob, "\n")
          }
        } else {
          new_energy_state <- old_energy_state
          new_sys_config   <- old_sys_config
          new_samp_mat   <- old_samp_mat
          new_samp_cor_mat <- old_samp_cor_mat
          count            <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... stops at",
                stopping[[1]], "\n")
          }
        }
      }
      # Best energy state
      energy_states[k] <- new_energy_state
      if (new_energy_state < best_energy_state / 1.0000001) {
        best_k                <- k
        best_sys_config       <- new_sys_config
        best_energy_state     <- new_energy_state
        best_old_energy_state <- old_energy_state
        old_sys_config        <- old_sys_config
        best_samp_mat       <- new_samp_mat
        best_old_samp_mat   <- old_samp_mat
        best_samp_cor_mat     <- new_samp_cor_mat
        best_old_samp_cor_mat <- old_samp_cor_mat
      }
      # Plotting
      if (plotit && any(round(seq(1, iterations, 10)) == k)) {
        .spSANNplot(energy_state0, energy_states, k, acceptance, 
                    accept_probs, boundary, new_sys_config[, 2:3],
                    sys_config0[, 2:3], y_max0, y.max, x_max0, x.max)
      }
      # Freezing parameters
      if (count == stopping[[1]]) {
        if (new_energy_state > best_energy_state * 1.000001) {
          old_sys_config   <- old_sys_config
          new_sys_config   <- best_sys_config
          new_samp_mat   <- best_samp_mat
          new_samp_cor_mat <- best_samp_cor_mat
          old_energy_state <- best_old_energy_state
          new_energy_state <- best_energy_state
          old_samp_mat   <- best_old_samp_mat
          old_samp_cor_mat <- best_old_samp_cor_mat
          count            <- 0
          cat("\n", "reached maximum count with suboptimal configuration\n")
          cat("\n", "restarting with previously best configuration\n")
          cat("\n", count, "iteration(s) with no improvement... stops at",
              stopping[[1]], "\n")
        } else {
          break
        }
      }
      if (progress) setTxtProgressBar(pb, k)
    }
    if (progress) close(pb)
    if (plotit) par(par0)
    res <- .spSANNout(new_sys_config, energy_state0, energy_states, time0)
    return (res)
  }
# INTERNAL FUNCTION - BREAKS FOR CONTINUOUS COVARIATES #########################
.contStrata <-
  function (n.pts, covars, strata.type) {
    if (strata.type == "equal.area") {
      probs <- seq(0, 1, length.out = n.pts + 1)
      strata <- sapply(covars, quantile, probs = probs, na.rm = TRUE)
    }
    if (strata.type == "equal.range") {
      strata <- sapply(1:n_cov, function(i) seq(min(covars[, i]), 
                                                max(covars[, i]),
                                                length.out = n.pts + 1))
    }
    strata <- data.frame(strata)
    return (strata)
  }
# INTERNAL FUNCTION - DISTRIBUTION FOR CONTINUOUS COVARIATES ###################
.contDistri <-
  function (pre.distri, n.pts) {
    if (length(pre.distri) > 1) {
      if (length(pre.distri) || sum(pre.distri) != n.pts) {
        stop(paste("'pre.distri' must be of length/sum ", n.pts, sep = ""))
      }
    }
    # Sample points covering the extremes of the marginal distribution
    if (pre.distri == 0.5) {
      pre.distri <- rep(0, n.pts)
      pre.distri[1] <- n.pts / 2
      pre.distri[n.pts] <- n.pts / 2
    }
    return (pre.distri)
  }
# INTERNAL FUNCTION - NADIR FOR CONTINUOUS COVARIATES ##########################
# Maximum absolute value: upper bound is equal to 100
# For a single covariate, the lower bound is equal to 0
# For the correlation matrix, the lower bound is always equal to 0
.contNadir <-
  function (pre.distri, n.pts, n.cov, pop.cor.mat, sim.nadir,
            candidates, covars, strata) {
    # Absolute nadir point
    if (is.null(sim.nadir)) {
      if (pre.distri == 1) {
        strata_nadir <- (2 * (n.pts - 1)) * n.cov / 100
      } else {
        if (pre.distri == 0.5) {
          strata_nadir <- 2 * n.pts * n.cov / 100
        } else {
          strata_nadir <- rep(0, n.pts)
          strata_nadir[which.min(pre.distri)] <- n.pts
          strata_nadir <- sum(abs(strata_nadir - pre.distri)) * n.cov / 100
        }
      }
      cor_nadir <- (2 * n.cov) + sum(abs(pop.cor.mat - diag(n.cov)))
      cor_nadir <- cor_nadir / 100
    } else {
      # Simulate the nadir point
      message("simulating nadir values...")
      strata_nadir <- vector()
      correl_nadir <- vector()
      for (i in 1:sim.nadir) {
        pts <- sample(c(1:dim(candidates)[1]), n.pts)
        samp_mat <- covars[pts, ]
        if (n.cov == 1) {
          samp_mat <- data.frame(samp_mat)
        }
        samp_cor_mat <- cor(samp_mat, use = "complete.obs")
        breaks <- lapply(1:n.cov, function(i) list(samp_mat[, i], strata[, i]))
        counts <- sapply(breaks, function(x) hist(x[[1]], breaks = x[[2]],
                                                  plot = FALSE)$counts)
        r_counts <- rowSums(abs(counts - pre.distri))
        strata_nadir[i] <- sum(r_counts)
        correl_nadir[i] <- sum(abs(pop.cor.mat - samp_cor_mat))
      }
      strata_nadir <- mean(strata_nadir) / 100
      correl_nadir <- mean(correl_nadir) / 100
    }
    res <- list(strata = strata_nadir, correl = correl_nadir)
    return (res)
  }
# INTERNAL FUNCTION - CRITERION FOR CONTINUOUS COVARIATES ######################
.objCont <-
  function (samp.mat, strata, n.cov, pre.distri, pop.cor.mat,
            samp.cor.mat, nadir, weights) {
    breaks <- lapply(1:n.cov, function(i) list(samp.mat[, i], 
                                               strata[, i]))
    counts <- sapply(breaks, function(x) hist(x[[1]], breaks = x[[2]],
                                              plot = FALSE)$counts)
    r_counts <- rowSums(abs(counts - pre.distri))
    
    obj_cont <- sum(r_counts) / nadir[[1]]
    obj_cont <- obj_cont * weights[[1]]
    obj_cor <- sum(abs(pop.cor.mat - samp.cor.mat)) / nadir[[2]]
    obj_cor <- obj_cor * weights[[2]]
    res <- obj_cont + obj_cor
    return (res)
  }
# INTERNAL FUNCTION - NADIR FOR CATEGORICAL COVARIATES #########################
.catNadir <-
  function (sim.nadir, candi, n.pts, covars, pop.prop, pop.cor.mat, n.cov) {
    message("simulating nadir values...")
    strata_nadir <- vector()
    correl_nadir <- vector()
    for (i in 1:sim.nadir) {
      pts <- sample(c(1:dim(candi)[1]), n.pts)
      samp_mat <- covars[pts, ]
      if (n.cov == 1) {
        samp_mat <- data.frame(samp_mat)
      }
      samp_cor_mat <- cramer(samp_mat)
      samp_prop <- sapply(samp_mat, function(x) table(x) / n.pts)
      strata_nadir[i] <- sum(abs(samp_prop - pop.prop))
      correl_nadir[i] <- sum(abs(pop.cor.mat - samp_cor_mat))
    }
    strata_nadir <- mean(strata_nadir) / 100
    correl_nadir <- mean(correl_nadir) / 100
    res <- list(strata = strata_nadir, correl = correl_nadir)
    return (res) 
  }
# INTERNAL FUNCTION - CRITERION FOR CATEGORICAL COVARIATES #####################
.objCat <-
  function (samp.mat, pop.prop, nadir, weights, pop.cor.mat, samp.cor.mat, 
            n.pts) {
    samp_prop <- sapply(samp.mat, function(x) table(x) / n.pts)
    obj_cat <- sum(abs(samp_prop - pop.prop)) / nadir[[1]]
    obj_cat <- obj_cat * weights[[1]]
    obj_cor <- sum(abs(pop.cor.mat - samp.cor.mat)) / nadir[[2]]
    obj_cor <- obj_cor * weights[[2]]
    res <- obj_cat + obj_cor
    return (res)
  }
