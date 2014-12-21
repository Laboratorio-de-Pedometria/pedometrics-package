#' Optimization of spatial samples for trend estimation
#' 
#' Optimizes spatial samples for trend estimaton using spatial simulated
#' annealing.
#' 
#' @template spJitter_doc
#' @template spSANN_doc
#' 
# FUNCTION - MAIN ##############################################################
spsannTrend <-
  function (points, candidates, covariates, continuous = TRUE, pre.distri = 1,
            weights = list(strata = 0.5, correl = 0.5), use.coords = FALSE,
            strata.type = "equal.area", sim.nadir = NULL,
            
            x.max, x.min, y.max, y.min, iterations,
            acceptance = list(initial = 0.99, cooling = iterations / 10),
            stopping = list(max.count = iterations / 10), plotit = TRUE,
            boundary, progress = TRUE, verbose = TRUE) {
    if (ncol(candidates) != 3) stop ("'candidates' must have three columns")
    if (sum(unlist(weights)) != 1) stop ("the 'weights' must sum to 1")
    if (plotit) par0 <- par()
    if (is.integer(points)) {
      n_pts <- points
      points <- sample(c(1:dim(candidates)[1]), n_pts)
      points <- candidates[points, ]
    } else {
      n_pts <- nrow(points)
    }
    sys_config0 <- points
    old_sys_config <- sys_config0
    
    # Prepare covariates and create the starting 'sample matrix'
    if (use.coords) {
      covariates <- data.frame(covariates, candidates[, 2:3])
    }
    if (!is.data.frame(covariates)) covariates <- as.data.frame(covariates)
    n_cov <- ncol(covariates)
    samp_mat <- covariates[points[, 1], ]
    if (n_cov == 1) {
      samp_mat <- data.frame(samp_mat)
    }
    
    # Base data and initial energy state
    # Continuous or categorical covariates?
    if (continuous) {
      # ASR: we should compute the true population correlation matrix
      pop_cor_mat <- cor(covariates)
      strata <- .contStrata(n_pts, covariates, strata.type)
      cont_distri <- .contDistri(pre.distri, n_pts)
      nadir <- .contNadir(pre.distri, n_pts, n_cov, pop_cor_mat, sim.nadir,
                          candidates, covariates, strata)
      samp_cor_mat <- cor(samp_mat)
      energy_state0 <- .objCont(samp_mat, strata, n_cov, pre.distri, 
                                pop_cor_mat, samp_cor_mat, nadir, weights)
    } else {
      # Compute core data for categorical covariates
      
      # Calculate the initial energy state
      
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
      
      # Continuous or categorical covariates?
      if (continuous) {
        # Update sample and correlation matrices; New energy state
        new_row <- covariates[new_sys_config[which_point, 1], ]
        new_samp_mat[which_point, ] <- new_row
        new_samp_cor_mat <- cor(new_samp_mat)
        new_energy_state <- .objCont(new_samp_mat, strata, n_cov, pre.distri, 
                                     pop_cor_mat, new_samp_cor_mat, nadir,
                                     weights)
        
      } else {
        # This is to update data for categorical covariates
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
  function (n.pts, covariates, strata.type) {
    if (strata.type == "equal.area") {
      probs <- seq(0, 1, length.out = n.pts + 1)
      strata <- sapply(covariates, quantile, probs = probs, na.rm = TRUE)
    }
    if (strata.type == "equal.range") {
      strata <- sapply(1:n_cov, function(i) seq(min(covariates[, i]), 
                                                max(covariates[, i]),
                                                length.out = n.pts + 1))
    }
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
            candidates, covariates, strata) {
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
        pts_mat <- covariates[pts, ]
        if (n.cov == 1) {
          pts_mat <- data.frame(pts_mat)
        }
        pts_cor_mat <- cor(pts_mat)
        breaks <- lapply(1:n.cov, function(i) list(pts_mat[, i], strata[, i]))
        counts <- sapply(breaks, function(x) hist(x[[1]], breaks = x[[2]],
                                                  plot = FALSE)$counts)
        r_counts <- rowSums(abs(counts - pre.distri))
        strata_nadir[i] <- sum(r_counts)
        correl_nadir[i] <- sum(abs(pop.cor.mat - pts_cor_mat))
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
