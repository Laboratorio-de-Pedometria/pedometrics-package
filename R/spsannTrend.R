# FUNCTION - MAIN ##############################################################
spsannPPL <-
  function (points, candidates, covariates, continuous = TRUE, pre.distri = 1,
            weights = list(strata = 0.5, correl = 0.5), coordinates = FALSE,
            
            x.max, x.min, y.max, y.min, iterations = 10000,
            acceptance = list(initial = 0.99, cooling = iterations / 10),
            stopping = list(max.count = iterations / 10), plotit = TRUE,
            boundary, progress = TRUE, verbose = TRUE) {
    if (plotit) par0 <- par()
    n_pts  <- points
    points <- sample(c(1:dim(candidates)[1]), n_pts)
    
    # Prepare covariates and create the first 'design matrix'
    if (coordinates) {
      covariates <- data.frame(covariates, candidates[, 2:3])
    }
    if (!is.data.frame(covariates)) covariates <- as.data.frame(covariates)
    n_cov <- ncol(covariates)
    design_mat <- covariates[points, ]
    
    # Sample the points internaly
    points         <- candidates[points, ]
    sys_config0    <- points
    old_sys_config <- sys_config0
    
    # Continuous or categorical covariates?
    if (continuous) {
      # Compute core data for continuous covariates
      pop_cor_mat  <- cor(covariates)
      cont_strata  <- .getContStrata(n_pts, covariates)
      cont_distri  <- .getContDistri(pre.distri, n_pts)
      cont_nadir   <- .getContNadir(cont_distri, n_pts, n_cov)
      
      # Calculate the initial energy state
      samp_cor_mat   <- cor(design_mat)
      pts_per_strata <- .getPointsPerStrataCont(design_mat, cont_strata, 
                                                cont_distri, cont_nadir, n_cov)
      energy_state0  <- .objTrendCont(pts_per_strata, pop_cor_mat,
                                      samp_cor_mat, weights)
    } else {
      # Compute core data for categorical covariates
      
      # Calculate the initial energy state
      
    }
    # Other settings for the simulated annealing algorithm
    old_samp_cor_mat  <- samp_cor_mat
    new_samp_cor_mat  <- samp_cor_mat
    best_samp_cor_mat <- samp_cor_mat
    old_design_mat    <- design_mat
    new_design_mat    <- design_mat
    best_design_mat   <- design_mat
    count             <- 0
    old_energy_state  <- energy_state0
    best_energy_state <- Inf
    energy_states     <- vector()
    accept_probs      <- vector()
    x_max0            <- x.max
    y_max0            <- y.max
    if (progress) pb <- txtProgressBar(min = 1, max = iterations, style = 3)
    time0 <- proc.time()
    
    # begin the main loop
    for (k in 1:iterations) {
      
      # jitter one of the points and update x.max and y.max
      which_point <- sample(c(1:n_pts), 1)
      new_sys_config <- spJitterFinite(old_sys_config, candidates, x.max, 
                                       x.min, y.max, y.min, which_point)
      x.max <- x_max0 - (k / iterations) * (x_max0 - x.min)
      y.max <- y_max0 - (k / iterations) * (y_max0 - y.min)
      
      # Continuous or categorical covariates?
      if (continuous) {
        # update the design and correlation matrices
        new_design_mat <- design_mat
        new_row <- covariates[new_sys_config[which_point, 1], ]
        new_design_mat[which_point, ] <- new_row
        new_samp_cor_mat <- cor(new_design_mat)
        
        # calculate the new energy state
        pts_per_strata <- .getPointsPerStrataCont(new_design_mat, cont_strata, 
                                                  cont_distri, cont_nadir, n_cov)
        new_energy_state <- .objTrendCont(pts_per_strata, pop_cor_mat,
                                          new_samp_cor_mat, weights)
      } else {
        new_design_mat
        new_samp_cor_mat
        pts_per_strata
        new_energy_state
      }
      
      # evaluate the new system configuration
      random_prob     <- runif(1)
      actual_prob     <- acceptance[[1]] * exp(-k / acceptance[[2]])
      accept_probs[k] <- actual_prob
      if (new_energy_state <= old_energy_state) {
        old_sys_config   <- new_sys_config
        old_energy_state <- new_energy_state
        old_design_mat   <- new_design_mat
        old_samp_cor_mat <- new_samp_cor_mat
        count <- 0
      } else {
        if (new_energy_state > old_energy_state & random_prob <= actual_prob) {
          old_sys_config   <- new_sys_config
          old_energy_state <- new_energy_state
          old_design_mat   <- new_design_mat
          old_samp_cor_mat <- new_samp_cor_mat
          count <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... p = ", 
                random_prob, "\n")
          }
        } else {
          new_energy_state <- old_energy_state
          new_sys_config   <- old_sys_config
          new_design_mat   <- old_design_mat
          new_samp_cor_mat <- old_samp_cor_mat
          count <- count + 1
          if (verbose) {
            cat("\n", count, "iteration(s) with no improvement... stops at",
                stopping[[1]], "\n")
          }
        }
      }
      # Best energy state
      energy_states[k] <- new_energy_state
      if (new_energy_state < best_energy_state / 1.0000001) {
        best_k <- k
        best_sys_config       <- new_sys_config
        best_energy_state     <- new_energy_state
        best_old_energy_state <- old_energy_state
        old_sys_config        <- old_sys_config
        best_design_mat       <- new_design_mat
        best_old_design_mat   <- old_design_mat
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
          new_design_mat   <- best_design_mat
          new_samp_cor_mat <- best_samp_cor_mat
          old_energy_state <- best_old_energy_state
          new_energy_state <- best_energy_state
          old_design_mat   <- best_old_design_mat
          old_samp_cor_mat <- best_old_samp_cor_mat
          count <- 0
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
# INTERNAL FUNCTION - GET THE BREAKS FOR CONTINUOUS COVARIATES #################
.getContStrata <-
  function (n.strata, cont.covars) {
    probs <- seq(0, 1, length.out = n.strata + 1)
    breaks <- sapply(cont.covars, quantile, probs = probs, na.rm = TRUE)
    return (breaks)
  }
# INTERNAL FUNCTION - COMPUTE THE DESIRED DISTRIBUTION OF POINTS PER STRATA ####
.getContDistri <-
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
# INTERNAL FUNCTION - COMPUTE THE MAXIMUM ABSOLUTE VALUE (NADIR) ###############
.getContNadir <-
  function (pre.distri, n.pts, n.cov) {
    if (pre.distri == 1) {
      nadir <- (2 * (n.pts - 1)) * n.cov / 100
    }
    if (pre.distri == 0.5) {
      nadir <- 2 * n.pts * n.cov / 100
    } else {
      nadir <- rep(0, n.pts)
      nadir[which.min(pre.distri)] <- n.pts
      nadir <- sum(abs(nadir - pre.distri)) * n.cov / 100
    }
    return (nadir)
  }
# INTERNAL FUNCTION - COMPUTE THE NUMBER OF POINTS PER STRATA ##################
.getPointsPerStrataCont <-
  function (samples, strata, pre.distri, nadir, n.cov) {
    breaks <- lapply(1:n.cov, function(i) list(samples[, i], strata[, i]))
    counts <- sapply(breaks, function(x) hist(x[[1]], breaks = x[[2]],
                                              plot = FALSE)$counts)
    # The result is scaled between 0 and 100 dividing by the number of
    # covariates and strata (samples)
    res <- rowSums(abs(counts - pre.distri)) / nadir
    return (res)
  }
# INTERNAL FUNCTION - CALCULATE THE CRITERION FOR CONTINUOUS COVARIATES ########
.objTrendCont <-
  function (pts.per.strata, pop.cor.mat, samp.cor.mat,
            weights = list(strata = 0.5, correl = 0.5)) {
    obj_cor <- sum(abs(pop.cor.mat - samp.cor.mat)) * weights[[2]]
    obj_cont <- sum(pts.per.strata) * weights[[1]]
    res <- sum(obj_cont, obj_cor)
    return(res)
  }