#' Evaluation of local geostatistical models of uncertainty
#' 
#' Evaluate the accuracy and precision of a local geostatistical model of
#' uncertainty (\bold{GMU}) using summary measures and graphical displays.
#' 
#' @param observed Vector of observed values (validation points).
#' 
#' @param simulated Data frame or matrix with simulated values (columns) for
#' each validation point (rows).
#' 
#' @param pi Vector defining the width of the series of symmetric 
#' \emph{p}-probability intervals. Defaults to 
#' \code{pi = seq(0.01, 0.99, 0.01)}.
#' 
#' @param plotit Logical for plotting the results. Defaults to 
#' \code{plotit = TRUE}.
#' 
#' @details  
#' 
#' @references 
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @example 
set.seed(2001)
observed <- round(rnorm(20), 3)
simulated <- t(sapply(1:length(observed), function (i) round(rnorm(100), 3)))
res <- checkGMU(observed, simulated)
res$summary.stats
# FUNCTION #####################################################################
checkGMU <-
  function (observed, simulated, pi = seq(0.01, 0.99, 0.01), plotit = TRUE) {
    
    # Initial settings
    n_pts <- length(observed)
    n_pis <- length(pi)
    
    # Compute the symmetric p-probability intervals
    pi_bounds <- sapply(1:length(pi), function (i) c(1 - pi[i], 1 + pi[i]) / 2)
    
    # Check if the true values fall into each of the symmetric p-probability
    # intervals
    fall <- matrix(nrow = n_pts, ncol = n_pis)
    width <- matrix(nrow = n_pts, ncol = n_pis)
    message("Processing ", n_pis, " symmetric p-probability intervals...")
    for (i in 1:n_pts) {
      for (j in 1:n_pis) {
        bounds <- quantile(simulated[i, ], pi_bounds[, j])
        fall[i, j] <- bounds[1] < observed[i] & observed[i] <= bounds[2]
        width[i, j] <- diff(bounds)
      }
    }
    
    # Compute the proportion of true values that fall into each of the 
    # symmetric p-probability intervals
    count <- apply(fall, 2, sum)
    prop <- count / n_pts
    
    # Compute the average width of the symmetric p-probability intervals
    width <- width * fall
    local_width <- apply(width, 2, sum) / count
    
    # Compute summary statistics
    accu <- prop >= pi
    pi_idx <- which(accu)
    accu <- sum(prop >= pi) / n_pis # accuracy (Deutsch, 1997)
    prec <- 1 - 2 * sum(prop[pi_idx] - pi[pi_idx]) / n_pis # precision
    pi_w <- ifelse(1:n_pis %in% pi_idx, 1, 2)
    good <- 1 - (sum(pi_w * abs(prop - pi)) / n_pis) # goodness (Deutsch, 1997)
    pred <- apply(simulated, 1, mean)
    pred_var <- apply(simulated, 1, var)
    uncer <- mean(pred_var) # Uncertainty (Deutsch, 1997)
    err <- pred - observed
    me <- mean(err) # mean error
    serr <- err ^ 2
    mse <- mean(serr) # mean squared error
    srmse <- mean(serr / pred_var) # scaled root mean squared error
    corr <- cor(pred, observed) # linear correlation
    stats <- round(data.frame(me = me, mse = mse, srmse = srmse, cor = corr, 
                              A = accu, P = prec, G = good, U = uncer), 4)
    
    if (plotit) {
      on.exit(par())
      par(mfrow = c(2, 2))
      cex <- ifelse(n_pts > 10, 0.5, 1)
      
      # Plot accuracy plot
      plot(0:1, 0:1, type = 'n', main = "Accuracy plot", 
           xlab = "Probability interval - p", 
           ylab = "Proportion in this interval")
      abline(a = 0, b = 1)
      points(x = pi, y = prop, cex = cex)
      text(x = 0, y = 1, labels = "More accurate", pos = 4)
      text(x = 1, y = 0, labels = "Less accurate", pos = 2)
      
      # Plot PI-width plot
      plot(x = pi, local_width, main = "PI-width plot", 
           xlab = "Probability interval - p", 
           ylab = "Width of this interval", cex = cex)
      
      # Plot observed vs simulated values
      lim <- range(c(observed, pred))
      plot(x = observed, pred, main = "Observed vs Simulated", 
           xlab = "Observed value", ylim = lim, xlim = lim,
           ylab = "Average simulated value", cex = cex)
      abline(a = 0, b = 1)
      
      # Plot box plots
      idx <- 1:n_pts
      idx <- idx[order(rank(observed))]
      if (n_pts > 100) { 
        sub_idx <- round(seq(1, n_pts, length.out = 100))
        boxplot(simulated[, idx[sub_idx]], col = "yellow", 
                pars = list(cex = cex), names = idx[sub_idx])
        points(observed[idx[sub_idx]], col = "red", pch = 17, cex = cex)
        xlab <- "Validation point (max of 100)"
      } else {
        boxplot(simulated[, idx], col = "yellow", pars = list(cex = cex),
                names = idx, xlab = "Validation point")
        points(observed[idx], col = "red", pch = 17, cex = cex)
        xlab <- "Validation point"
      }
      title(main = "Distribution of simulated values",
            xlab = xlab, ylab = "Simulated and observed values")
      
    }
    
    # Output
    res <- list(data.frame(pi = pi, prop = prop, local.width = local_width), 
                summary.stats = stats)
    return (res)
    
  }
