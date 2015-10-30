#' Evaluation of local geostatistical models of uncertainty
#' 
#' Evaluate the quality of a local geostatistical model of uncertainty 
#' (\bold{GMU}) using summary measures and graphical displays.
#' 
#' @param observed Vector of observed values (validation points).
#' 
#' @param simulated Data frame or matrix with simulated values (columns) for
#' each validation point (rows).
#' 
#' @param pi Vector defining the width of the series of probability intervals.
#' Defaults to \code{pi = seq(0.01, 0.99, 0.01)}.
#' 
#' @param symmetric Logical for choosing the type of probability interval.
#' Defaults to \code{symmetric = TRUE}.
#' 
#' @param plotit Logical for plotting the results. Defaults to 
#' \code{plotit = TRUE}.
#' 
#' @details 
#' Several definitions are needed to correctly interpret the results of this
#' function.
#' 
#' \subsection{Accuracy}{
#' The definition of \emph{accuracy}, $A$, depends on the type of probability 
#' interval that is being used. For the general case (Papritz & Dubois, 1999),
#' a local GMU is said to be accurate if the proportion $p^*$ of true values 
#' falling within the $p$ probability interval is equal to $p$, that is, when 
#' $p^* = p$. Thus, a local GMU will be more accurate when all points in the 
#' coverage probability plot are exactly on the 1:1 line. For the symmetric 
#' case (Deutsch, 1997), an accurate local GMU is that for which the proportion 
#' $p^*$ of true values falling within the symmetric $p$ probability interval 
#' is equal or larger than $p$, that is, when $p^* \geq p$. Thus, a local GMU 
#' will be more accurate when all points in the coverage probability plot are 
#' on or above the 1:1 line. In both cases, $A$ ranges from 0 (lest accurate) 
#' to 1 (most accurate), with the symmetric case always presenting a larger $A$.
#' }
#' \subsection{Precision}{
#' The \emph{precision}, $P$, is defined only for an accurate local GMU, and
#' measures how close $p^*$ is to $p$ (Deutsch, 1997). For the general case
#' (Papritz & Dubois, 1999), $P$ is always equal to 1 because an accurate 
#' local GMU is defined by $p^* = p$, rendering the measure of precision 
#' useless. For the symmetric case, $P$ ranges from 0 (lest precise) to 1 (most 
#' precise).
#' }
#' \subsection{Goodness}{
#' The \emph{goodness}, $G$, is a measure of the departure of the points from
#' the 1:1 line in the coverage probability plot (Deutsch, 1997). Like $A$ and 
#' $P$, the definition of $G$ depends on the type of probability interval that 
#' is being used. Remember that, for the general case, innaccuracy is defined 
#' as $p^* < p$ and $p^* > p$, while for the symmetric case, innaccuracy is
#' defined only as $p^* < p$. In both cases, $G$ ranges from 0 (minimum 
#' goodness) to 1 (maximum goodness), the maximum $G$ being achieved when 
#' $p^* = p$, that is, all points in the coverage probability plot are exactly 
#' on the 1:1 line. However, because the innacurate case is weighted twice, the 
#' symmetric case always presents a larger $G$.
#' }
#' @references 
#' 
#' Deutsch, C. Direct assessment of local accuracy and precision. Baafi, E. Y. 
#' & Schofield, N. A. (Eds.) \emph{Geostatistics Wollongong '96}. Dordrecht:
#' Kinwer Academic Pubiishers, v. I, p. 115-125, 1997.
#' 
#' Papritz, A. & Dubois, J. R. Mapping heavy metals in soil by (non-)linear
#' kriging: an empirical validation. Gómez-Hernández, J.; Soares, A. & 
#' Froidevaux, R. (Eds.) \emph{geoENV II -- Geostatistics for Environmental
#' Applications}. Springer, p. 429-440, 1999.
#' 
#' Goovaerts, P. Geostatistical modelling of uncertainty in soil science.
#' \emph{Geoderma}. v. 103, p. 3 - 26, 2001.
#' 
#' Goovaerts, P. AUTO-IK: a 2D indicator kriging program for the automated 
#' non-parametric modeling of local uncertainty in earth sciences. 
#' \emph{Computers & Geosciences}. v. 35, p. 1255-1270, 2009.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @examples
#' set.seed(2001)
#' observed <- round(rnorm(100), 3)
#' simulated <- t(sapply(1:length(observed), function (i) round(rnorm(100), 3)))
#' resa <- checkGMU(observed, simulated, symmetric = T)
#' resb <- checkGMU(observed, simulated, symmetric = F)
#' round(resa$stats, 3)
#' round(resb$stats, 3)
# FUNCTION #####################################################################
checkGMU <-
  function (observed, simulated, pi = seq(0.01, 0.99, 0.01), symmetric = TRUE,
            plotit = TRUE) {
    
    # Initial settings
    n_pts <- length(observed)
    n_pis <- length(pi)
    
    # If required, compute the symmetric probability intervals
    if (symmetric) {
      pi_bounds <- 
        sapply(1:length(pi), function (i) c(1 - pi[i], 1 + pi[i]) / 2)
      message("Processing ", n_pis, " symmetric probability intervals...")
    } else {
      message("Processing ", n_pis, " probability intervals...")
    }
    
    # Do true values fall into each of the (symmetric) probability intervals?
    fall <- matrix(nrow = n_pts, ncol = n_pis)
    width <- matrix(nrow = n_pts, ncol = n_pis)
    if (symmetric) { # Deutsch (1997)
      for (i in 1:n_pts) {
        x <- simulated[i, ]
        y <- observed[i]
        for (j in 1:n_pis) {
          bounds <- quantile(x = x, pi_bounds[, j])
          fall[i, j] <- bounds[1] < y & y <= bounds[2]
          width[i, j] <- bounds[2] - bounds[1]
        }
      }
    } else { # Papritz & Dubois (1999)
      for (i in 1:n_pts) {
        x <- simulated[i, ]
        y <- observed[i]
        lowwer <- min(x)
        for (j in 1:n_pis) {
          upper <- quantile(x, pi[j])
          fall[i, j] <- y <= upper
          width[i, j] <- upper - lowwer
        }
      }
    }
    
    # Compute the proportion of true values that fall into each of the 
    # (symmetric) probability intervals
    count <- apply(fall, 2, sum)
    prop <- count / n_pts
    
    # Compute the average width of the (symmetric) probability intervals into
    # each the true values fall
    width <- width * fall
    width <- apply(width, 2, sum) / count
    
    # Compute summary statistics
    if (symmetric) {
      accu <- prop >= pi
      pi_idx <- which(accu)
      accu <- sum(prop >= pi) / n_pis # accuracy
    } else {
      accu <- prop == pi
      pi_idx <- which(accu)
      accu <- sum(prop == pi) / n_pis # accuracy
    }
    prec <- 1 - 2 * sum(prop[pi_idx] - pi[pi_idx]) / n_pis # precision
    pi_w <- ifelse(1:n_pis %in% pi_idx, 1, 2)
    good <- 1 - (sum(pi_w * abs(prop - pi)) / n_pis) # goodness
    pred <- apply(simulated, 1, mean)
    pred_var <- apply(simulated, 1, var)
    uncer <- mean(pred_var) # Uncertainty (Deutsch, 1997)
    err <- pred - observed
    me <- mean(err) # mean error
    serr <- err ^ 2
    mse <- mean(serr) # mean squared error
    srmse <- mean(serr / pred_var) # scaled root mean squared error
    corr <- cor(pred, observed) # linear correlation
    stats <- data.frame(me = me, mse = mse, srmse = srmse, cor = corr, 
                        A = accu, P = prec, G = good, U = uncer)
    
    if (plotit) {
      on.exit(par())
      par(mfrow = c(2, 2))
      cex <- ifelse(n_pts > 10, 0.5, 1)
      
      # Plot accuracy plot
      plot(0:1, 0:1, type = 'n', main = "Coverage probability",
           xlab = ifelse(symmetric, "Symmetric probability interval",
                         "Probability interval"),
           ylab = "Proportion")
      abline(a = 0, b = 1)
      points(x = pi, y = prop, cex = cex)
      if (symmetric) {
        text(x = 0, y = 1, labels = "More accurate", pos = 4)
        text(x = 1, y = 0, labels = "Less accurate", pos = 2)
      } else {
        text(x = 0, y = 1, labels = "Less accurate", pos = 4)
        text(x = 1, y = 0, labels = "Less accurate", pos = 2)
      }
      
      # Plot PI-width plot
      plot(x = pi, width, main = "Interval width", 
           xlab = ifelse(symmetric, "Symmetric probability interval",
                         "Probability interval"),
           ylab = "Width", cex = cex)
      
      # Plot observed vs simulated values
      lim <- range(c(observed, pred))
      plot(x = observed, pred, main = "Observed vs Simulated", 
           xlab = "Observed", ylim = lim, xlim = lim,
           ylab = "Simulated (average)", cex = cex)
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
      title(main = "Distribution of values", xlab = xlab, ylab = "Distribution")
    }
    
    # Output
    res <- list(data = data.frame(pi = pi, prop = prop, width = width), 
                stats = stats, symmetric = symmetric)
    return (res)
    
  }
