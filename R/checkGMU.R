#' Evaluation of geostatistical models of uncertainty
#' 
#' Evaluate the local quality of a geostatistical model of uncertainty 
#' (GMU) using summary measures and graphical displays.
#' 
#' @param observed Vector of observed values at the validation points.
#' 
#' @param simulated Data frame or matrix with simulated values (columns) for
#' each validation point (rows).
#' 
#' @param pi Vector defining the width of the series of probability intervals.
#' Defaults to \code{pi = seq(0.01, 0.99, 0.01)}.
#' 
#' @param symmetric Logical for choosing the type of probability interval.
#' Defaults to \code{symmetric = TRUE}. See \sQuote{Details} for more 
#' information.
#' 
#' @param plotit Logical for plotting the results. Defaults to 
#' \code{plotit = TRUE}.
#' 
#' @details 
#' There is no standard way of evaluating the local quality of a GMU. The 
#' collection of summary measures and graphical displays presented here is far
#' from being comprehensive, nor is it intended to be the \sQuote{best}  
#' selection of existing summary measures and graphical displays. My choice
#' is purely based on the literature -- I have tried to include here those
#' summary measures and graphical displays that are most commonly used to 
#' evaluate the local quality of GMUs.
#' 
#' Understanding a few definitions is needed to correctly interpret the 
#' collection of summary measures and graphical displays presented here. I try 
#' to give a brief overview of them in the following lines.
#'
#' \subsection{Error statistics}{
#' Error statistics measure how well the GMU predicts the measured values at the
#' validation points. Four error statistics are employed:
#' 
#' \describe{
#' \item{Mean error (ME)}{
#' Measures the bias of the predictions of the GMU, being defined as the mean of
#' the differences between the average of the simulated values and the observed
#' values.
#' }
#' \item{Mean squared error (MSE)}{
#' Measures the accuracy of the predictions of the GMU, being defined as the
#' mean of the squared differences between the average of the simulated values
#' and the observed values.
#' }
#' \item{Scaled root mean squared error (SRMSE)}{
#' Measures how good is the estimate of the (prediction error) variance of the
#' GMU, where SRMSE > 1 indicates underestimation, while SRMSE < 1 indicates
#' overestimation. The SRMSE is defined as the mean of the squared differences
#' between the average of the simulated values and the observed values divided 
#' by the variance of the simulated values.
#' }
#' \item{Linear correlation}{
#' Measures how close the GMU predictions are to the observed values. A scatter 
#' plot of the observed values versus the average of the simulated values 
#' is produced to allow checking for possible unwanted outliers and 
#' non-linearities. The square of the linear correlation measures the fraction 
#' of the overall spread of observed values that is explained by the GMU, 
#' that is, the amount of variance explained (AVE), also known as coefﬁcient 
#' of determination or ratio of scatter.
#' }
#' }
#' }
#' \subsection{Coverage probabilities}{
#' Include a description of what coverage probabilities are. Describe the two
#' types of probability intervals that can be used here.
#' 
#' Deutsch (1997) proposed three summary measures of the coverage 
#' probabilities to assess the local \emph{goodness} of a GMU: accuracy ($A$),
#' precision ($P$), and goodness ($G$). According to him, a GMU can be 
#' considered \dQuote{good} if it is both accurate and precise. He also defined
#' that its uncertainty ($U$) should be as small as possible. I am unaware of 
#' the robustness of these measures because they have been poorly explored by 
#' the geostatistical community, except for a few studies developed by Pierre 
#' Goovaerts and his later software implementation (Goovaerts, 2009).
#' 
#' It is important to note that the goodness measures proposed by Deutsch (1997)
#' were defined based on the use of symmetric, median-centred probability 
#' intervals to compute the coverage probabilities. I am unaware of the 
#' robustness of these goodness measures when the coverage probabilities are
#' computed using left-bounded probability intervals (Papritz & Dubois, 1999).
#' The generalization made here is purely operational, and the user should 
#' use the results with caution till we have a better understanding of these
#' goodness measures.
#' 
#' \describe{
#' \item{Accuracy}{
#' An accurate GMU is that for which the proportion $p^*$ of true values 
#' falling within the symmetric $p$ probability interval is equal to or larger 
#' than $p$, that is, when $p^* \geq p$. Thus, a GMU will be more accurate when 
#' all points in the coverage probability plot are on or above the 1:1 line. 
#' The range of $A$ goes from 0 (lest accurate) to 1 (most accurate).
#' }
#' \item{Precision}{
#' The \emph{precision}, $P$, is defined only for an accurate GMU, and measures
#' how close $p^*$ is to $p$ (Deutsch, 1997). The range of $P$ goes from 0 
#' (lest precise) to 1 (most precise). Thus, a GMU will be more accurate when 
#' all points in the PI-width plot are on or above the 1:1 line.
#' }
#' \item{Goodness}{
#' The \emph{goodness}, $G$, is a measure of the departure of the points from
#' the 1:1 line in the coverage probability plot (Deutsch, 1997). $G$ ranges 
#' from 0 (minimum goodness) to 1 (maximum goodness), the maximum $G$ being 
#' achieved when $p^* = p$, that is, all points in both coverage probability 
#' and interval width plots are exactly on the 1:1 line.
#' }
#' }
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
#' simulated <- t(sapply(1:length(observed), 
#'                       function (i) round(rnorm(100), 3)))
#' resa <- checkGMU(observed, simulated, symmetric = T)
#' resb <- checkGMU(observed, simulated, symmetric = F)
#' round(resa$stats, 3)
#' round(resb$stats, 3)
# FUNCTION #####################################################################
checkGMU <-
  function (observed, simulated, pi = seq(0.01, 0.99, 0.01),
            symmetric = TRUE, plotit = TRUE) {
    
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
    g_fall <- matrix(nrow = n_pts, ncol = n_pis)
    g_width <- matrix(nrow = n_pts, ncol = n_pis)
    if (symmetric) { # Deutsch (1997)
      for (i in 1:n_pts) {
        x <- simulated[i, ]
        y <- observed[i]
        for (j in 1:n_pis) {
          # Local
          bounds <- stats::quantile(x = x, probs = pi_bounds[, j])
          fall[i, j] <- bounds[1] < y & y <= bounds[2]
          width[i, j] <- bounds[2] - bounds[1]
          # Global
          g_bounds <- stats::quantile(x = observed, probs = pi_bounds[, j])
          g_fall[i, j] <- g_bounds[1] < y & y <= g_bounds[2]
          g_width[i, j] <- g_bounds[2] - g_bounds[1]
        }
      }
    } else { # Papritz & Dubois (1999)
      for (i in 1:n_pts) {
        x <- simulated[i, ]
        y <- observed[i]
        lowwer <- min(x)
        g_lowwer <- min(observed)
        for (j in 1:n_pis) {
          # Local
          upper <- stats::quantile(x = x, probs = pi[j])
          fall[i, j] <- y <= upper
          width[i, j] <- upper - lowwer
          # Global
          g_upper <- stats::quantile(x = observed, probs = pi[j])
          g_fall[i, j] <- y <= g_upper
          g_width[i, j] <- g_upper - g_lowwer
        }
      }
    }
    
    # Compute the proportion of true values that fall into each of the 
    # (symmetric) probability intervals
    count <- apply(fall, 2, sum)
    prop <- count / n_pts
    
    g_count <- apply(g_fall, 2, sum)
    # g_prop <- g_count / n_pts
    
    # Compute the average width of the (symmetric) probability intervals into
    # each the true values fall
    width <- width * fall
    width <- apply(width, 2, sum) / count
    
    g_width <- g_width * g_fall
    g_width <- apply(g_width, 2, sum) / g_count
    
    # Compute summary statistics
    accu <- prop >= pi
    pi_idx <- which(accu)
    accu <- sum(prop >= pi) / n_pis # accuracy
    prec <- 1 - 2 * sum(prop[pi_idx] - pi[pi_idx]) / n_pis # precision
    pi_w <- ifelse(1:n_pis %in% pi_idx, 1, 2)
    good <- 1 - (sum(pi_w * abs(prop - pi)) / n_pis) # goodness
    pred <- apply(simulated, 1, mean) # predicted value
    pred_var <- apply(simulated, 1, var) # prediction variance
    uncer <- mean(pred_var) # uncertainty
    err <- pred - observed # error
    me <- mean(err) # mean error
    serr <- err ^ 2 # squared error
    mse <- mean(serr) # mean squared error
    srmse <- mean(serr / pred_var) # scaled root mean squared error
    corr <- cor(pred, observed) # linear correlation
    stats <- data.frame(me = me, mse = mse, srmse = srmse, cor = corr, 
                        A = accu, P = prec, G = good, U = uncer)

    if (plotit) {
      on.exit(par())
      par(mfrow = c(2, 2))
      cex <- ifelse(n_pts > 10, 0.5, 1)
      
      # Coverage probability plot
      plot(0:1, 0:1, type = 'n', main = "Coverage probability",
           xlab = "Probability interval", ylab = "Proportion")
      abline(a = 0, b = 1)
      points(x = pi, y = prop, cex = cex)
      if (symmetric) {
        text(x = 1, y = 0, labels = "Symmetric PIs", pos = 2)
      }
      
      # PI-width plot
      lim <- range(c(width, g_width), na.rm = TRUE)
      plot(x = width, y = g_width, ylim = lim, xlab = "Local PI", 
           ylab = "Global PI", cex = cex, xlim = lim, main = "PI width")
      abline(a = 0, b = 1)
      if (symmetric) {
        text(x = lim[2], y = lim[1], labels = "Symmetric PIs", pos = 2)
      }
      
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
