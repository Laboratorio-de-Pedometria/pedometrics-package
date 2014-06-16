#  file pedometria/R/cdfPlot.R
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

#  Purpose        : plot CDF
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : 0.1-0
#  Depends on     : spsurvey
#  Dependency of  : 
#  Note           : Known to work only with quantitative variables.
#                   Tested only with Ubuntu.
#  TODOs          : Clean the code. Remove unecessary arguments.

#  Timeline
#     Dec 2014: first version (by A. Samuel-Rosa)
#  23 Mar 2014: Changed function name from cdpplot() to cdfPlot() to comply
#               with programming style convention. Corrected funtion definition.
#               (by A. Samuel-Rosa)
#  19 May 2014: Changed legend from 'Confidence Limits' to 'CL' to guarantee
#               better plotting of the legend. Added option to show the
#               parameters of the cdf (mean, median, and percentile).
#  20 May 2014: Added argument to define if the confidence limits of the CDF
#               should be included in the plot.
#  16 Jun 2014: Corrected function call.

cdfPlot <- 
  function (obj, ind, units.cdf = "percent", type.plot = "s", 
            type.cdf = "continuous", logx = "", xlbl = NULL, ylbl = "Percent",
            ylbl.r = NULL, figlab = NULL, legloc = "BR", confcut = 5,
            show.conflev = TRUE,
            conflev = 95, show.param = TRUE, round = 0, 
            col.param = "black", ...) {
    # Plot estimated cumulative distribution function
    #
    # Args:
    #   obj:       Object with the estimated CDF.
    #   ind:       Indicator variable.
    #   type.plot: Desired type of plot to be produced.
    #
    # Returns:
    #   A plot of estimated CDF with confidence limits.
    #
    op <- par(mgp = c(1.7, 0.6, 0), mar = c(3, 3, 2, 4) + 0.1)
    obj <- obj
    ind <- ind
    cdfest <- obj$CDF[obj$CDF$Indicator == paste(ind), ]
    if (units.cdf == "percent") {
      cdfdata <- cdfest[, c(4, 6, 8, 9, 10)]
    } else if (units.cdf == "units") {
      cdfdata <- cdfest[, c(4, 10, 12, 13, 6)]
    } else {
      stop(paste("the choice of units must be either 'percent' or 'units'"))
    }
    pctval <- c(confcut, 100 - confcut)
    tvalue <- cdfest[, 6] >= pctval[1] & cdfest[, 6] <= pctval[2]
    x <- interp.cdf(pctval, cdfest[, 6], cdfdata[, 1])
    ylow <- interp.cdf(pctval, cdfest[, 6], cdfdata[, 3])
    yhi <- interp.cdf(pctval, cdfest[, 6], cdfdata[, 4])
    if (units.cdf == "percent") {
      ylimit <- c(0, 100)
    } else if (units.cdf == "units") {
      ylimit <- pretty(c(min(c(cdfdata[, 2], ylow)), max(c(cdfdata[, 2], yhi))))
      ylimit <- ylimit[c(1, length(ylimit))]
    }
    if (type.cdf == "continuous") {
      ty <- c("l", "s")
      if(any(ty == type.plot)){
        plot(cdfdata[, 1], cdfdata[, 2], type = type.plot, ylim = ylimit, 
             xlab = xlbl, ylab = ylbl, log = logx, ...)
        value <- c(x[1], cdfdata[, 1][tvalue], x[2])
        lower <- c(ylow[1], cdfdata[, 3][tvalue], ylow[2])
        upper <- c(yhi[1], cdfdata[, 4][tvalue], yhi[2])
        if (show.conflev) {  # Logical for showing the CDF's confidence limits
          lines(value, lower, type = type.plot, lty = 3, lwd = 1.5)
          lines(value, upper, type = type.plot, lty = 3, lwd = 1.5)
        }
        if (show.param) {
          mea <- round(mean(cdfdata[, 1]), round)
          med <- round(median(cdfdata[, 1]), round)
          per <- round(interp.cdf(conflev, cdfest[, 6], cdfdata[, 1]), round)
          lines(rep(mea, 2), ylimit, lty = "dashed", col = col.param)
          lines(rep(med, 2), ylimit, lty = "dotdash", col = col.param)
          lines(rep(per, 2), ylimit, lty = "longdash", col = col.param)
        }
      } else{
        stop(paste("the type of plot must be either 'l' (line) or 's' (stair)"))
      }
    } else if (type.cdf == "Ordinal") {
      x <- rep(cdfdata[, 1], each = 2)[-1]
      y <- rep(cdfdata[, 2], each = 2)
      tmp <- cbind(matrix(c(x, x[length(x)]), ncol = 2, byrow = TRUE), 
                   rep(NA, nrow(cdfdata)))
      x <- as.vector(t(tmp))
      tmp <- cbind(matrix(y, ncol = 2, byrow = TRUE), rep(NA, nrow(cdfdata)))
      y <- as.vector(t(tmp))
      plot(x, y, type = "l", ylim = ylimit, xlab = xlbl, ylab = ylbl, ...)
      len <- length(cdfdata[, 1][tvalue])
      if (len > 1) {
        value <- rep(cdfdata[, 1][tvalue], each = 2)[-1]
        tmp <- cbind(matrix(c(value, value[length(value)]), 
                            ncol = 2, byrow = TRUE), rep(NA, len))
        value <- as.vector(t(tmp))
        len <- length(cdfdata[, 4][tvalue])
        if (len > 1) {
          lower <- rep(cdfdata[, 3][tvalue], each = 2)
          tmp <- cbind(matrix(lower, ncol = 2, byrow = TRUE), rep(NA, len))
          lower <- as.vector(t(tmp))
          upper <- rep(cdfdata[, 4][tvalue], each = 2)
          tmp <- cbind(matrix(upper, ncol = 2, byrow = TRUE), rep(NA, len))
          upper <- as.vector(t(tmp))
          lines(value, lower, lty = 3, lwd = 1.5)
          lines(value, upper, lty = 3, lwd = 1.5)
        }
      }
    } else {
      stop(paste("the type of CDF must be either 'continuous' or 'ordinal'"))
    }
    title(figlab, line = 1)
    rx <- range(par("usr")[1:2], cdfdata[, 1])
    ry <- range(par("usr")[3:4], cdfdata[, 2])
    if (legloc == "BR") {
      xjust <- 1
      yjust <- 0
      legx <- rx[2]
      legy <- ry[1]
    } else if (legloc == "BL") {
      xjust <- 0
      yjust <- 0
      legx <- rx[1]
      legy <- ry[1]
    } else if (legloc == "TR") {
      xjust <- 1
      yjust <- 1
      legx <- rx[2]
      legy <- ry[2]
    } else if (legloc == "TL") {
      xjust <- 0
      yjust <- 1
      legx <- rx[1]
      legy <- ry[2]
    }
    if (show.param) {
      mea <- round(mean(cdfdata[, 1]), round)
      mea_sd <- round(sd(cdfdata[, 1]), round)
      med <- round(median(cdfdata[, 1]), round)
      per <- round(interp.cdf(conflev, cdfest[, 6], cdfdata[, 1]), round)
      legend(x = legx, y = legy, xjust = xjust, yjust = yjust,
             legend = c("CDF Estimate", 
                        paste(conflev, "% CL", sep = ""),
                        paste("Mean = ", mea, " (", mea_sd, ")", sep = ""),
                        paste("Median = ", med, sep = ""),
                        paste("P", conflev, " = ", per, sep = "")),
             lty = c("solid", "dotted", "dashed", "dotdash", "longdash"),
             lwd = c(1, 1.5, 1, 1, 1), bty = "n", cex = 1,
             col = c("black", "black", col.param, col.param, col.param))
    }
    else {
      legend(x = legx, y = legy, xjust = xjust, yjust = yjust,
             legend = c("CDF Estimate", 
                        paste(conflev, "% CL", sep = "")),
             lty = c(1, 3), lwd = c(1, 1.5), bty = "n", cex = 1) 
    }
    if (!is.null(ylbl.r)) {
      yl.lab <- seq(par("yaxp")[1], par("yaxp")[2], len = par("yaxp")[3] + 1)
      if (ylbl.r == "Same") {
        axis(side = 4, at = yl.lab, labels = yl.lab)
        mtext(ylbl, side = 4, line = 2, cex = par("cex"))
      } else {
        yr.lab <- interp.axis(yl.lab, cdfdata[, 2], cdfdata[, 5])
        axis(side = 4, at = yl.lab, labels = as.character(round(yr.lab)))
        mtext(ylbl.r, side = 4, line = 2, cex = par("cex"))
      }
    }
    par(op)
    invisible(NULL)
  }