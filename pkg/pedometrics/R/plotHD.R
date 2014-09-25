#  file pedometrics/R/plotHD.R
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
#  Purpose        : Create histogram and density plot
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Depends on     : lattice, latticeExtra, car, moments
#
plotHD <- 
  function (x, HD = "over", nint = 20, digits = 2, stats = TRUE, 
            BoxCox = FALSE, col = c("lightgray", "black"), lwd = c(1, 1),
            lty = "dashed", xlim, ylim, ...) {
    if (BoxCox) {
      lambda <- powerTransform(x)
      print(summary(lambda))
      lambda <- as.numeric(lambda$lambda)
      if (lambda < 0) {
        message("estimated lambda value is negative... setting to zero")
        lambda <- 0
      }
      x <- bcPower(x, lambda)
    } else {
      lambda <- 1
    }
    if (HD == "over") {
      if (missing(xlim)) {
        xlim <- densityplot(x)$x.limits
      }
      p <- histogram(x, type = "density", col = col[1], xlim = xlim,
                     lwd = lwd[1], ...,
                     panel = function(x, ...) {
                       panel.histogram(x, ...)
                       panel.rug(x, col = col[2])
                       panel.mathdensity(dmath = dnorm, col = col[2],
                                         lwd = lwd[2], lty = lty,
                                         args = list(mean = mean(x), sd = sd(x)),
                                         n = length(x))
                     })
      if (missing(ylim)) {
        y1 <- p$y.limits
        y2 <- densityplot(x)$y.limits
        ylim <- c(min(c(y1[1], y2[1])), max(c(y1[2], y2[2])))
      }
      p$y.limits <- ylim
      if (stats) {
        skw <- round(c(skewness(x)), 2)
        leg <- c(paste("Lambda = ", round(lambda, 4), "\n", 
                       "Mean = ", round(mean(x), digits), " (", 
                                  round(sd(x), digits), ")\n",
                       "Median = ", round(median(x), digits), "\n",
                       "Range = ", round(min(x), digits), "-", 
                                   round(max(x), digits), "\n",
                       "Skew = ", skw, sep = ""))
        if (skw >= 1) {
          y <- NA
          pos <- NA
          p <- p + latticeExtra::layer(panel.text(x = x, y = y, labels = leg, 
                                                  pos = pos),
                                       data = list(x = c(max(p$x.limits) * 0.99), 
                                                   y = c(max(p$y.limits) * 0.9), 
                                                   leg = leg, pos = 2))
        }
        if (skw < 1) {
          y <- NA
          pos <- NA
          p <- p + latticeExtra::layer(panel.text(x = x, y = y, labels = leg, 
                                                  pos = pos),
                                       data = list(x = c(min(p$x.limits)),
                                                   y = c(max(p$y.limits) * 0.9),
                                                   leg = leg, pos = 4))
        }
      }
    }
    if (HD == "stack") {
      p2 <- densityplot(x, col = col[2], pch = 20, cex = 0.5, n = length(x),
                        lwd = lwd[2], lty = lty, ...)
      if (missing(xlim)) {
        xlim <- p2$x.limits
      }
      p1 <- histogram(x, col = col[1], xlim = xlim, nint = nint, lwd = lwd[1],
                      ...)
      p <- update(c(p1, p2), layout = c(1, 2), 
                  ylab = list(c("Percent of Total", "Density")), ...)
    }
    return(p)
  }
# End!