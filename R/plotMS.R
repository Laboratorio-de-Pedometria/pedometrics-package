#  file pedometrics/R/plotMS.R
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
#  Purpose        : produce model series plot
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : beta
#  Depends on     : lattice::xyplot(), lattice::levelplot(), latticeExtra::c(),
#                   grid::grid.points()
#  Dependency of  :
#  Note           : tested only in Ubuntu
#  TODO           : 
plotMS <-
  function (obj, grid, line, ind, type = c("b", "g"), pch = c(20, 2),
            size = 0.5, arrange = "desc", color = NULL, 
            xlim = NULL, ylab = NULL, xlab = NULL, ...) {
    # check arguments ##########################################################
    if (missing(obj)) {
      stop("<obj> is a mandatory argument")
    }
    if (missing(grid)) {
      stop("<grid> is a mandatory argument")
    }
    if (missing(line)) {
      stop("<line> is a mandatory argument")
    }
    if (missing(ind)) {
      stop("<ind> is a mandatory argument")
    }
    if (class(obj) != "data.frame") {
      stop("<obj> should be of class data.frame")
    }
    if (!any(class(grid) == c("integer", "character", "numeric"))) {
      stop("<grid> should be an integer value or a character string")
    }
    if (!any(class(line) == c("integer", "character", "numeric"))) {
      stop("<line> should be an integer value or a character string")
    }
    if (!any(class(ind) == c("integer", "numeric")) || round(ind) != ind) {
      stop("<ind> should be an integer value")
    }
    if (any(class(line) == c("integer", "numeric"))) {
      nam0 <- c("candidates", "df", "aic", "rmse", "nrmse", "r2", "adj_r2", 
                "ADJ_r2")
      nam1 <- colnames(obj)[line]
      if (!any(colnames(obj)[line] == nam0)) {
        stop(paste("<ylab> should be provided for performance statistics <",
                   nam1, ">",  sep = ""))
      }
    }
    if (!missing(xlab)) {
      if (length(xlab) != 1) {
        stop("<xlab> should have length equal to 1")
      }
    }
    if (!missing(ylab)) {
      if (length(ylab) != 2) {
        stop("<ylab> should have length equal to 2")
      }
    }
    if (length(type) != 2) {
      stop("<type> should have length equal to 2")
    }
    if (length(pch) != 2) {
      stop("<pch> should have length equal to 2")
    }
    # prepare data #############################################################
    if (class(line) == "numeric") {
      line <- colnames(obj)[line]
    }
    if (any(line == c("r2", "adj_r2", "ADJ_r2"))) {
      obj <- arrange(obj, obj[, line])
    } else {
      obj <- arrange(obj, desc(obj[, line]))  
    }
    grid <- as.matrix(obj[, grid])
    x <- seq(1, dim(obj)[1], 1)
    y <- as.numeric(obj[, line])
    if (missing(color)) {
      color <- cm.colors(length(unique(as.numeric(grid))))
    }
    if (missing(xlim)) {
      xlim <- c(0.5, dim(obj)[1] + 0.5)
    }
    if (missing(xlab)) {
      xlab <- "Model ranking"
    }
    if (missing(ylab)){
      if (class(line) == "numeric") {
        line <- colnames(obj)[line]
      }
      if (line == "candidates") {
        yl <- "Candidate predictors"
      }
      if (line == "df") {
        yl <- "Degrees of freedom"
      }
      if (line == "aic") {
        yl <- "AIC"
      }
      if (line == "rmse") {
        yl <- "RMSE"
      }
      if (line == "nrmse") {
        yl <- "NRMSE"
      }
      if (line == "r2") {
        yl <- expression(paste(R^2, sep = ''))
      }
      if (any(line == c("adj_r2", "ADJ_r2"))) {
        yl <- expression(paste('Adjusted ',R^2, sep = ''))
      }
      ylab <- list(c(yl, "Design"))
    }
    rank_center <- rep(NA, dim(grid)[2])
    for (i in 1:length(rank_center)) {
      rank_center[i] <- 
        mean(cbind(x, grid)[, 1][which(cbind(x, grid)[, i + 1] == ind)])
    }
    grid <- grid[, order(rank_center)]
    # prepare plot #############################################################
    p1 <- xyplot(y ~ x, xlim = xlim, type = type, pch = pch[1],
                 scales = list(y = list(rot = 0)))
    p2 <- levelplot(grid, colorkey = FALSE,  xlim = xlim, col.regions = color,
                    scales = list(y = list(rot = 90)),
                    panel = function (...) {
                      panel.levelplot(...)
                      grid.points(x = sort(rank_center), 
                                  seq(1, dim(grid)[2], 1),
                                  pch = pch[2], size = unit(size, "char"))
                    })
    # print plot ###############################################################
    update(c(p1, p2), layout = c(1, 2), xlab = xlab, 
           ylab = ylab, aspect = c((dim(grid)[2] * 2) / dim(grid)[1]),
           par.settings = list(layout.heights = list(panel = c(0.5, 0.5))),
           ...)
  }
# End!