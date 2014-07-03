#  file pedometrics/R/statsMS.R
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
#  Purpose        : Get model statistics
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : beta
#  Depends on     : plyr::arrange, plyr:desc, pedometrics::adjR2()
#  Dependency of  :
#  Note           : Tested only in Ubuntu. Designed to return the statistics of
#                   linear models built with buildMS().
#  TODO           : 
#
#  Timeline
#  26 Mar 2014: First version (by A. Samuel-Rosa)
#  04 Apr 2014: Added option to check is the adjusted R2 was penalized.
#  09 Apr 2014: Penalization is now calculated here.
#  15 Apr 2014: Improved documentation.
#  22 Apr 2014: Changed name to statsModelSeries(). A column with model ID is
#               automatically create. Information about the modeling desing can
#               be added to the table. The table now can also be arranged
#               (ordered) using one of the available statistics.
#  16 Jun 2014: Added option to call the function using statsMS().
#  16 Jun 2014: Documentation was moved to a Rd file. Included argument checking.
#               Added option to round the results.
#
statsMS <-
  function (model, design.info, arrange.by, digits) {
    # check arguments ##########################################################
    if (missing(model)) {
      stop("<model> is a mandatory argument")
    }
    if (!any(class(model) == c("list", "lm"))) {
      stop("<model> should be of class list or lm")
    }
    if (!missing(design.info)) {
      if (class(design.info) != "data.frame") {
        stop("<design.info> should be of class data.frame")
      }
    }
    if (!missing(arrange.by)) {
      if (class(arrange.by) != "character") {
        stop("<arrange.by> should be of class character")
      }  
    }
    # prepare data #############################################################
    if (class(model) == "lm") {
      model <- list(model)
    }
    # get performance statistics ###############################################
    n      <- as.numeric(sapply(model, attr, "n"))
    p      <- as.numeric(sapply(model, attr, "p"))
    df     <- sapply(model, extractAIC)[1, ]
    aic    <- sapply(model, extractAIC)[2, ]
    res    <- sapply(model, residuals)
    rmse   <- rep(NA, ncol(res))
    nrmse  <- rep(NA, ncol(res))
    sd_y   <- sd(data.frame(lapply(model, model.frame))[, 1])
    for (i in 1:ncol(res)) {
      rmse[i] <- sqrt(sum(res[, i] * res[, i]) / (n[i] - p[i]))
      nrmse[i] <- rmse[i] / sd_y
    }
    r2     <- as.numeric(unlist(sapply(model, summary)[8, ]))
    adj_r2 <- adjR2(r2, n, p = c(p - 1))
    id <- seq(1, length(n), 1)
    # prepare output ###########################################################
    if (!missing(digits)) {
      if (length(digits) == 4) {
        aic    <- round(aic, digits[1])
        rmse   <- round(rmse, digits[2])
        nrmse  <- round(nrmse, digits[3])
        adj_r2 <- round(adj_r2, digits[4])
      } else {
        aic    <- round(aic, digits)
        rmse   <- round(rmse, digits)
        nrmse  <- round(nrmse, digits)
        adj_r2 <- round(adj_r2, digits) 
      }
    }
    if (!missing(design.info)) {
      candidates <- p
      tab <- data.frame(cbind(id, design.info, candidates, df, aic, rmse, nrmse,
                              adj_r2), stringsAsFactors = FALSE)
    } else {
      tab <- data.frame(id = id, candidates = p, df = df, aic = aic, rmse = rmse,
                        nrmse = nrmse, adj_r2 = adj_r2, stringsAsFactors = FALSE)  
    }
    # arrange data
    if (!missing(arrange.by)) {
      tab <- arrange(tab, desc(tab[, arrange.by]))
    }
    return(tab)
  }
# End!