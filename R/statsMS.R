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
#  Depends on     : plyr::arrange, plyr:desc
#  Dependency of  :
adjR2 <- 
  function (r2, n, p) {
    r2 <- 1 - (1 - r2) * ((n - 1) / (n - p - 1))
    return(r2)
  }
statsMS <-
  function (model, design.info, arrange.by, digits) {
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
    if (class(model) == "lm") {
      model <- list(model)
    }
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
    r2     <- as.numeric(unlist(sapply(model, summary)["r.squared", ]))
    adj_r2 <- as.numeric(unlist(sapply(model, summary)["adj.r.squared", ]))
    ADJ_r2 <- adjR2(r2, n, p = c(p - 1))
    id <- seq(1, length(n), 1)
    if (!missing(digits)) {
      if (length(digits) == 6) {
        aic    <- round(aic, digits[1])
        rmse   <- round(rmse, digits[2])
        nrmse  <- round(nrmse, digits[3])
        r2     <- round(r2, digits[4])
        adj_r2 <- round(adj_r2, digits[5])
        ADJ_r2 <- round(ADJ_r2, digits[6])
      } else {
        aic    <- round(aic, digits)
        rmse   <- round(rmse, digits)
        nrmse  <- round(nrmse, digits)
        r2     <- round(r2, digits)
        adj_r2 <- round(adj_r2, digits)
        ADJ_r2 <- round(ADJ_r2, digits)
      }
    }
    if (!missing(design.info)) {
      candidates <- p
      tab <- data.frame(cbind(id, design.info, candidates, df, aic, rmse, nrmse,
                              r2, adj_r2, ADJ_r2), stringsAsFactors = FALSE)
    } else {
      tab <- data.frame(id, candidates = p, df, aic, rmse, nrmse, r2, adj_r2, 
                        ADJ_r2, stringsAsFactors = FALSE)  
    }
    if (!missing(arrange.by)) {
      tab <- arrange(tab, desc(tab[, arrange.by]))
    }
    return(tab)
  }