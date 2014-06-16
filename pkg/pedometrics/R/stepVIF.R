#  file pedometrics/R/stepVIF.R
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

#  Purpose        : variable selection using the variance-inflation factor
#  Maintainer     : Alessandro Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : beta
#  Depends on     : car
#  Dependency of  :
#  Note           : tested only in Ubuntu
#  TODOs          : include option to use partial correlations as criteria to 
#                   drop predictor variables.

#  Timeline
#  22 Mar 2014: first version (by A. Samuel-Rosa)
#  25 Mar 2014: changed the way models are updated using update()
#  15 Apr 2014: improved documentation

stepVIF <-
  function (model, ...) {
    UseMethod("stepVIF")
  }

stepVIF <-
  function (model, threshold = 10, verbose = FALSE) {
    # Choose a model by VIF in a Stepwise Algorithm
    #
    # Args:
    #   model:     Object of class lm.
    #   threshold: Maximum acceptable vif value.
    #   verbose:   Logical for printing iteration results. Default is FALSE.
    #
    # Returns:
    #   A linear model with low collinearity.
    
    if (!inherits(model, "lm")) {
      stop ("'model' must be of class 'lm'")
    }
    if (threshold <= 0) {
      stop ("'threshold' must be a positive number")
    }
    # set conditional variable (number of iterations = number of variables)
    iter <- dim(model.frame(model))[2]
    iter0 <- iter
    while (iter > 0) {
      iter <- iter - 1
      # calculate generalized variance-inflation factors
      init_vif <- data.frame(car::vif(model))
      var_nam <- rownames(init_vif)
      if (ncol(init_vif) == 3) {
        init_vif <- data.frame(init_vif[, 3])
        } else {
          init_vif <- data.frame(sqrt(init_vif[, 1]))
        }
      rownames(init_vif) <- var_nam
      colnames(init_vif) <- "vif"
      # get vars with vif > threshold
      lim_vif <- sqrt(threshold)
      if (max(init_vif) <= lim_vif) {
        cat("all predictor variables have a VIF lower than the threshold\n")
        break
      }
      vars_vif <- which(init_vif > lim_vif)
      df <- model.frame(model)
      df_nam <- colnames(df[, -1])[vars_vif]
      if (length(df_nam) == 1) {  # only one predictor with high VIF
        # update model formula
        #out <- names(vars_vif)
        out <- df_nam
        new_form <- formula(paste(". ~ .", paste(out, collapse = "-"), sep = "-"))
        model <- stats::update(model, new_form)
        break
      }
      vars_vif <- data.frame(df[, -1][, vars_vif])
      colnames(vars_vif) <- df_nam
      # fit lm between dependent variable and every predictor variable
      new_fit <- lapply(vars_vif, function (X) {
        lm(df[, 1] ~ X)
        })
      r2 <- unlist(sapply(new_fit, summary)["adj.r.squared", ])
      min_r2 <- which(r2 == min(r2))
      # update model formula
      out <- names(min_r2)
      new_form <- formula(paste(". ~ .", paste(out, collapse = "-"), sep = "-"))
      model <- stats::update(model, new_form)
      end_vif <- data.frame(car::vif(model))
      if (ncol(end_vif) == 3) {
        end_vif <- end_vif[, 3]
        } else {
          end_vif <- data.frame(sqrt(end_vif[, 1]))
        }
      # check VIF
      end_vif <- max(end_vif*end_vif)
      if (verbose) {  # print the results of each iteration (if requested)
        tab <- round(init_vif[names(vars_vif),]^2)
        names(tab) <- names(vars_vif)
        cat("------------------------------------------------------------\n")
        cat(paste("iteration: ", iter0 - iter, sep = ""), "\n")
        cat(paste("collinearity cases: ", length(names(tab)), sep = ""), "\n")
        cat("generalized variance inflation factor\n")
        print(tab)
        cat("correlation with dependent variable (Adjusted R squared)\n")
        print(round(r2, 4))
        cat(paste("dropped term: ", names(min_r2), "\n"))
        print(model)
      }
      if (end_vif <= threshold)  # evaluate model vif
        break
    }
    return (model)
  }
# End!