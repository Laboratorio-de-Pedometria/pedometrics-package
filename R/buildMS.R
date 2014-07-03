#  file pedometrics/R/buildMS.R
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
#  Purpose        : build predictive models
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : beta
#  Depends on     : pbapply::pblapply(), MASS::stepAIC(), pedometrics::stepVIF()
#  Dependency of  :
#  Note           : tested only in Ubuntu
#  TODO           : - include functionality to remove outliers
#                   - add option to set the order in which stepAIC and stepVIF
#                     are run
#
#  Timeline
#  26 Mar 2014: First version (by A. Samuel-Rosa)
#  28 Mar 2014: Improved documentation. Included arguments 'vif.verbose',
#               'vif.threshold', 'aic.direction', 'aic.trace', 'aic.steps'.
#  01 Apr 2014: Removed option to remove aliased predictors. Now the model
#               is checked for aliased predictors authomatically.
#  04 Apr 2014: Added option to penalize the final model using the original
#               number of covariates to calculate the adjusted R squared.
#  09 Apr 2014: Removed the option for penalization. The number of candidate
#               predictor variables now is added as an attribute to the final
#               model and used for penalization by modelStats(). Removed the
#               check of aliased predictors.
#  22 Apr 2014: Changed name to buildModelSeries().
#  16 Jun 2014: Added option to call the function using buildMS().
#  17 Jun 2014: Moved the documentation to a Rd file. Replaced argument <object>
#               by <formula>. Improved checking of arguments. Improved
#               documentation. Added option to pass further arguments to 
#               stepAIC().
#
buildMS <- 
  function (formula, data,
            vif = FALSE, vif.threshold = 10, vif.verbose = FALSE,
            aic = FALSE, aic.direction = "both", aic.trace = FALSE,
            aic.steps = 5000, ...) {
    # check arguments ##########################################################
    if (missing(formula)) {
      stop("<formula> is a mandatory argument")
    }
    if (class(formula) != "list") {
      formula <- list(formula)
    }
    if (missing(data)) {
      stop("<data> is a mandatory argument")
    }
    if (class(data) != "data.frame") {
      data <- as.data.frame(data)
    }
    # lm() #####################################################################
    print("fitting linear model using ols")
    model <- pblapply(formula, function (X){
      lm(X, data)
      })
    # get the initial number of candidate predictors and observations
    p <- sapply(model, function (X) {
      dim(model.matrix(X))[2]
      })
    n <- sapply(model, function (X) {
      dim(model.matrix(X))[1]
    })
    # stepVIF() ################################################################
    if (vif) {
      print("backward variable selection using VIF")
      model <- pblapply(model, function (X) {
        stepVIF(X, threshold = vif.threshold, verbose = vif.verbose)
        })
    }
    # stepAIC() ################################################################
    if (aic) {
      print(paste(aic.direction, " variable selection using AIC", sep = ""))
      model <- pblapply(model, function (X) {
        stepAIC(X, direction = aic.direction, steps = aic.steps, 
                trace = aic.trace, ...)
        })
    }
    # prepare output ###########################################################
    # add attributes to the final model
    a <- lapply(model, attributes)
    for (i in 1:length(a)) {
      a[[i]]$p <- p[i]
    }
    for (i in 1:length(a)) {
      a[[i]]$n <- n[i]
    }
    for (i in 1:length(model)) {
      attributes(model[[i]]) <- a[[i]]
    }
    return(model)
  }
# End!