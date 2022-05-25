#' Build a series of linear models using automated variable selection
#'
#' @description 
#' This function allows building a series of linear models with [stats::lm()] using one or more
#' automated variable selection implemented in function [pedometrics::stepVIF()] and
#' [MASS::stepAIC()].
#'
#' @param formula A list containing one or several model formulas (a symbolic description of the
#' model to be fitted).
#'
#' @param data Data frame containing the variables in the model formulas.
#'
#' @param vif Logical for performing backward variable selection using the Variance-Inflation Factor
#' (VIF). Defaults to `VIF = FALSE`.
#'
#' @param vif.threshold Numeric value setting the maximum acceptable VIF value. Defaults to
#' `vif.threshold = 10`.
#'
#' @param vif.verbose Logical for printing iteration results of backward variable selection using
#' the VIF. Defaults to `vif.verbose = FALSE`.
#'
#' @param aic Logical for performing variable selection using Akaike's Information Criterion (AIC).
#' Defaults to `aic = FALSE`.
#'
#' @param aic.direction Character string setting the direction of variable selection when using AIC.
#' Available options are `"both"`, `"forward"`, and `"backward"`. Defaults to
#' `aic.direction = "both"`.
#'
#' @param aic.trace Logical for printing iteration results of variable selection using the AIC.
#' Defaults to `aic.trace = FALSE`.
#'
#' @param aic.steps Integer value setting the maximum number of steps to be considered for variable
#' selection using the AIC. Defaults to `aic.steps = 5000`.
#'
#' @param ... Further arguments passed to [MASS::stepAIC()].
#'
#' @details
#' This function was devised to deal with a list of linear model formulas. The main objective is to
#' bring together several functions commonly used when building linear models, such as automated
#' variable selection. In the current implementation, variable selection can be done using
#' [pedometrics::stepVIF()] or [MASS::stepAIC()] or both. [pedometrics::stepVIF()] is a backward
#' variable selection procedure, while [MASS::stepAIC()] supports backward, forward, and
#' bidirectional variable selection. For more information about these functions, please visit their
#' respective help pages.
#'
#' An important feature of [pedometrics::buildModelSeries()] is that it records the initial number
#' of candidate predictor variables and observations offered to the model, and adds this information
#' as an attribute to the final selected model. Such feature was included because variable selection
#' procedures result biased linear models (too optimistic), and the effective number of degrees of
#' freedom is close to the number of candidate predictor variables initially offered to the model
#' (Harrell, 2001). With the initial number of candidate predictor variables and observations
#' offered to the model, one can calculate penalized or adjusted measures of model performance. For
#' models built using [pedometrics::buildModelSeries()], this can be done using
#' [pedometrics::statsMS()].
#'
#' Some important details should be clear when using [pedometrics::buildModelSeries()]:
#'
#' * this function was originally devised to deal with a list of formulas, but can also be used with
#'   a single formula;
#' * in the current implementation, [pedometrics::stepVIF()] runs before [MASS::stepAIC()];
#' * function arguments imported from [MASS::stepAIC()] and [pedometrics::stepVIF()] were named as
#'   in the original functions, and received a prefix (`aic` or `vif`) to help the user identifying
#'   which function is affected by a given argument without having to go check the documentation.
#'
#' @return
#' A list containing the fitted linear models.
#'
#' @references
#' Harrell, F. E. (2001) _Regression modelling strategies: with applications to linear models,
#' logistic regression, and survival analysis._ First edition. New York: Springer.
#'
#' Venables, W. N. and Ripley, B. D. (2002) _Modern applied statistics with S._ Fourth edition.
#' New York: Springer.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @section TODO:
#' Add option to set the order in which [MASS::stepAIC()] and [pedometrics::stepVIF()] are run.
#'
#' @seealso [pedometrics::stepVIF()], [pedometrics::statsMS()]
#'
#' @examples
#' if (interactive()) {
#' # based on the second example of MASS::stepAIC()
#' library("MASS")
#' cpus1 <- cpus
#' for(v in names(cpus)[2:7])
#'   cpus1[[v]] <- cut(cpus[[v]], unique(stats::quantile(cpus[[v]])),
#'                     include.lowest = TRUE)
#' cpus0 <- cpus1[, 2:8]  # excludes names, authors' predictions
#' cpus.samp <- sample(1:209, 100)
#' cpus.form <- list(formula(log10(perf) ~ syct + mmin + mmax + cach + chmin +
#'                   chmax + perf),
#'                   formula(log10(perf) ~ syct + mmin + cach + chmin + chmax),
#'                   formula(log10(perf) ~ mmax + cach + chmin + chmax + perf))
#' data <- cpus1[cpus.samp,2:8]
#' cpus.ms <- buildModelSeries(cpus.form, data, vif = TRUE, aic = TRUE)
#' }
#' 
#' @aliases buildMS buildModelSeries
# FUNCTION #########################################################################################
#' @export
#' @rdname buildModelSeries
buildModelSeries <-
  function(formula, data, vif = FALSE, vif.threshold = 10, vif.verbose = FALSE, aic = FALSE,
          aic.direction = "both", aic.trace = FALSE, aic.steps = 5000, ...) {
    # check if suggested packages are installed
    if (!requireNamespace("MASS")) stop("MASS package is missing")
    # check arguments
    if (missing(formula)) {
      stop("'formula' is a mandatory argument")
    }
    if (!inherits(formula, "list")) {
      formula <- list(formula)
    }
    if (missing(data)) {
      stop("'data' is a mandatory argument")
    }
    if (!inherits(data, "data.frame")) {
      data <- as.data.frame(data)
    }
    # stats::lm
    print("fitting multiple linear regression model using OLS")
    model <- lapply(formula, function(X) stats::lm(X, data))
    # get the initial number of candidate predictors and observations
    p <- sapply(model, function(X) dim(stats::model.matrix(X))[2])
    n <- sapply(model, function(X) dim(stats::model.matrix(X))[1])
    # pedometrics::stepVIF
    if (vif) {
      print("backward predictor variable selection using VIF")
      model <- lapply(model, function(X) {
        stepVIF(X, threshold = vif.threshold, verbose = vif.verbose)
      })
    }
    # MASS::stepAIC
    if (aic) {
      print_aic <- ifelse(aic.direction == "both", "both way", aic.direction)
      print_out <- paste0(print_aic, " predictor variable selection using AIC")
      print(print_out)
      model <- lapply(model, function(X) {
        MASS::stepAIC(X, direction = aic.direction, steps = aic.steps, trace = aic.trace, ...)
      })
    }
    # Prepare output - add attributes to the final model
    a <- lapply(model, attributes)
    for (i in seq_along(a)) {
      a[[i]]$p <- p[i]
    }
    for (i in seq_along(a)) {
      a[[i]]$n <- n[i]
    }
    for (i in seq_along(model)) {
      attributes(model[[i]]) <- a[[i]]
    }
    return(model)
  }
#' @export
#' @rdname buildModelSeries
buildMS <- buildModelSeries
