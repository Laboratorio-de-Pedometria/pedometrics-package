#' Guess the parameters of a spatial covariance function
#'
#' @description
#' Guess the parameters of the spatial covariance function of a random, regionalized variable. A
#' guess of such parameters is required to start the fitting functions of many geostatistical
#' packages such as __gstat__, __geoR__, and __georob__.
#'
#' @inheritParams variogramBins
#'
#' @param z Numeric vector with the values of the regionalized variable for which the values for the
#' spatial covariance parameters should be guessed.
#'
#' @param lags Numeric scalar defining the width of the variogram bins or a numeric vector with the
#' lower and upper bounds of the variogram bins. If missing, the variogram bins are computed using
#' [pedometrics::variogramBins()]. See \sQuote{Details} for more information.
#'
#' @param method Character keyword defining the method used for guessing the spatial covariance
#' parameters of the regionalized variable. Defaults to `method = "a"`. See \sQuote{Details} for
#' more information.
#'
#' @param min.npairs Positive integer defining the minimum number of point-pairs required so that a
#' variogram bin is used to guessing the spatial covariance parameters of the  of the regionalized
#' variable. Defaults to `min.npairs = 30`.
#'
#' @param model Character keyword defining the spatial covariance function that will be fitted to
#' the data of the regionalized variable. Currently, most of the basic spatial covariance function
#' are accepted. See `geoR::cov.spatial()` for more information. Defaults to `model = "matern"`.
#'
#' @param nu Numerical value for the additional smoothness parameter \eqn{\nu} of the spatial
#' covariance function of the regionalized variable. See `RandomFields::RMmodel()` and argument
#' `kappa` of `geoR::cov.spatial()` for more information.
#'
#' @param plotit Should the guessed spatial covariance parameters be plotted along with the sample
#' (experimental) variogram of the regionalized variable? Defaults to `plotit = FALSE`.
#'
#' @param estimator Character keyword defining the estimator for computing the sample (experimental)
#' variogram of the regionalized variable, with options `"qn"` (default), `"mad"`, `"matheron"`, and
#' `"ch"`. See `georob::sample.variogram()` for more details.
#'
#' @return
#' A vector of numerical values, the guesses for the spatial covariance parameters of the
#' regionalized variable:
#' 
#'   * nugget
#'   * partial sill
#'   * range
#'
#' @details
#' There are five methods two guess the covariance parameters. Two of them, `"a"` and `"c"`, rely on
#' a sample variogram with exponentially growing variogram bins, while the other three, `"b"`,
#' `"d"`, and `"e"`, use equal-width variogram bins (see [pedometrics::variogramBins()]). All of
#' them are [heuristic](https://en.wikipedia.org/wiki/Heuristic).
#'
#' Method `"a"` was developed in-house and is the most elaborated of them, specially for guessing
#' the nugget variance.
#'
#' Method `"b"` was proposed by \doi{10.1016/0098-3004(95)00095-X}{Jian et al. (1996)} and
#' is implemented in \href{https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_variogram_a0000000593.htm}{SAS/STAT(R) 9.22}.
#'
#' Method `"c"` is implemented in the __automap__-package and was developed by
#' \doi{10.1016/j.cageo.2008.10.011}{Hiemstra et al. (2009)}.
#'
#' Method `"d"` was developed by \doi{10.1007/s11004-012-9434-1}{Desassis & Renard (2012)}.
#'
#' Method `"e"` was proposed by Larrondo et al. (2003)
#' <http://www.ccgalberta.com/ccgresources/report05/2003-122-varfit.pdf> and is implemented in the
#' VARFIT module of GSLIB <http://www.gslib.com/>.
#'
#' @references
#' Desassis, N. & Renard, D. Automatic variogram modelling by iterative least squares: univariate
#' and multivariate cases. _Mathematical Geosciences_. Springer Science + Business Media, v. 45, p.
#' 453-470, 2012.
#' 
#' Hiemstra, P. H.; Pebesma, E. J.; Twenhöfel, C. J. & Heuvelink, G. B. Real-time automatic
#' interpolation of ambient gamma dose rates from the Dutch radioactivity monitoring network.
#' _Computers & Geosciences_. Elsevier BV, v. 35, p. 1711-1721, 2009.
#' 
#' Jian, X.; Olea, R. A. & Yu, Y.-S. Semivariogram modelling by weighted least squares. _Computers &
#' Geosciences_. Elsevier BV, v. 22, p. 387-397, 1996.
#' 
#' Larrondo, P. F.; Neufeld, C. T. & Deutsch, C. V. _VARFIT: a program for semi-automatic variogram
#' modelling_. Edmonton: Department of Civil and Environmental Engineering, University of Alberta,
#' p. 17, 2003.
#' 
# @note 
# Package __geoR__ is used to guess the range (scale) parameter of the following spatial covariance
# functions: "matern" (except when `nu = 0.5`), "powered.exponential", "stable", "cauchy",
# "gencauchy", "gneiting", and "gneiting.matern". The practical range is the distance at which the
# semivariance reaches its maximum, i.e. the sill.
#' 
#' @section Dependencies:
# The __geoR__ package, provider of functions for the analysis of geostatistical data in R, is
# required for [pedometrics::variogramGuess()] to work. The development version of the __geoR__
# package is available on <http://www.leg.ufpr.br/geoR/> while its old versions are available on
# the CRAN archive at <https://cran.r-project.org/src/contrib/Archive/geoR/>.
#' 
#' The __georob__ package, provider of functions for the robust geostatistical analysis of spatial
#' data in R, is required for [pedometrics::variogramGuess()] to work. The old versions of the
#' __georob__ package are available on the CRAN archive at
#' <https://cran.r-project.org/src/contrib/Archive/georob/>.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @seealso [pedometrics::variogramBins()]
#' 
#' @examples
# if (all(c(require(sp), require(georob), require(geoR)))) {
#' if (all(c(require(sp), require(georob)))) {
#'   data(meuse, package = "sp")
#'   icp <- variogramGuess(z = log(meuse$copper), coords = meuse[, 1:2])
#' }
#' @concept variogram
# FUNCTION - MAIN ##################################################################################
#' @export
#' @rdname variogramGuess
variogramGuess <-
  function(z, coords, lags, cutoff = 0.5, method = "a", min.npairs = 30, model = "matern", nu = 0.5,
          estimator = "qn", plotit = FALSE) {
    # check if suggested packages are installed
    if (!requireNamespace("georob")) stop("georob package is missing")
    # if (!requireNamespace("geoR")) stop("geoR package is missing") # MOVED TO 
    # check function arguments
    cov_models <- c(
      "matern", "exponential", "gaussian", "spherical", "circular", "cubic", "wave", "linear",
      "power", "powered.exponential", "stable", "cauchy", "gencauchy", "gneiting",
      "gneiting.matern", "pure.nugget")
    if (!model %in% cov_models) {
      stop(paste("model '", model, "' is not implemented", sep = ""))
    }
    # Check lags and max.dist
    if (missing(lags)) {
      if (method %in% c("a", "c")) {
        lags <- variogramBins(coords, cutoff = cutoff)
      } else {
        lags <- variogramBins(coords, n.lags = 15, type = "equi", cutoff = cutoff)
      }
    }
    cutoff <- sqrt(
      sum(apply(apply(coords[, 1:2], 2, range), 2, diff) ^ 2)) * cutoff
    # Compute variogram
    v <- georob::sample.variogram(
      object = z, locations = coords, lag.dist.def = lags, max.lag = cutoff, estimator = estimator)
    lags0 <- length(v$lag.dist)
    # Merge lag-distance classes that have too few point-pairs
    if (any(v$npairs < min.npairs)) {
      message("correcting lags for minimum number of point-pairs...")
      idx <- which(v$npairs < min.npairs) + 1
      while (length(idx) >= 1) {
        lags <- lags[-idx[1]]
        v <- georob::sample.variogram(
          object = z, locations = coords, lag.dist.def = lags, max.lag = cutoff,
          estimator = estimator)
        idx <- which(v$npairs < min.npairs) + 1
      }
    }
    if (plotit) {
      graphics::plot(v)
    }
    # SILL -----------------------------------------------------------------------------------------
    # The initial guess for the sill should be the easiest among all
    # parameters needed to fit a covariance model. Several rules are used in the
    # literature:
    # 1) the average of the maximum semivariance and the median semivariance in 
    #    the sample variogram (Hiemstra et al., 2009);
    # 2) the average of all the experimental points (Larrondo et al., 2003);
    # 3) the average  semivariance of the three last lag-distance classes
    #    (Jian et al., 1996)
    # 4) the total variance (Desassis & Renard, 2012).
    # I define the partial sill as the difference between the variance
    # of the data minus the nugget variance.
    sill <- switch(
      method,
      a = { # Samuel-Rosa
        # The average of the variance of the data and gamma at the two last 
        # lag-distance classes.
        mean(c(stats::var(z), v$gamma[c(length(v$gamma), length(v$gamma) - 1)]))
      },
      b = { # JianEtAl1996
        mean(v$gamma[seq(length(v$gamma) - 2, length(v$gamma))])
      },
      c = { # HiemstraEtAl2009
        mean(c(max(v$gamma), stats::median(v$gamma)))
      },
      d = { # DesassisEtAl2012
        stats::var(z)
      },
      e = { # LarrondoEtAl2003
        mean(v$gamma)
      }
    )
    if (plotit) {
      graphics::abline(h = sill, lty = "dashed")
    }
    # RANGE ----------------------------------------------------------------------------------------
    # In general, the initial guess for the range (scale) parameter is made based on the lag-distance classes
    # and on the dimensions of the study area. A common rule is to compute the initial range as half the 
    # maximum distance up to which lag-distance classes have been defined (Jian et al., 1996; Larrondo et al.,
    # 2003; Desassis & Renard, 2012). Others set the initial range to a proportion of the diagonal of the study
    # area, say 0.1, 0.35 or 0.5 (Hiemstra et al., 2009). This is rather arbitrary: the lag-distance classes
    # usually are defined by some automatic procedure implemented in the software being used, which does not
    # account for the features of the data that is being analysed.
    # I propose using the estimate of the total variance and semivariance of the variable to make an initial
    # guess for the range parameter. The total variance is used here because it is the initial guess of the
    # total sill (see bellow).
    # I start computing the absolute difference between the total variance and the semivariogram in each 
    # lag-distance class (except for the first). Then, I record the index of the lag-distance class where the
    # semivariance is closest to the variance. The separation distance at the centre of this lag-distance class
    # is recorded. This value can be taken to be approximately equivalent to the practical range. Thus, it is
    # corrected using geoR::practicalRange.
    # geoR IS ARCHIVED FROM TIME TO TIME... IT MUST BE REMOVED FRO HERE!!!
    range <- switch(
      method,
      a = { # Samuel-Rosa2015
        v$lag.dist[which.min(abs(v$gamma[-c(1, length(v$gamma))] - sill)) + 1]
      },
      b = { # JianEtAl1996
        lags[length(lags)] * 0.5
      },
      c = { # HiemstraEtAl2009
        sqrt(sum(apply(apply(coords, 2, range), 2, diff) ^ 2)) * 0.1
      },
      d = { # DesassisEtAl2012
        lags[length(lags)] * 0.5
      },
      e = { # LarrondoEtAl2003
        lags[length(lags)]
      }
    )
    # Correct initial guess of the range parameter
    if (model %in% c('linear', 'power')) {
      range <- Inf
    } else if (model == 'pure.nugget') {
      range <- 0
    } else if (model == 'exponential' | model == 'matern' & nu == 0.5) {
      range <- range / 2.995731
    } else if (model %in% c('spherical', 'circular', 'cubic')) {
      range <- range
    } else if (model == 'gaussian') {
      range <- range / 1.730818
    } else if (model == 'wave') {
      range <- range / 2.991457
    } else {
      # "matern", "powered.exponential", "stable", "cauchy", "gencauchy", "gneiting", "gneiting.matern"
      # geoR is archived from time to time...
      # if (requireNamespace("geoR", quietly = TRUE)) {
      #   range <- range / geoR::practicalRange(cov.model = model, phi = 1, kappa = nu)
      # } else {
      #   message(
      #     paste("geoR not installed... returning guess of practical range for model ", model)
      #   )
        range <- range
      # }
    }
    if (plotit) {
      graphics::abline(v = range, lty = "dashed")
    }
    # NUGGET ---------------------------------------------------------------------------------------
    # The initial guess for the nugget variance is commonly made using one of
    # the following rules:
    # 1) use the minimum semivariance in the sample variogram (Hiemstra et al.,
    #    2009)
    # 2) set the initial nugget value to zero (Larrondo et al., 2003)
    # We can also find rules that take into account the difference in the
    # semivariance between the first and second lag-distance classes weighted
    # by the difference in the size of these lag-distance classes (Jian et al.,
    # 1996). The resulting initial guess for the nugget variance is always lower
    # than the minimum semivariance value.
    nugget <- switch(
      method,
      a = { # Samuel-Rosa
        # Is the minimum gamma in lags others than the first?
        # 
        if (which.min(v$gamma) > 1) {
          # 
          # If the minimum gamma is in lags other than in the first, then we 
          # may have one of two possibilities at hand. First, it may be that
          # the best covariance model is that of a pure nugget effect. Second,
          # the data at hand is not appropriate to estimate the behaviour of 
          # the variogram close to the origin. So, what is the best initial
          # guess?
          # 
          # The first task is to check which of the first three lags has the
          # lowest gamma.
          # 
          if (which.min(v$gamma[1:3]) == 2) {
            # 
            # If the second lag has the lowest gamma, we have to find out if 
            # gamma in the first lag is higher than in the third lag.
            # 
            if (v$gamma[1] > v$gamma[3]) {
              #
              # When gamma is the first lag is higher than gamma in the third 
              # lag, the best initial guess is the average of gamma in the 
              # second and third lags.
              # 
              mean(v$gamma[c(2, 2, 3)])
            } else {
              #
              # When gamma in the first lag is lower than in the third lag, then
              # the best initial guess is the average of gamma in the first and
              # second lags.
              # 
              mean(v$gamma[c(1, 2, 2)])
            }
          } else {
            # 
            # The third lag has the lowest gamma.
            # 
            if (v$gamma[1] > v$gamma[2]) {
              mean(v$gamma[c(2, 3, 3)])
            } else {
              mean(v$gamma[c(1, 3, 3)])
            }
          }
          # 
          # The first task is to check if gamma in the first lag is larger than
          # gamma in the second lag. This could easily happen if we have a 
          # pure nugget effect.
          # Is the semivariance of the fist lag larger than that of the second
          # lag? Or is the semivariance of the fist lag larger than the 
          # variance of z? Or is the semivariance of the fist and second lags
          # larger than that of the third? This looks like a messy sample
          # variogram.
          # if (v$gamma[1] >= v$gamma[2] || v$gamma[1] >= var(z) || 
          # which.min(v$gamma[1:3]) == 3) {
          # 
          # Well... Let us simply use the minimum gamma value.
          # min(v$gamma[1:3])
          # 
          # }
          # min(v$gamma)
        } else {
          # The first lag-distance class holds the minimum gamma!
          # 
          # The task now is to estimate the shape of the semivariogram near the
          # origin. The first thing to do is to test if gamma at the second lag
          # is closer to the estimate of the sill, or to gamma at the first lag.
          # If gamma at the second lag is closer to the estimated sill, then it 
          # means that the sample variogram is very steep near the origin.
          # Because the distance between these two lags and the difference in 
          # their sizes are very small, it may be that gamma in the first 
          # lag-distance class is spuriously low (or not).
          # 
          # if (diff(v$gamma[1:2]) > abs(diff(c(v$gamma[2], var(z))))) {
          if (diff(v$gamma[1:2]) > abs(diff(c(v$gamma[2], sill)))) {
            # Gamma in the second lag is closer to the estimated sill than to 
            # gamma in the first lag.
            # 
            # The task now is to evaluate the third lag in relation to the first
            # two. In the ideal situation, gamma will increase monotonically 
            # from the first to the third lag. If not, i.e. gamma in the third
            # lag is lower than in the second lag, then our data is not 
            # appropriate to estimate the behaviour of the variogram near the 
            # origin: gamma in the first lag is too low, and gamma at the second
            # lag is too high (higher than gamma at the third lag). It may be
            # better to use a conservative initial guess, such as the average 
            # of gamma at the first and third lags.
            # 
            if (!identical(order(v$gamma[1:3]), 1:3)) {
              # mean(c(v$gamma[1], min(v$gamma[-1])))
              mean(v$gamma[c(1, 3)])
            } else {
              # Gamma increases monotonically from the first to the third lag,
              # but gamma at the first lag is too low. It may be better to use 
              # a conservative initial guess, such as the average of gamma at 
              # the first and second lags.
              # 
              # mean(v$gamma[c(1, 2)])
              mean(v$gamma[c(1, 1, 2)])
            }
          } else {
            # Gamma in the second lag is closer to gamma in the first lag than
            # to the estimated sill.
            # 
            # This is the most wanted situation because it should enable 
            # estimating the behaviour of the variogram close to the origin with
            # great precision. The question is whether we should use gamma in 
            # the fist lag as the initial guess, or a value lower than that. If
            # the variogram is very steep, then it could be more appropriate to
            # use a value lower than gamma in the first lag as our initial 
            # guess. We could use the estimated range to decide on this matter.
            # a) If the estimated range is lower than the mean lag distance of 
            # the third lag class, then it is likely that the sample variogram 
            # is very steep near the origin (exponential model?). Thus, the 
            # initial guess for the nugget is half the difference between zero 
            # and gamma at the first lag.
            # b) Otherwise, it is likely that the sample variogram rises slowly
            # near the origin (Gaussian model?). The initial guess for the
            # nugget is gamma at the first lag.
            # 
            if (range < v$lag.dist[2]) {
              if (lags0 == length(v$lag.dist)) {
                diff(c(0, v$gamma[1])) * 0.5
              } else {
                v$gamma[1]
              }
            } else {
              v$gamma[1]
            }
          }
        }
      },
      b = { # JianEtAl1996
        max(0, c(v$gamma[1] - (lags[1] / diff(lags[1:2])) * diff(v$gamma[1:2])))
      },
      c = { # HiemstraEtAl2009
        min(v$gamma)
      },
      d = { # DesassisEtAl2012
        1e-12
      },
      e = { # LarrondoEtAl2003
        1e-12
      }
    )
    if (plotit) {
      graphics::abline(h = nugget, lty = "dashed")
    }
    # Guess the partial sill for a pure nugget effect model
    if (nugget < sill) {
      p_sill <- sill - nugget
    } else {
      # p_sill <- 1e-12
      p_sill <- nugget * 1e-3
    }
    # Prepare output
    res <- c(range = range, p_sill = p_sill, nugget = nugget)
    return(res)
  }
#' @export
#' @rdname variogramGuess
vgmICP <- variogramGuess
