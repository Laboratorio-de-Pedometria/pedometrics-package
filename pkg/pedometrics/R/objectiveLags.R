#  file pedometrics/R/objectiveLags.R
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
#  Purpose        : count the number of points or point pairs per lag distance 
#                   class; compute the deviation of the observed distribution of 
#                   counts from a prespecified distribution; compute the minimum
#                   count of points or point pairs across all lag distance 
#                   classes
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : G. Heuvelink (gerard.heuvelink@wur.nl)
#
# POINTS PER LAG DISTANCE CLASS
points_per_lag <-
  function (obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints")) {
        stop ("'obj' should be of class SpatialPoints")
      } else {
        if (is.na(proj4string(obj)) || !is.projected(obj)) {
          stop ("'obj' should have a projected CRS") 
        }
      }
    }
    if (missing(lags)) {
      stop ("'lags' is a mandatory argument")
    } else {
      if (!is.numeric(lags)) {
        stop ("'lags' should be a numeric value or vector")
      } 
    }
    if (cutoff != Inf) {
      if (!is.numeric(cutoff)) {
        stop ("'cutoff' should be a numeric value")
      }
      if (length(lags) > 1) {
        stop ("'cutoff' cannot be used when the lag intervals are specified")
      } 
    } else {
      cutoff <- c(dist(t(bbox(obj))))
    }
    d <- coordinates(obj)
    d <- dist(d, method = "euclidean")
    d <- as.matrix(d)
    if (length(lags) == 1) {
      if (lags.type == "equidistant") {
        lags <- seq(0, cutoff, length.out = lags + 1)
      }
      if (lags.type == "exponential") {
        idx <- vector()
        for (i in 1:lags - 1) {
          idx[i] <- lags.base ^ i
        }
        lags <- c(0, rev(cutoff / idx), cutoff)
      }
    }
    pts <- vector()
    for (i in 1:length(lags)) {
      n <- which(d > lags[i] & d <= lags[i + 1], arr.ind = TRUE)
      pts[i] <- length(unique(c(n)))
    }
    res <- data.frame(lag.lower = lags[-length(lags)], 
                      points = pts[-length(lags)], lag.upper = lags[-1])
    return (res)
  }
# OBJECIVE FUNCTION - POINT PAIRS PER LAG DISTANCE CLASS
objPoints <- 
  function (obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf, 
            criterion = "minimum", pre.distribution, weights = 1) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints")) {
        stop ("'obj' should be of class SpatialPoints")
      } else {
        if (is.na(proj4string(obj)) || !is.projected(obj)) {
          stop ("'obj' should have a projected CRS") 
        }
      }
    }
    if (missing(lags)) {
      stop ("'lags' is a mandatory argument")
    } else {
      if (!is.numeric(lags)) {
        stop ("'lags' should be a numeric value or vector")
      } 
    }
    if (cutoff != Inf) {
      if (!is.numeric(cutoff)) {
        stop ("'cutoff' should be a numeric value")
      }
      if (length(lags) > 1) {
        stop ("'cutoff' cannot be used when the lag intervals are specified")
      } 
    }
    n_pts <- length(obj)
    if (length(lags) > 1) {
      n_lags <- length(lags) - 1
    } else {
      n_lags <- lags
    }
    if (criterion == "distribution") {
      if (!missing(pre.distribution)) {
        if (!is.numeric(pre.distribution)) {
          stop ("pre.distribution should be of class numeric")
        }
        if (length(pre.distribution) != n_lags) {
          stop ("the length of 'pre.distribution' should match the number of lags")
        }
      } else {
        pre.distribution <- rep(n_pts, n_lags)
      }
      if (length(weights) == 1) {
        if (weights == 1) {
          weights <- 1 / length(pre.distribution)
        } else {
          stop ("the sum of weights should be equal to 1")
        }
      } else {
        if (length(weights) != n_lags) {
          stop ("the number of weights should match the number of lags")
        }
        if (sum(weights) != 1) {
          stop ("the sum of weights should be equal to 1")
        }
      }
      points <- points_per_lag(obj, lags = lags, cutoff = cutoff, 
                               lags.type = lags.type, lags.base = lags.base)
      ss <- c(pre.distribution - points$points) ^ 2
      ss <- sum(ss * weights)
      return (ss) 
    }
    if (criterion == "minimum") {
      points <- points_per_lag(obj, lags = lags, cutoff = cutoff, 
                               lags.type = lags.type, lags.base = lags.base)
      a <- log10Ceiling(length(obj))
      b <- min(points$points) + 1
      res <- a / b
      return (res)
    }    
  }
# POINT PAIRS PER LAG DISTANCE CLASS
pairs_per_lag <- 
  function (obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints")) {
        stop ("'obj' should be of class SpatialPoints")
      } else {
        if (is.na(proj4string(obj)) || !is.projected(obj)) {
          stop ("'obj' should have a projected CRS") 
        }
      }
    }
    if (missing(lags)) {
      stop ("'lags' is a mandatory argument")
    } else {
      if (!is.numeric(lags)) {
        stop ("'lags' should be a numeric value or vector")
      } 
    }
    if (cutoff != Inf) {
      if (!is.numeric(cutoff)) {
        stop ("'cutoff' should be a numeric value")
      }
      if (length(lags) > 1) {
        stop ("'cutoff' cannot be used when the lag intervals are specified")
      } 
    } else {
      cutoff <- c(dist(t(bbox(obj))))
    }
    d <- coordinates(obj)
    d <- dist(d, method = "euclidean")
    if (length(lags) == 1) {
      if (lags.type == "equidistant") {
        lags <- seq(0, cutoff, length.out = lags + 1)
      }
      if (lags.type == "exponential") {
        idx <- vector()
        for (i in 1:lags - 1) {
          idx[i] <- lags.base ^ i
        }
        lags <- c(0, rev(cutoff / idx), cutoff)
      }
    }
    pairs <- vector()
    for (i in 1:length(lags)) {
      n <- which(d > lags[i] & d <= lags[i + 1])
      pairs[i] <- length(n)
    }
    res <- data.frame(lag.lower = lags[-length(lags)], 
                      pairs = pairs[-length(lags)], lag.upper = lags[-1])
    return (res)
  }
# OBJECIVE FUNCTION - POINT PAIRS PER LAG DISTANCE CLASS
objPairs <- 
  function (obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf,
            criterion = "minimum", pre.distribution, weights = 1) {
    if (missing(obj)) {
      stop ("'obj' is a mandatory argument")
    } else {
      if (!inherits(obj, "SpatialPoints")) {
        stop ("'obj' should be of class SpatialPoints")
      } else {
        if (is.na(proj4string(obj)) || !is.projected(obj)) {
          stop ("'obj' should have a projected CRS") 
        }
      }
    }
    if (missing(lags)) {
      stop ("'lags' is a mandatory argument")
    } else {
      if (!is.numeric(lags)) {
        stop ("'lags' should be a numeric value or vector")
      } 
    }
    if (cutoff != Inf) {
      if (!is.numeric(cutoff)) {
        stop ("'cutoff' should be a numeric value")
      }
      if (length(lags) > 1) {
        stop ("'cutoff' cannot be used when the lag intervals are specified")
      } 
    }
    n_pts <- length(obj)
    if (length(lags) > 1) {
      n_lags <- length(lags) - 1
    } else {
      n_lags <- lags
    }
    if (criterion == "distribution") {
      if (!missing(pre.distribution)) {
        if (!is.numeric(pre.distribution)) {
          stop ("pre.distribution should be of class numeric")
        }
        if (length(pre.distribution) != n_lags) {
          stop ("the length of 'pre.distribution' should match the number of lags")
        }
      } else {
        pre.distribution <- rep(n_pts * (n_pts - 1) / (2 * n_lags), n_lags)
      }
      if (length(weights) == 1) {
        if (weights == 1) {
          weights <- 1 / length(pre.distribution)
        } else {
          stop ("the sum of weights should be equal to 1")
        }
      } else {
        if (length(weights) != n_lags) {
          stop ("the number of weights should match the number of lags")
        }
        if (sum(weights) != 1) {
          stop ("the sum of weights should be equal to 1")
        }
      }
      pairs <- pairs_per_lag(obj, lags = lags, lags.type = lags.type,
                             lags.base = lags.base, cutoff = cutoff)
      ss <- c(pre.distribution - pairs$pairs) ^ 2
      ss <- sum(ss * weights)
      return (ss)  
    }
    if (criterion == "minimum") {
      pairs <- pairs_per_lag(obj, lags = lags, cutoff = cutoff, 
                             lags.type = lags.type, lags.base = lags.base)
      a <- log10Ceiling(n_pts * (n_pts - 1) / (2 * n_lags))
      b <- min(pairs$pairs) + 1
      res <- a / b
      return (res)
    }    
  }
# End!