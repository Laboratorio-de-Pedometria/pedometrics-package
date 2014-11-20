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
# function to round a number to the immediately higher order of magnitude
# .log10Ceiling <- 
#   function (x) {
#     x <- log10(x)
#     x <- ceiling(x)
#     x <- 10 ^ x
#     return (x)
#   }
# POINTS PER LAG DISTANCE CLASS
pointsPerLag <-
  function (points, candidates, lags, lags.type = "equidistant", 
            lags.base = 2, cutoff = NULL) {
    d <- as.matrix(dist(candidates[points, ], method = "euclidean"))
    if (length(lags) == 1) {
      if (lags.type == "equidistant") {
        lags <- seq(0, cutoff, length.out = lags + 1)
      }
      if (lags.type == "exponential") {
        idx <- lags.base ^ c(1:lags - 1)
        lags <- c(0, rev(cutoff / idx))
      }
    }
    pts <- apply(X = d, 1, function (X) {
      table(cut(X, breaks = lags, include.lowest = FALSE))
      })
    return (pts)
  }
# OBJECIVE FUNCTION - POINT PAIRS PER LAG DISTANCE CLASS
objPoints <- 
  function (points, candidates, lags, lags.type = "equidistant", lags.base = 2,
            cutoff = NULL, criterion = "minimum", pre.distri) {
    n_pts <- dim(points)[1]
    if (length(lags) > 1) {
      n_lags <- length(lags) - 1
    } else {
      n_lags <- lags
    }
    if (criterion == "distribution") {
      if (!missing(pre.distri)) {
        if (!is.numeric(pre.distri)) {
          stop ("pre.distri should be of class numeric")
        }
        if (length(pre.distri) != n_lags) {
          stop ("the length of 'pre.distri' should match the number of lags")
        }
      } else {
        pre.distri <- rep(n_pts, n_lags)
      }
      points <- pointsPerLag(points, lags = lags, cutoff = cutoff, 
                               lags.type = lags.type, lags.base = lags.base)
      res <- sum(pre.distri - points$points)
      return (res) 
    }
    if (criterion == "minimum") {
      points <- pointsPerLag(points, lags = lags, cutoff = cutoff, 
                               lags.type = lags.type, lags.base = lags.base)
      #a <- .log10Ceiling(dim(points)[1])
      #b <- min(points$points) + 1
      #res <- a / b
      res <- 10000 / (min(points$points) + 1)
      return (res)
    }    
  }
# POINT PAIRS PER LAG DISTANCE CLASS
pairsPerLag <- 
  function (points, lags, lags.type = "equidistant", lags.base = 2,
            cutoff = NULL) {
    if (missing(points)) {
      stop ("'points' is a mandatory argument")
    }
    if (missing(lags) || !is.numeric(lags)) {
      stop ("'lags' should be a numeric value or vector")
    }
    if (length(lags) == 1 && is.null(cutoff)) {
      stop ("'cutoff' is a mandatory when the lag intervals are not specified") 
    }
    if (length(lags) > 1 && !is.null(cutoff)) {
      stop ("'cutoff' cannot be used when the lag intervals are specified")
    }
    d <- dist(points, method = "euclidean")
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
  function (points, lags, lags.type = "equidistant", lags.base = 2,
            cutoff = NULL, criterion = "minimum", pre.distri) {
    if (missing(points)) {
      stop ("'points' is a mandatory argument")
    }
    if (missing(lags) || !is.numeric(lags)) {
      stop ("'lags' should be a numeric value or vector")
    }
    if (length(lags) == 1 && is.null(cutoff)) {
      stop ("'cutoff' is a mandatory when the lag intervals are not specified") 
    }
    if (length(lags) > 1 && !is.null(cutoff)) {
      stop ("'cutoff' cannot be used when the lag intervals are specified")
    }
    n_pts <- dim(points)[1]
    if (length(lags) > 1) {
      n_lags <- length(lags) - 1
    } else {
      n_lags <- lags
    }
    if (criterion == "distribution") {
      if (!missing(pre.distri)) {
        if (!is.numeric(pre.distri)) {
          stop ("pre.distri should be of class numeric")
        }
        if (length(pre.distri) != n_lags) {
          stop ("the length of 'pre.distri' should match the number of lags")
        }
      } else {
        pre.distri <- rep(n_pts * (n_pts - 1) / (2 * n_lags), n_lags)
      }
      pairs <- pairsPerLag(points, lags = lags, lags.type = lags.type,
                             lags.base = lags.base, cutoff = cutoff)
      res <- sum(pre.distri - pairs$pairs)
      return (res)
    }
    if (criterion == "minimum") {
      pairs <- pairsPerLag(points, lags = lags, cutoff = cutoff, 
                             lags.type = lags.type, lags.base = lags.base)
      #a <- .log10Ceiling(n_pts * (n_pts - 1) / (2 * n_lags))
      #b <- min(pairs$pairs) + 1
      #res <- a / b
      res <- 10000 * (min(pairs$pairs) + 1)
      return (res)
    }    
  }
# End!