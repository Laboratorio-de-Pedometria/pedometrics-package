#  file pedometrics/R/gcpDiff.R
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
#  Purpose        : calculate differences between xyz coordinates
#  Maintainer     : A Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  :  
#  Version        : beta
#  Depends        : gcpDiff()
#  Note           : tested only in Ubuntu 13.10
#  TODOs          :
#
#  Timeline
#     Dec 2013: First version. (by A. Samuel-Rosa)
#  23 Mar 2014: Changed function name from deltagcp() to gcpDiff() to comply
#               with programming style convention. Corrected funtion definition.
#               (by A. Samuel-Rosa)
#  16 Jun 2014: Corrected function call. Improved documentation.

gcpDiff <- 
  function(measured, predicted, type = "xy", aggregate = FALSE, rounding = 0) {
    # Calculate differences between xy or z coordinates of ground control points
    #
    # Args:
    #   measured:  Reference ground control points.
    #   predicted: Point data being validated.
    #   type:      Type of data under analysis.
    #   aggregate: Boolean for aggregating the data from cluster sampling.
    #   rounding:  Rounding level of the data in the output data frame
    #
    # Returns:
    #   A data frame with differences in xy or z coordinates.
    #
    if(type == "xy") {  # difference in the geographic space
      measured <- coordenadas(measured)
      predicted <- coordenadas(predicted)
      # Because the number of GCPs of 'measured' and 'predicted'
      # may not be the same
      measured <- measured[match(predicted[, 1], measured[, 1]), ][, 2:3]
      diff <- predicted[, 2:3] - measured
      names(diff) <- c("dx", "dy")
      azim <- gcpVector(dx = diff$dx, dy = diff$dy)
      abso <- abs(diff)
      colnames(abso) <- c("abs.dx", "abs.dy")
      quad <- diff * diff
      colnames(quad) <- c("sq.dx", "sq.dy")
      diff <- round(diff, rounding)
      abso <- round(abso, rounding)
      quad <- round(quad, rounding)
      azim <- round(azim, rounding)
      siteID <- predicted$siteID
      erro <- data.frame(siteID, diff, abso, quad, azim)
      erro <- erro[order(as.numeric(row.names(erro))), ]
      row.names(erro) <- NULL
      return(erro)
    }
    if(type == "z") {  # difference in the attribute space
      measured <- measured
      predicted <- predicted
      dz <- predicted$z - measured$z
      abs.dz <- abs(dz)
      sq.dz <- dz*dz
      dz <- round(dz, rounding)
      abs.dz <- round(abs.dz, rounding)
      sq.dz <- round(sq.dz, rounding)
      siteID <- predicted$siteID
      if(aggregate == FALSE) {
        erro <- data.frame(siteID, dz, abs.dz, sq.dz)
        row.names(erro) <- NULL
        return(erro)
      }
      if(aggregate == TRUE) {
        erro <- data.frame(dz, abs.dz, sq.dz)
        by <- list(siteID)
        names(by) <- "siteID"
        erro <- aggregate(erro, by, FUN = mean)
        #erro = data.frame(siteID = seq(1:length(dz)), erro)
        row.names(erro) <- NULL
        return(erro)
      }
    }
    print(erro)
  }
# End!