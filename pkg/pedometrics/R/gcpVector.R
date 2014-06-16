#  file pedometria/R/gcpVector.R
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

#  Purpose        : calculate module and azimuth
#  Maintainer     : A Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : 0.1-0
#  Depends on     : 
#  Dependency of  : gcpDiff()
#  Note           : tested only in Ubuntu 13.10 
#  TODOs          :

#  Timeline
#     Dec 2014: first version (by A. Samuel-Rosa)
#  23 Mar 2014: Changed function name from gcpvector() to gcpVector() to comply
#               with programming style convention. Corrected funtion definition.
#               (by A. Samuel-Rosa)

gcpVector <-
  function(dx, dy) {
    UseMethod("gcpVector")
  }

gcpVector <-
  # Calculate the module and azimuth
  #
  # Args:
  #   dx: difference on the x coordinate
  #   dy: difference on the y coordinate
  #
  # Returns:
  #   Module, its square and azimuth.
  
  function (dx, dy) {
    vec <- sqrt((dx * dx) + (dy * dy))
    vec2 <- vec * vec
    azim1 <- (180 * (atan(dx / dy)) / pi)
    quad <- dy >= 0
    azim1[is.na(azim1)] <- 0
    azim2 <- azim1
    azim2[quad == FALSE] <- azim1[quad == FALSE] + 180
    quad2 <- azim2 >= 0
    azim3 <- azim2
    azim3[quad2 == FALSE] <- azim2[quad2 == FALSE] + 360
    azim <- rep(NA, length(dx))
    azim <- azim3
    res <- data.frame(vec, vec2, azim)
    colnames(res) <- c("module", "sq.module", "azimuth")
    return(res)
  }