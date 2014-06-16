#  file pedometrics/R/coordenadas.R
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
# Purpose        : obtain coordinates and site ID
# Maintainer     : Alessandro Samuel-Rosa (alessandrosamuelrosa@gmail.com)
# Contributions  : ; 
# Version        : 0.1-0
# Note           : prepare design argument for spsurvey.analysis
#  TODOs          : Clean the code. Remove unecessary arguments.
#
#  Timeline
#     Dec 2013: first version (by A. Samuel-Rosa)
#  16 Jun 2014: Corrected function call. Improved documentation.

coordenadas <-
  function(x) {
    coo <- data.frame(x$siteID, coordinates(x))
    coo <- coo[order(as.numeric(x$siteID)), ]
    colnames(coo) <- c("siteID", "xcoord", "ycoord")
    row.names(coo) <- NULL
    return(coo)
  }
# End!