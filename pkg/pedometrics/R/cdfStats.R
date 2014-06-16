#  file pedometrics/R/cdfStats.R
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
#  Purpose        : descriptive statistics of a CDF
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : 
#  Version        : 0.1-0
#  Depends on     : 
#  Dependency of  : cdfTable()
#  Notes          : Known to work only with quantitative variables.
#                   Tested only in Ubuntu
#  TODOs          : 
#
#  Timeline
#     Dec 2013: first version (by A. Samuel-Rosa)
#  22 Mar 2014: Changed function name from cdfstats() to cdfStats() to comply
#               with programming style convention. Corrected funtion definition.
#               (by A. Samuel-Rosa)
#  16 Jun 2014: Corrected function call. Improved documentation.

cdfStats <- 
  function(obj, ind, all = TRUE) {
    # Summary statistics of a cumulative distribution function.
    #
    # Args:
    #   obj: Object with the estimated cumulative distribution function.
    #   ind: Indicator variable.
    #   all: Summary statistics to be returned.
    #
    # Returns:
    #   Summary statistics of a cumulative distribution function.
    #
    stats <- data.frame(obj$Pct[obj$Pct$Indicator == ind, 4:9][8:10, ],
                        row.names = NULL)
    if(all) {
      res <- stats  
    } else {
      res <- stats[1, 3]
    }
    res
  }
# End!