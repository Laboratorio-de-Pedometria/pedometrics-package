#  file pedometrics/R/readsat.R
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
# Purpose        : Optimize importing of satellite imagery to GRASS GIS;
# Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com); 
# Contributions  : ;
# Status         : beta;
# Depends        : GRASS GIS 6+, GDAL;
# Note           : tested only under Ubuntu 13.10;
#
#  Timeline
#     Dec 2013: first version (by A. Samuel-Rosa)
#  16 Jun 2014: Corrected function call. Improved documentation.

readsat <-
  function(import = "single", dir = getwd(), overwrite = FALSE, input,
           output, sat, in_prefix, in_suffix, out_prefix) {
    dir <- dir
    if(import == "single") {
      if(overwrite) {
        system(paste("r.in.gdal --overwrite --verbose input=", dir, input,
                     " output=", output, sep = ""))
        } else {
          system(paste("r.in.gdal --verbose input=", dir, input,
                               " output=", output, sep = ""))
        }
    }
    if(import == "group") {
      if(overwrite) {
        system(paste("r.in.gdal -k --overwrite --verbose input=", dir, input,
                     " output=", output, sep = ""))
      } else {
        system(paste("r.in.gdal -k --verbose input=", dir, input, " output=",
                     output, sep = ""))
      }
    }
    if(import == "multi") {
      dir0 <- getwd()
      setwd(dir)
      if(sat == "landsat5tm") {
        bands <- seq(1, 7, 1)
      }
      if(sat == "landsat7etm") {
        bands <- seq(1, 7, 1)
      }
      # set output file names
      out_prefix <- in_prefix
      output <- paste(out_prefix, ".", bands, sep = "")
      # get input file names
      input <- lapply(bands, function(x){
        paste(in_prefix, x, in_suffix, sep = "")
        }
        )
      # read files
      if(overwrite) {
        files <- paste("r.in.gdal --overwrite --verbose input=",
                       input, " output=", output, sep = "")
      } else {
        files <- paste("r.in.gdal --verbose input=",
                       input, " output=", output, sep = "")
      }
      files <- lapply(files, function(x) {
        system(x)
        }
        )
      setwd(dir0)
    }
  }
# End!