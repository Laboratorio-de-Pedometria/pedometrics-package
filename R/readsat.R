# Purpose        : Optimize importing of satellite imagery to GRASS GIS;
# Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com); 
# Contributions  : ;
# Status         : beta;
# Depends        : GRASS GIS 6+, GDAL;
# Note           : tested only under Ubuntu 13.10;

readsat <-
  function(input, output, sat, in_prefix, in_suffix, out_prefix, ...) {
    UseMethod("readsat")
  }

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