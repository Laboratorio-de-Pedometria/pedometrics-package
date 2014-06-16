# Purpose        : obtain coordinates and site ID
# Maintainer     : Alessandro Samuel-Rosa (alessandrosamuelrosa@gmail.com)
# Contributions  : ; 
# Version        : 0.1-0
# Note           : prepare design argument for spsurvey.analysis

coordenadas <-
  function(x) {
    UseMethod("coordenadas")
  }

coordenadas <-
  function(x) {
    coo <- data.frame(x$siteID, coordinates(x))
    coo <- coo[order(as.numeric(x$siteID)), ]
    colnames(coo) <- c("siteID", "xcoord", "ycoord")
    row.names(coo) <- NULL
    return(coo)
  }
# End!