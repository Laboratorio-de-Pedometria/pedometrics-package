# Purpose        : Calculates top-of-atmosphere radiance (or reflectance) and temperature;
# Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com); 
# Contributions  : ;
# Status         : beta;
# Depends        : spgrass6, XML, stringr;
# Note           : works only with Landsat imagery, tested only under Ubuntu 13.10; 

toar <-
  function(in_prefix, out_prefix, sat, meta, ...) {
    UseMethod("toar")
  }

toar <-
  function(in_prefix, out_prefix, sat, meta, correct = TRUE,
                   rad = TRUE){
            # read metadata (xml file)
            meta = xmlChildren(xmlParse(meta))$prdf
            # set satellite and sensor data
            sensor.tab = rbind(c("LANDSAT 1 MSS", "landsat1mss", "mss1"),
                               c("LANDSAT 2 MSS", "landsat2mss", "mss2"),
                               c("LANDSAT 3 MSS", "landsat3mss", "mss3"),
                               c("LANDSAT 4 MSS", "landsat4mss", "mss4"),
                               c("LANDSAT 5 MSS", "landsat5mss", "mss5"),
                               c("LANDSAT 4 TM", "landsat4tm", "tm4"),
                               c("LANDSAT 5 TM", "landsat5tm", "tm5"),
                               c("LANDSAT 7 ETM", "landsat7etm", "etm7"),
                               c("LANDSAT 8 OLI/TIRS", "landsat8oli", "ot8"))
            colnames(sensor.tab) <- c("sensor.meta", "sensor.arg", "code")
            sensor = paste(xmlValue(meta[["satellite"]][["name"]]),
                           xmlValue(meta[["satellite"]][["number"]]),
                           xmlValue(meta[["satellite"]][["instrument"]]),
                           sep = " ")
            sensor = sensor.tab[sensor.tab[,1] == sensor, "code"]
            sat = sensor.tab[sensor.tab[,2] == sat, "code"]
            # evaluate satellite and sensor data
            if(sensor != sat){
              stop("satellite sensor does not match metadata file content")
              }else{
                # date of imaging
                date = str_sub(xmlValue(meta[["viewing"]][["center"]]), 1L, 10L)
                # date of processing (product date)
                product_date =
                  str_sub(xmlValue(meta[["image"]][["processingTime"]]), 1L, 10L)
                # sun elevation
                sun_elevation =
                  xmlValue(meta[["image"]][["sunPosition"]][["elevation"]])
                if(correct){
                  method = "corrected"
                  }else{
                    method = "uncorrected"
                  }
                # select radiance (-r) or reflectance value output
                if(rad){
                  system(paste("i.landsat.toar --verbose -n -r input_prefix=",
                               in_prefix, " output_prefix=", out_prefix,
                               " sensor=", sensor, " method=", method,
                               " date=", date, " sun_elevation=", sun_elevation,
                               " product_date=", product_date, sep = ""))
                  }else{
                    system(paste("i.landsat.toar --verbose -n input_prefix=",
                                 in_prefix, " output_prefix=", out_prefix,
                                 " sensor=", sensor, " method=", method,
                                 " date=", date, " sun_elevation=", sun_elevation,
                                 " product_date=", product_date, sep = ""))
                  }
              }
          }
          
# End!