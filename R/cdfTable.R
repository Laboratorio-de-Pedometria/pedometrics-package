#  Purpose        : tex table with descriptive statistics of a CDF
#  Maintainer     : A. Samuel-Rosa (alessandrosamuelrosa@gmail.com)
#  Contributions  : ; 
#  Version        : 0.1-0
#  Depends on     : cdfStats()
#  Dependency of  : 
#  Note           : Known to work only with quantitative variables.
#                 : Tested only using Ubuntu.
#  TODOs          : Make more tests.

#  Timeline
#     Dec 2013: first version (by A. Samuel-Rosa)
#  22 Mar 2014: Changed function name from cdfStats() to cdfStats() to comply
#               with programming style convention. Corrected funtion definition.
#               (by A. Samuel-Rosa)
#  25 May 2014: Added option to return a data.frame object.

cdfTable <- 
  function(x, ...) {
    UseMethod("cdfTable")
  }

cdfTable <- 
function(x, type = "xy", rounding = 0, tex = FALSE, data.frame = FALSE) {
  # Table with descriptive statistics of a CDF
  #
  # Args:
  #   x:        Object with the estimated CDF.
  #   type:     Type of data under analysis.
  #   rounding: Rounding level of the data in the output table.
  #   tex:      Logical for creating TeX code.
  #   table:    Logical for returning a data.frame object.
  #
  # Returns:
  #   Table with descriptive statistics.
  
  if(type == "xy") {
    tb <- data.frame(c("Mean", "Absolute mean", "Squared mean"),
                     round(c(cdfStats(x, "dx")[1, 3],
                             cdfStats(x, "abs.dx")[1, 3],
                             cdfStats(x, "sq.dx")[1, 3]), rounding),
                     round(c(cdfStats(x, "dx")[3, 3],
                             cdfStats(x, "abs.dx")[3, 3],
                             cdfStats(x, "sq.dx")[3, 3]), rounding),
                     round(c(cdfStats(x, "dy")[1, 3],
                             cdfStats(x, "abs.dy")[1, 3],
                             cdfStats(x, "sq.dy")[1, 3]), rounding),
                     round(c(cdfStats(x, "dy")[3, 3],
                             cdfStats(x, "abs.dy")[3, 3],
                             cdfStats(x, "sq.dy")[3, 3]), rounding),
                     round(c(cdfStats(x, "module")[1, 3],
                             NA,
                             cdfStats(x, "sq.module")[1, 3]), rounding),
                     round(c(cdfStats(x, "module")[3, 3],
                             NA,
                             cdfStats(x, "sq.module")[3, 3]), rounding),
                     round(c(cdfStats(x, "azimuth")[1, 3],
                             NA,
                             NA), rounding),
                     round(c(cdfStats(x, "azimuth")[3, 3],
                             NA,
                             NA), rounding))
    colnames(tb) <- c("", "xcoord", "sd", "ycoord", "sd",
                      "module", "sd", "azimuth", "sd")
    if(tex == TRUE) {
      xt <- xtable(tb, NA.string = NA, digits = rounding,
                   caption = paste("Descriptive statistics (n = ",
                                  cdfStats(x, "dx")[1, 2], ").",
                                  sep = ""))
    }
    if (data.frame) {
      return (tb)
    }
    else {
      cat("-------------------------------------------------", "\n")
      cat("Descriptive statistics (n = ", cdfStats(x, "dx")[1, 2], ")",
          sep = "", "\n")
      cat("-------------------------------------------------", "\n")
      print(tb)
      cat("-------------------------------------------------", "\n")
      if(tex == TRUE) {
        print(xt, caption.placement = "top")
      }
    }
  }
  if(type == "z") {
    tb <- data.frame(c("Mean", "Absolute mean", "Squared mean"),
                     round(c(cdfStats(x, "dz")[1, 3],
                             cdfStats(x, "abs.dz")[1, 3],
                             cdfStats(x, "sq.dz")[1, 3]), rounding),
                     round(c(cdfStats(x, "dz")[3, 3],
                             cdfStats(x, "abs.dz")[3, 3],
                             cdfStats(x, "sq.dz")[3, 3]), rounding)
                     )
    colnames(tb) <- c("", "z", "sd")
    if(tex == TRUE) {
      xt <- xtable(tb, NA.string = NA, digits = rounding,
                   caption = paste("Descriptive statistics (n = ",
                                   cdfStats(x, "dz")[1,2], ").",
                                   sep = ""))
    }
    if (data.frame) {
      return (tb)
    }
    else {
      cat("-------------------------------------------------", "\n")
      cat("Descriptive statistics (n = ", cdfStats(x, "dz")[1, 2], ")",
          sep = "", "\n")
      cat("-------------------------------------------------", "\n")
      print(tb)
      cat("-------------------------------------------------", "\n")
      if(tex == TRUE) {
        print(xt, caption.placement = "top")
      }  
    }
  }
}
# End!