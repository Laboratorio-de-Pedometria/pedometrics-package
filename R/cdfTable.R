# Table with descriptive statistics of an estimated cumulative distribution
# function
# 
# This function returns a table containing the descriptive statistics of the
# cumulative distribution function of a set of continuous variables. TeX code
# is printed to copy and paste in a document.
# 
# Summary statistics included in the table (estimated population mean and
# standard deviation) are obtained from the resulting object of
# \code{cont.analysis()} by internally using the function \code{cdfStats()}.
# 
# There are two types of data that can be submitted to function
# \code{cdfTable()}.  The first (\code{type = "xy"}) is composed by two
# instances (\sQuote{x} and \sQuote{y}) and is produced during horizontal
# (positional) validation exercises (validation in the geographic space).
# Thus, \sQuote{x} and \sQuote{y} represent, respectively, the horizontal
# displacement (error) in \sQuote{x} and \sQuote{y} coordinates.
# 
# The second type of data (\code{type = "z"}) is composed by only one instance
# (\sQuote{z}) and is generated by vertical validation exercises (validation
# in the attribute space). Thus, \sQuote{z} represents the vertical
# displacement (error) of the attribute \sQuote{z} being measured.
# 
# @param x Object with the estimated cumulative distribution function of the
# set of continuous variables. The resulting object of \code{cont.analysis()}
# of \pkg{spsurvey}-package.
# @param type Type of data under analysis. Defaults to \code{type = "xy"}. See
# \sQuote{Details}.
# @param rounding Rounding level of the data in the output table. Defaults to
# \code{rounding = 0}.
# @param tex Logical for creating TeX code. Defaults to \code{tex = FALSE}.
# @param data.frame Logical for returning a data.frame object. Defaults to
# \code{data.frame = FALSE}.
# @return Returned value depends on how arguments \code{type} and \code{tex}
# are set. \item{list("type")}{ If \code{type = "xy"}, then the function
# returns a table with estimated population mean and standard deviation of
# error statistics for \sQuote{x} and \sQuote{y} coordinates. These error
# statistics include the mean error, mean absolute error, and mean square
# error. It also returns the estimated mean and mean square error vector
# (module), and the estimated mean azimuth. The number of ground control
# points used to make the estimates is printed by default.
# 
# If \code{type = "z"}, then the function returns a table with estimated
# population mean and standard deviation of error statistics for \sQuote{z},
# the attribute under analysis. These error statistics include the mean error,
# mean absolute error, and mean square error. The number of ground control
# points used to make the estimates is printed by default. }
# \item{list("tex")}{ If \code{tex = TRUE}, them the function prints the TeX
# code for the table defined by the argument \code{type}. Otherwise the TeX
# code is not generated. }
# @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
# @seealso \code{\link[pedometrics]{cdfStats}}.
# @references Kincaid, T. M. and Olsen, A. R. (2013).  spsurvey: Spatial
# Survey Design and Analysis.  R package version 2.6. URL: \url{https://www.epa.gov/}.
# @export
# @examples
# 
# \dontrun{
# if (require(spsurvey)) {
# ## Estimate the CDF
# my.cdf <- spsurvey::cont.analysis(spsurvey.obj = my.spsurvey)
# 
# ## Print table and TeX code
# cdfTable(my.cdf)
# }
# }
# 
# FUNCTION #####################################################################
cdfTable <-
  function(x, type = "xy", rounding = 0, tex = FALSE, data.frame = FALSE) {
    if (type == "xy") {
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
      colnames(tb) <- c("", "xcoord", "sd", "ycoord", "sd", "module", "sd", "azimuth", "sd")
      if (tex == TRUE) {
        xt <- xtable::xtable(tb, NA.string = NA, digits = rounding,
          caption = paste0("Descriptive statistics (n = ", cdfStats(x, "dx")[1, 2], ")."))
      }
      if (data.frame) {
        return(tb)
      }
      else {
        cat("-------------------------------------------------", "\n")
        cat("Descriptive statistics (n = ", cdfStats(x, "dx")[1, 2], ")", sep = "", "\n")
        cat("-------------------------------------------------", "\n")
        print(tb)
        cat("-------------------------------------------------", "\n")
        if (tex == TRUE) {
          print(xt, caption.placement = "top")
        }
      }
    }
    if (type == "z") {
      tb <- data.frame(c("Mean", "Absolute mean", "Squared mean"),
                       round(c(cdfStats(x, "dz")[1, 3],
                               cdfStats(x, "abs.dz")[1, 3],
                               cdfStats(x, "sq.dz")[1, 3]), rounding),
                       round(c(cdfStats(x, "dz")[3, 3],
                               cdfStats(x, "abs.dz")[3, 3],
                               cdfStats(x, "sq.dz")[3, 3]), rounding)
                       )
      colnames(tb) <- c("", "z", "sd")
      if (tex == TRUE) {
        xt <- xtable::xtable(tb, NA.string = NA, digits = rounding,
          caption = paste("Descriptive statistics (n = ", cdfStats(x, "dz")[1, 2], ")."))
      }
      if (data.frame) {
        return(tb)
      }
      else {
        cat("-------------------------------------------------", "\n")
        cat("Descriptive statistics (n = ", cdfStats(x, "dz")[1, 2], ")", sep = "", "\n")
        cat("-------------------------------------------------", "\n")
        print(tb)
        cat("-------------------------------------------------", "\n")
        if (tex == TRUE) {
          print(xt, caption.placement = "top")
        }
      }
    }
  }
