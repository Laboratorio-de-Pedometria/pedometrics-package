#' Histogram and density plot
#' 
#' @description
#' Plot a histogram and a density plot of a single variable using the R-package __lattice__.
#'
#' @param x Vector of numeric values of the variable for which the histogram and density plot
#' should be created.
#'
#' @param HD Character value indicating the type of plot to be created. Available options are
#' `"over"`, to create a histogram superimposed by the theoretical density plot of a normally
#' distributed variable, and `"stack"`, to create a histogram and an empirical density plot in
#' separated panels. Defaults to `HD = "over"`.
#'
#' @param nint Integer specifying the number of histogram bins. Defaults to `nint = 20`.
#'
#' @param stats Logical to indicate if descriptive statistics of the variable `x` should be added
#' to the plot. Available only when `HD = "over"`. The function tries to automatically find the
#' best location to put the descriptive statistics given the shape of the histogram. Defaults to 
#' `stats = TRUE`.
#'
#' @param digits Integer indicating the number of decimal places to be used 
#' when printing the statistics of the variable `x`. Defaults to `digits = 2`.
#' 
#' @param BoxCox Logical to indicate if the variable `x` should be transformed using the Box-Cox
#' family of power transformations. The estimated lambda value of the Box-Cox transform is printed
#' in the console. It is set to zero when negative. Defaults to `BoxCox = FALSE`.
#' 
#' @param col Vector of two elements, the first indicating the color of the histogram, the second
#' indicating the color of the density plot. Defaults to `col = c("lightgray", "black")`.
#' 
#' @param lwd Vector of two elements, the first indicating the line width of the histogram, the
#' second indicating the line width of the density plot. Defaults to `lwd = c(1, 1)`.
#' 
#' @param lty Character value indicating the line type for the density plot. Defaults to
#' `lty = "dashed"`.
#' 
#' @param xlim Vector of two elements defining the limits of the x axis. The function automatically
#' optimizes `xlim` based on the density plot.
#' 
#' @param ylim Vector of two elements defining the limits of the y axis. The function automatically
#' optimizes `ylim` based both histogram and density plot.
#' 
#' @param ... Other arguments that can be passed to __lattice__ functions. There is no guarantee
#' that they will work.
#' 
#' @details
#' The user should visit the help pages of [lattice::histogram()], [lattice::densityplot()],
#' [lattice::panel.mathdensity()], [car::powerTransform()], and [car::bcPower()] to obtain more
#' details about the main functions used to built [pedometrics::plotHD()].
#' 
#' @return
#' An object of class `"trellis"`. The [lattice::update.trellis()] method can be used to update
#' components of the object and the [lattice::print.trellis()] print method (usually called by
#' default) will plot it on an appropriate plotting device.
#'
#' @references
#' Sarkar, Deepayan (2008) _Lattice: Multivariate Data Visualization with R_, Springer.
#' \url{http://lmdvr.r-forge.r-project.org/}
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @seealso [lattice::histogram()], [lattice::densityplot()], [lattice::panel.mathdensity()]
#' 
#' @section Dependencies:
#' The __car__ package, provider of functions to accompany Fox and Weisberg's An R Companion to
#' Applied Regression, is required for [pedometrics::plotHist()] to work. The development version of
#' the __car__ package is available on <https://r-forge.r-project.org/projects/car/> while its old
#' versions are available on the CRAN archive at
#' <https://cran.r-project.org/src/contrib/Archive/car/>.
#' 
#' @examples
#' if (all(c(require(car), require(lattice), require(latticeExtra)))) {
#'   x <- rnorm(100, 10, 2)
#'   p1 <- plotHist(x, HD = "stack")
#'   p2 <- plotHist(x, HD = "over")
#' }
#' @importFrom stats update
# FUNCTION #########################################################################################
# TODO:
#   - replace 'lattice'-functions with 'graphics'-functions
#' @export
#' @rdname plotHist
plotHist <-
  function(x, HD = "over", nint = 20, digits = 2, stats = TRUE, BoxCox = FALSE,
    col = c("lightgray", "black"), lwd = c(1, 1), lty = "dashed", xlim, ylim, ...) {
    # Check if suggested packages are installed
    if (!requireNamespace("car")) stop("car package is missing")
    if (!requireNamespace("lattice")) stop("lattice package is missing")
    if (!requireNamespace("latticeExtra")) stop("latticeExtra package is missing")
    if (BoxCox) {
      # Check if the variable has negative values
      check <- any(x <= 0)
      if (check) {
        message("data has negative values...")
        x <- x + abs(min(x)) + 1
      }
      lambda <- car::powerTransform(x)
      print(summary(lambda))
      lambda <- as.numeric(lambda$lambda)
      if (lambda < 0) {
        message("estimated lambda value is negative... setting to zero")
        lambda <- 0
      }
      x <- car::bcPower(x, lambda)
    } else {
      lambda <- 1
    }
    if (HD == "over") {
      if (missing(xlim)) {
        xlim <- lattice::densityplot(x)$x.limits
      }
      p <- lattice::histogram(
        x, type = "density", col = col[1], xlim = xlim, nint = nint, lwd = lwd[1], ...,
        panel = function(x, ...) {
          lattice::panel.grid(h = -1, v = -1, lty = "dotted")
          lattice::panel.histogram(x, ...)
          lattice::panel.rug(x, col = col[2], lwd = lwd[1])
          lattice::panel.mathdensity(
            dmath = stats::dnorm, col = col[2], lwd = lwd[2], lty = lty, n = length(x),
            args = list(mean = mean(x), sd = stats::sd(x)))
          }
        )
      if (missing(ylim)) {
        y1 <- p$y.limits
        y2 <- lattice::densityplot(x)$y.limits
        ylim <- c(min(c(y1[1], y2[1])), max(c(y1[2], y2[2])))
      }
      p$y.limits <- ylim
      if (stats) {
        skw <- round(c(skewness(x)), 2)
        leg <- c(paste("Lambda = ", round(lambda, 4), "\n",
                       "Mean = ", round(mean(x), digits), " (",
                                  round(stats::sd(x), digits), ")\n",
                       "Median = ", round(stats::median(x), digits), "\n",
                       "Range = ", round(min(x), digits), "-",
                                   round(max(x), digits), "\n",
                       "Skew = ", skw, sep = ""))
        if (round(skw, 1) >= 0.9) {
          y <- NA
          pos <- NA
          p <- p +
            latticeExtra::layer(
              lattice::panel.text(x = x, y = y, labels = leg, pos = pos),
              data = list(x = c(max(p$x.limits) * 0.99),
                y = c(max(p$y.limits) * 0.8), leg = leg, pos = 2))
        }
        if (round(skw, 1) < 0.9) {
          y <- NA
          pos <- NA
          p <- p +
            latticeExtra::layer(
              lattice::panel.text(x = x, y = y, labels = leg, pos = pos),
              data = list(x = c(min(p$x.limits)), y = c(max(p$y.limits) * 0.8), leg = leg, pos = 4))
        }
      }
    }
    if (HD == "stack") {
      p2 <- lattice::densityplot(
        x, col = col[2], pch = 20, cex = 0.5, n = length(x), lwd = lwd[2], lty = lty, ...)
      if (missing(xlim)) {
        xlim <- p2$x.limits
      }
      p1 <- lattice::histogram(x, col = col[1], xlim = xlim, nint = nint, lwd = lwd[1], ...)
      p <- update(c(p1, p2), layout = c(1, 2), ylab = list(c("Percent of Total", "Density")), ...)
    }
    return(p)
  }
#' @export
#' @rdname plotHist
plotHD <- plotHist
