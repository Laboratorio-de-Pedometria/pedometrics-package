#' Model series plot
#' 
#' @description 
#' Produce a graphical output to examine the effect of using different model specifications (design)
#' on the predictive performance of these models (a model series). Devised to access the results of
#' [pedometrics::buildModelSeries()] and [pedometrics::statsMS()], but can be easily adapted to
#' work with any model structure and performance measure.
#' 
#' @param obj Object of class `data.frame`, generally returned by [pedometrics::statsMS()],
#' containing:
#' 
#'    1. a series of performance statistics of several models, and
#'    2. the design information of each model.
#' 
#' See \sQuote{Details} for more information.
#' 
#' @param grid Vector of integer values or character strings indicating the columns of the
#' `data.frame` containing the design data which will be gridded using the function
#' [lattice::levelplot()]. See \sQuote{Details} for more information.
#' 
#' @param line Character string or integer value indicating which of the performance statistics
#' (usually calculated by [pedometrics::statsMS()]) should be plotted using the function
#' [lattice::xyplot()]. See \sQuote{Details} for more information.
#' 
#' @param ind Integer value indicating for which group of models the mean rank is to be calculated.
#' See \sQuote{Details} for more information.
#' 
#' @param type Vector of character strings indicating some of the effects to be used when plotting
#' the performance statistics using [lattice::xyplot()]. Defaults to `type = c("b", "g")`. See
#' [lattice::panel.xyplot()] for more information on how to set this argument.
#' 
#' @param pch Vector with two integer values specifying the symbols to be used to plot points. The
#' first sets the symbol used to plot the performance statistic, while the second sets the symbol
#' used to plot the mean rank of the indicator set using argument `ind`. Defaults to
#' `pch = c(20, 2)`. See [graphics::points()] for possible values and their interpretation.
#' 
#' @param size Numeric value specifying the size of the symbols used for plotting the mean rank of
#' the indicator set using argument `ind`. Defaults to `size = 0.5`. See [grid::grid.points()] for
#' more information.
#' 
#' @param arrange Character string indicating how the model series should be arranged, which can be
#' in ascending (`"asc"`) or descending (`"desc"`, default) order.
# See [plyr::arrange()] for more information.
#' 
#' @param color Vector defining the colors to be used in the grid produced by function
#' [lattice::levelplot()]. If `color = NULL`, defaults to `color = cm.colors(n)`, where `n` is the
#' number of unique values in the columns defined by argument `grid`. See [grDevices::cm.colors()]
#' to see how to use other color palettes.
#' 
#' @param xlim Numeric vector of length 2, giving the x coordinates range. If `xlim = NULL` (which
#' is the recommended value), defaults to `xlim = c(0.5, dim(obj)[1] + 0.5)`. This is, so far, the
#' optimum range for adequate plotting.
#' 
#' @param ylab Character vector of length 2, giving the y-axis labels. When `obj` is a `data.frame`
#' returned by [pedometrics::statsMS()], and the performance statistic passed to argument
#' `line` is one of those calculated by [pedometrics::statsMS()] (`"candidates"`, `"df"`, `"aic"`,
#' `"rmse"`, `"nrmse"`, `"r2"`, `"adj_r2"`, or `"ADJ_r2"`), the function tries to automatically
#' identify the correct `ylab`.
#' 
#' @param xlab Character vector of unit length, the x-axis label. Defaults `xlab = "Model ranking"`.
#' 
#' @param at Numeric vector indicating the location of tick marks along the x axis (in native
#' coordinates).
#' 
#' @param ... Other arguments for plotting, although most of these have no been tested. Argument
#' `asp`, for example, is not effective since the function automatically identifies the best aspect
#' for plotting based on the dimensions of the design data.
#' 
#' @details
#' This section gives more details about arguments `obj`, `grid`, `line`, `arrange`, and `ind`.
#' 
#' \subsection{obj}{
#' The argument `obj` usually constitutes a `data.frame` returned by [pedometrics::statsMS()].
#' However, the user can use any `data.frame` object as far as it contains the two basic units of
#' information needed:
#' \enumerate{
#' \item design data passed with argument `grid`
#' \item performance statistic passed with argument `line`
#' }
#' }
#' \subsection{grid}{
#' The argument `grid` indicates the _design_ data which is used to produce the grid output in the
#' top of the model series plot. By _design_ we mean the data that specify the structure of each
#' model and how they differ from each other. Suppose that eight linear models were fit using three
#' types of predictor variables (`a`, `b`, and `c`). Each of these predictor variables is available
#' in two versions that differ by their accuracy, where `0` means a less accurate predictor
#' variable, while `1` means a more accurate predictor variable. This yields 2^3 = 8 total possible
#' combinations. The _design_ data would be of the following form:
#' 
#' \verb{
#' > design
#'   a b c
#' 1 0 0 0
#' 2 0 0 1
#' 3 0 1 0
#' 4 1 0 0
#' 5 0 1 1
#' 6 1 0 1
#' 7 1 1 0
#' 8 1 1 1
#' }
#' }
#' \subsection{line}{
#' The argument `line` corresponds to the performance statistic that is used to arrange the models
#' in ascending or descending order, and to produce the line output in the bottom of the model
#' series plot. For example, it can be a series of values of adjusted coefficient of determination,
#' one for each model:
#' 
#' \verb{
#' adj_r2 <- c(0.87, 0.74, 0.81, 0.85, 0.54, 0.86, 0.90, 0.89)
#' }
#' }
#' \subsection{arrange}{
#' The argument `arrange` automatically arranges the model series according to the performance
#' statistics selected with argument `line`. If `obj` is a `data.frame` returned by
#' [pedometrics::statsMS()], then the function uses standard arranging approaches. For most
#' performance statistics, the models are arranged in descending order. The exception is when
#' `"r2"`, `"adj_r2"`, or `"ADJ_r2"` are used, in which case the models are arranged in ascending
#' order. This means that the model with lowest value appears in the leftmost side of the model
#' series plot, while the models with the highest value appears in the rightmost side of the plot.
#' 
#' \verb{
#' > arrange(obj, adj_r2)
#'   id a b c adj_r2
#' 1  5 1 0 1   0.54
#' 2  2 0 0 1   0.74
#' 3  3 1 0 0   0.81
#' 4  4 0 1 0   0.85
#' 5  6 0 1 1   0.86
#' 6  1 0 0 0   0.87
#' 7  8 1 1 1   0.89
#' 8  7 1 1 0   0.90
#' }
#' 
#' This results suggest that the best performing model is that of `id = 7`, while the model of
#' `id = 5` is the poorest one.
#' }
#' \subsection{ind}{
#' The model series plot allows to see how the design influences model performance. This is achieved
#' mainly through the use of different colors in the grid output, where each unique value in the
#' _design_ data is represented by a different color. For the example given above, one could try to
#' see if the models built with the more accurate versions of the predictor variables have a better
#' performance by identifying their relative distribution in the model series plot. The models
#' placed at the rightmost side of the plot are those with the best performance.
#' 
#' The argument `ind` provides another tool to help identifying how the design, more specifically
#' how each variable in the _design_ data, influences model performance. This is done by simply
#' calculating the mean ranking of the models that were built using the updated version of each
#' predictor variable. This very same mean ranking is also used to rank the predictor variables and
#' thus identify which of them is the most important.
#' 
#' After arranging the `design` data described above using the adjusted coefficient of
#' determination, the following mean rank is obtained for each predictor variable:
#' 
#' \verb{
#' > rank_center
#'      a    b    c
#' 1 5.75 6.25 5.25
#' }
#' 
#' This result suggests that the best model performance is obtained when using the updated version
#' of the predictor variable `b`. In the model series plot, the predictor variable `b` appears in
#' the top row, while the predictor variable `c` appears in the bottom row.
#' }
#' @return
#' An object of class `"trellis"` consisting of a model series plot.
#' 
#' @references
#' Deepayan Sarkar (2008). _Lattice: Multivariate Data Visualization with R._ Springer, New York.
#' ISBN 978-0-387-75968-5.
#' 
#' Roger D. Peng (2008). _A method for visualizing multivariate time series data._ Journal of
#' Statistical Software. v. 25 (Code Snippet), p. 1-17.
#' 
#' Roger D. Peng (2012). _mvtsplot: Multivariate Time Series Plot._ R package version 1.0-1. 
#' <https://CRAN.R-project.org/package=mvtsplot>.
#' 
#' A. Samuel-Rosa, G. B. M. Heuvelink, G. de Mattos Vasques, and L. H. C. dos Anjos, Do more
#' detailed environmental covariates deliver more accurate soil maps?, _Geoderma_, vol. 243–244,
#' pp. 214–227, May 2015, doi: 10.1016/j.geoderma.2014.12.017.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @section Dependencies:
# The __plyr__ package, provider of tools for splitting, applying and combining data in R, is
# required for [pedometrics::plotModelSeries()] to work. The development version of the __plyr__
# package is available on <https://github.com/hadley/plyr> while its old versions are available on
# the CRAN archive at <https://cran.r-project.org/src/contrib/Archive/plyr/>.
#' 
#' The __grDevices__ package, provider of graphics devices and support for colours and fonts in R,
#' is required for [pedometrics::plotModelSeries()] to work.
#' 
#' The __grid__ package, a rewrite of the graphics layout capabilities in R, is required for
#' [pedometrics::plotModelSeries()] to work.
#' 
#' @note
#' Some of the solutions used to build this function were found in the source code of the R-package
#' __mvtsplot__. As such, the author of that package, Roger D. Peng \email{rpeng@@jhsph.edu}, is
#' entitled \sQuote{contributors} to the R-package __pedometrics__.
#'
#' @section Warning:
#' Use the original functions [lattice::xyplot()] and [lattice::levelplot()] for higher
#' customization.
#'
#' @seealso [lattice::xyplot()] [lattice::levelplot()]
#' 
#' @examples
# if (all(require(plyr), require(grDevices), require(grid))) {
#' if (all(require(grDevices), require(grid))) {
#'   # This example follows the discussion in section "Details"
#'   # Note that the data.frame is created manually
#'   id <- c(1:8)
#'   design <- data.frame(a = c(0, 0, 1, 0, 1, 0, 1, 1),
#'                        b = c(0, 0, 0, 1, 0, 1, 1, 1),
#'                        c = c(0, 1, 0, 0, 1, 1, 0, 1))
#'   adj_r2 <- c(0.87, 0.74, 0.81, 0.85, 0.54, 0.86, 0.90, 0.89)
#'   obj <- cbind(id, design, adj_r2)
#'   p <- plotModelSeries(obj, grid = c(2:4), line = "adj_r2", ind = 1, 
#'               color = c("lightyellow", "palegreen"),
#'               main = "Model Series Plot")
#' }
#' @keywords hplot
#' @importFrom stats update
# FUNCTION #########################################################################################
#' @export
#' @rdname plotModelSeries
plotModelSeries <-
  function(obj, grid, line, ind, type = c("b", "g"), pch = c(20, 2), size = 0.5, arrange = "desc",
    color = NULL, xlim = NULL, ylab = NULL, xlab = NULL, at = NULL, ...) {
    # check if suggested packages are installed
    if (!requireNamespace("grDevices")) stop("grDevices package is missing")
    # if (!requireNamespace("lattice")) stop("lattice package is missing")
    if (!requireNamespace("grid")) stop("grid package is missing")
    # if (!requireNamespace("plyr")) stop("plyr package is missing")
    # check function arguments
    if (missing(obj)) {
      stop("'obj' is a mandatory argument")
    }
    if (missing(grid)) {
      stop("'grid' is a mandatory argument")
    }
    if (missing(line)) {
      stop("'line' is a mandatory argument")
    }
    if (missing(ind)) {
      stop("'ind' is a mandatory argument")
    }
    if (!inherits(obj, "data.frame")) {
      stop("'obj' should be of class data.frame")
    }
    if (!inherits(grid, c("integer", "character", "numeric"))) {
      stop("'grid' should be an integer value or a character string")
    }
    if (!inherits(line, c("integer", "character", "numeric"))) {
      stop("'line' should be an integer value or a character string")
    }
    if (!inherits(ind, c("integer", "numeric")) || round(ind) != ind) {
      stop("'ind' should be an integer value")
    }
    if (inherits(line, c("integer", "numeric"))) {
      nam0 <- c("candidates", "df", "aic", "rmse", "nrmse", "r2", "adj_r2", "ADJ_r2")
      nam1 <- colnames(obj)[line]
      if (!any(colnames(obj)[line] == nam0)) {
        stop(paste0("'ylab' should be provided for performance statistics '", nam1, "'"))
      }
    }
    if (!missing(xlab)) {
      if (length(xlab) != 1) {
        stop("'xlab' should have length equal to 1")
      }
    }
    if (!missing(ylab)) {
      if (length(ylab) != 2) {
        stop("'ylab' should have length equal to 2")
      }
    }
    if (length(type) != 2) {
      stop("'type' should have length equal to 2")
    }
    if (length(pch) != 2) {
      stop("'pch' should have length equal to 2")
    }
    # prepare data
    if (inherits(line, "numeric")) {
      line <- colnames(obj)[line]
    }
    if (any(line == c("r2", "adj_r2", "ADJ_r2"))) {
      # obj <- plyr::arrange(obj, plyr::desc(obj[, line]))
      idx_arrange <- order(obj[[line]], decreasing = TRUE)
      obj <- obj[idx_arrange, ]
    } else {
      # obj <- plyr::arrange(obj, obj[, line])
      idx_arrange <- order(obj[[line]], decreasing = FALSE)
      obj <- obj[idx_arrange, ]
    }
    grid <- as.matrix(obj[, grid])
    x <- seq(1, dim(obj)[1], 1)
    y <- as.numeric(obj[, line])
    if (missing(at)) {
      if (max(x) < 100) {
        m <- round(max(x) / 10) * 10
        at <- c(1, seq(5, m, 5))
      } else {
        m <- round(max(x) / 10) * 10
        at <- c(1, seq(10, m, by = 10))
      }
    }
    if (missing(color)) {
      color <- grDevices::cm.colors(length(unique(as.numeric(grid))))
    }
    if (missing(xlim)) {
      xlim <- c(0.5, dim(obj)[1] + 0.5)
    }
    if (missing(xlab)) {
      xlab <- "Model ranking"
    }
    if (missing(ylab)) {
      if (inherits(line, "numeric")) {
        line <- colnames(obj)[line]
      }
      if (line == "candidates") {
        yl <- "Candidate predictors"
      }
      if (line == "df") {
        yl <- "Degrees of freedom"
      }
      if (line == "aic") {
        yl <- "AIC"
      }
      if (line == "rmse") {
        yl <- "RMSE"
      }
      if (line == "nrmse") {
        yl <- "NRMSE"
      }
      if (line == "r2") {
        yl <- expression(paste0(R^2))
      }
      if (any(line == c("adj_r2", "ADJ_r2"))) {
        yl <- expression(paste0("Adjusted ", R^2))
      }
      ylab <- list(c(yl, "Design"))
    }
    rank_center <- rep(NA, dim(grid)[2])
    for (i in seq_along(rank_center)) {
      rank_center[i] <- mean(cbind(x, grid)[, 1][which(cbind(x, grid)[, i + 1] == ind)])
    }
    grid <- grid[, order(rank_center, decreasing = TRUE)]
    p1 <- lattice::xyplot(
      y ~ x, xlim = rev(grDevices::extendrange(xlim, f = 0)), type = type, pch = pch[1],
      scales = list(y = list(rot = 0), x = list(at = at)))
    p2 <- lattice::levelplot(
      grid, colorkey = FALSE, xlim = rev(grDevices::extendrange(xlim, f = 0)),
      col.regions = color, scales = list(y = list(rot = 90)),
      panel = function (...) {
        lattice::panel.levelplot(...)
        grid::grid.points(x = sort(rank_center, decreasing = TRUE),
          seq(1, dim(grid)[2], 1), pch = pch[2], size = grid::unit(size, "char"))
        })
    # Print plot
    update(c(p1, p2), layout = c(1, 2), xlab = xlab,
      ylab = ylab, aspect = c((dim(grid)[2] * 2) / dim(grid)[1]),
      par.settings = list(layout.heights = list(panel = c(0.5, 0.5))), ...)
    }
#' @export
#' @rdname plotModelSeries
plotMS <- plotModelSeries
