#' Correlation plot
#'
#' @description
#' Plotting correlation matrices.
#'
#' @param r A square matrix with correlation values.
#'
#' @param r2 (optional) A second square matrix with correlation values.
#'
#' @param col (optional) Color table to use for `image` -- see [graphics::image()] for details. The
#' default is a colorblind-friendly palette created using [RColorBrewer::brewer.pal()] (`"RdBu"`).
#'
#' @param breaks (optional) Break points in sorted order to indicate the intervals for assigning the
#' colors. See [fields::image.plot()] for more details.
#'
#' @param col.names (optional) Character vector with short (up to 5 characters) column names.
#'
#' @param ... (optional) Additional parameters passed to plotting functions.
#'
#' @details
#' A correlation plot in an alternative way of showing the strength of the empirical correlations
#' between variables. This is done by using a diverging color palette, where the darker the color,
#' the stronger the absolute correlation value.
#'
#' [pedometrics::plotCor()] can also be used to compare correlations between the same variables at
#' different points in time or space or for different observations. This is done by passing two
#' square correlation matrices using arguments `r` and `r2`. The lower triangle of the resulting
#' correlation plot will contain correlations from `r`, correlations from `r2` will be in the upper
#' triangle, and the diagonal will be empty.
#'
#' @return
#' A correlation plot.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @examples
#' if (all(c(require(sp), require(fields)))) {
#'   data(meuse, package = "sp")
#'   cols <- c("cadmium", "copper", "lead", "zinc", "elev", "dist", "om")
#'
#'   # A single correlation matrix
#'   r <- cor(meuse[1:20, cols], use = "complete")
#'   r <- round(r, 2)
#'   plotCor(r)
#'
#'   # Two correlation matrices: r2 goes in the upper triangle
#'   r2 <- cor(meuse[21:40, cols], use = "complete")
#'   r2 <- round(r2, 2)
#'   plotCor(r, r2)
#' }
#' 
#' @export
# FUNCTION #########################################################################################
plotCor <-
  function(r, r2, col, breaks, col.names, ...) {
    # Check if suggested packages are installed
    if (!requireNamespace("fields")) stop("fields package is missing")
    # Check function arguments
    if (diff(dim(r)) != 0) {
      stop ("'r' should be a square matrix")
    }
    par0 <- graphics::par()
    on.exit(suppressWarnings(graphics::par(par0)))
    # In case we have two correlation matrices
    # Note that the diagonal is filled with NAs
    if (!missing(r2)) {
      if (diff(dim(r2)) != 0) {
        stop("'r2' should be a square matrix")
      }
      if (ncol(r2) != ncol(r)) {
        stop("'r' and 'r2' should have the same dimensions")
      }
      r[upper.tri(r)] <- r2[upper.tri(r2)]
      diag(r) <- NA_real_
    }
    # Missing color ramp
    if (missing(col)) {
      # Colors obtained using RColorBrewer::brewer.pal with arguments n = 11 and name = "RdBu"
      col <-  c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
        "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
      col <- grDevices::colorRampPalette(col)
      col <- rev(col(100))
    }
    # Missing breaks
    if (missing(breaks)) {
      breaks <- seq(-1, 1, length.out = 101)
    }
    n_col <- ncol(r)
    # The correlation matrix need to be transposed/transformed
    r <- t(r[n_col:1, ])
    fields::image.plot(r, axes = FALSE, col = col, breaks = breaks, legend.shrink = 1)
    graphics::box()
    graphics::text(
      x = (rep(1:n_col, n_col) - 1) / (n_col - 1),
      y = (rep(1:n_col, each = n_col) - 1) / (n_col - 1),
      labels = as.numeric(r), col = ifelse(abs(as.numeric(r)) >= 0.9, "ivory", "black"), ...)
    at <- seq(0, 1, length.out = n_col)
    # Column names
    if (missing(col.names)) {
      col_names <- colnames(r)
    } else {
      col_names <- col.names
    }
    if (any(nchar(col_names) > 5)) {
      idx_names <- paste("x", seq(n_col), " - ", rev(col_names), sep = "", collapse = "; ")
      message(paste("Too long column names found. Replacing with ", idx_names, ".", sep = ""))
      col_names <- paste("x", seq(n_col), sep = "")
    }
    graphics::axis(side = 1, at = at, labels = col_names, las = 2)
    graphics::axis(side = 2, at = at, labels = rev(col_names), las = 1)
    if (!missing(r2)) {
      graphics::axis(side = 3, at = at, labels = col_names, las = 2)
      graphics::axis(side = 4, at = at, labels = rev(col_names), las = 1)
    }
  }
