#' Categorize/stratify continuous variable(s)
#' 
#' Create break points, compute strata proportions, and stratify continuous variable(s) to create categorical 
#' variable(s).
#' 
#' @param x Vector, data frame or matrix with data on the continuous variable(s) to be categorized/stratified.
#' 
#' @param breaks Vector or list containing the lower and upper limits that should be used to break the 
#' continuous variable(s) into categories. See \sQuote{Details} for more information.
#' 
#' @param n Integer value indicating the number of strata that should be created.
#' 
#' @param type Character value indicating the type of strata that should be used, with options `"area"`, for
#' equal-area, and `"range"`, for equal-range strata. Defaults to `type = "area"`.
#' 
#' @param integer Logical value indicating if the categorical variable(s) be returned as `integer`s. Defaults 
#' to `integer = FALSE`, i.e. the variable(s) will be returned as `factor`s.
#' 
#' @param prop Logical value indicating if the strata proportions should be returned? Defaults to 
#' `prop = FALSE`.
#' 
#' @details
#' Argument `breaks` must be a vector if `x` is a vector, but a list if `x` is a data frame or matrix. Using a
#' list allows breaking each column of `x` into different number of categories.
#' 
#' @return
#' A vector, data frame, or matrix, depending on the class of `x`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[Hmisc]{cut2}}
#' @aliases cont2cat breakPoints stratify
#' @examples
#'
#' ## Compute the break points of marginal strata
#' x <- data.frame(x = round(rnorm(10), 1), y = round(rlnorm(10), 1))
#' x <- breakPoints(x = x, n = 4, type = "area", prop = TRUE)
#' x
#'
#' ## Convert continuous data into categorical data
#' # Matrix
#' x <- y <- c(1:10)
#' x <- cbind(x, y)
#' breaks <- list(c(1, 2, 4, 8, 10), c(1, 5, 10))
#' y <- cont2cat(x, breaks)
#' y
#' # Data frame
#' x <- y <- c(1:10)
#' x <- data.frame(x, y)
#' breaks <- list(c(1, 2, 4, 8, 10), c(1, 5, 10))
#' y <- cont2cat(x, breaks, integer = TRUE)
#' y
#' # Vector
#' x <- c(1:10)
#' breaks <- c(1, 2, 4, 8, 10)
#' y <- cont2cat(x, breaks, integer = TRUE)
#' y
#' 
#' ## Stratification
#' x <- data.frame(x = round(rlnorm(10), 1), y = round(rnorm(10), 1))
#' x <- stratify(x = x, n = 4, type = "area", integer = TRUE)
#' x
# FUNCTION - CONVERT CONTINUOUS DATA INTO CATEGORICAL DATA ####################################################
#' @export
#' @rdname cont2cat 
cont2cat <-
  function (x, breaks, integer = FALSE) {
    
    # Check if suggested packages are installed
    if (!requireNamespace("SpatialTools", quietly = TRUE)) {
      stop(paste("Package 'SpatialTools' needed for this function to work. ",
                 "Please install it.", sep = ""), call. = FALSE)
    }
    
    # Process input
    x_cl <- class(x)
    x <- as.data.frame(x)
    n_col <- ncol(x)
    if (n_col == 1) breaks <- list(breaks)
    
    for (i in 1:n_col) {
      
      # Check break points (adapted from Hmisc::cut2)
      r <- range(x[, i], na.rm = TRUE)
      if (r[1] < min(breaks[[i]])) breaks[[i]] <- c(r[1], breaks[[i]])
      if (r[2] > max(breaks[[i]])) breaks[[i]] <- c(breaks[[i]], r[2])
      
      # Cut data
      x[, i] <- cut(x = x[, i], breaks = breaks[[i]], include.lowest = TRUE, right = FALSE)
    }
    
    # Process output
    if (integer) x <- sapply(x, as.integer)
    if (n_col == 1) x <- as.vector(x)
    # if (x_cl == "matrix") x <- as.matrix(x)
    # 2020-february-03
    # The NEWS for current r-devel contains:
    #     * matrix objects now also inherit from class "array", namely, e.g.,
    #     class(diag(1)) is c("matrix", "array") which invalidates code
    #     assuming that length(class(obj)) == 1, an incorrect assumption that
    #     is less frequently fulfilled now.
    # Martin Maechler: When you think `class(.) == *`, think again!
    # https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html
    if ('matrix' %in% x_cl) {
      x <- as.matrix(x)
    }
    
    # Output
    return (x)
  }
# FUNCTION - COMPUTE BREAK POINTS AND STRATA PROPORTIONS ######################################################
#' @export
#' @rdname cont2cat 
breakPoints <-
  function (x, n, type = "area", prop = FALSE) {
    
    vec <- is.vector(x)
    x <- as.data.frame(x)
    n_col <- ncol(x)
    
    # Equal-area strata
    if (type == "area") {
      
      # Compute the break points using discrete sample quantiles
      probs <- seq(0, 1, length.out = n + 1)
      breaks <- lapply(x, stats::quantile, probs, na.rm = TRUE, type = 3)
      
    } else { # Equal-range strata
      
      # Compute the break points
      breaks <- lapply(1:n_col, function(i)
        seq(min(x[, i]), max(x[, i]), length.out = n + 1))
      
      # Find and replace by the closest population value
      d <- lapply(1:n_col, function(i)
        SpatialTools::dist2(matrix(breaks[[i]]), matrix(x[, i])))
      d <- lapply(1:n_col, function(i) apply(d[[i]], 1, which.min))
      breaks <- lapply(1:n_col, function(i) breaks[[i]] <- x[d[[i]], i])
    }
    
    # Keep only the unique break points
    breaks <- lapply(breaks, unique)
    
    # Compute the proportion of points per marginal strata
    if (prop) {
      count <- lapply(1:n_col, function (i)
        graphics::hist(x[, i], breaks[[i]], plot = FALSE)$counts
      )
      prop <- lapply(1:n_col, function (i) count[[i]] / sum(count[[i]]))
      names(prop) <- colnames(x)
      res <- list(breaks = breaks, prop = prop)
      
    } else {
      if (vec) {
        res <- unlist(breaks)
      } else {
        res <- breaks
      }
    }
    
    # Output
    return (res)
  }
# FUNCTION - MARGINAL STRATIFICATION ##########################################################################
#' @export
#' @rdname cont2cat 
stratify <-
  function (x, n, type = "area", integer = FALSE) {
    
    # Compute break points
    breaks <- breakPoints(x = x, n = n, type = type)
    
    # Convert continuous data into categorical data
    x <- cont2cat(x = x, breaks = breaks, integer = integer)
    
    # Output
    return (x)
  }
