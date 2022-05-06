#' Association between categorical variables
#'
#' @description 
#' Compute the Cramer's V, a descriptive statistic that measures the association between categorical
#' variables.
#'
#' @param x Data frame or matrix with a set of categorical variables.
#'
#' @details
#' Any integer variable is internally converted to a factor.
#'
#' @return
#' A matrix with the Cramer's V between the categorical variables.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @note
#' The original code is available at \url{https://sas-and-r.blogspot.com/}, Example 8.39:
#' calculating Cramer's V, posted by Ken Kleinman on Friday, June 3, 2011. As such, Ken Kleinman
#' \email{Ken_Kleinman@@hms.harvard.edu} is entitled a \sQuote{contributor} to the R-package
#' __pedometrics__.
#'
# The function \code{bigtabulate} used to compute the chi-squared test is the main bottleneck in
# the current version of [pedometrics::cramer()]. Ideally it will be implemented in C++.
#'
#' @references
#' Cramér, H. _Mathematical methods of statistics_. Princeton: Princeton University Press, p. 575,
#' 1946.
#'
#' Everitt, B. S. _The Cambridge dictionary of statistics_. Cambridge: Cambridge University Press,
#' p. 432, 2006.
#'
#' @examples
#' if (interactive()) {
#' data(meuse, package = "sp")
#' str(meuse)
#' test <- cramer(meuse[, c("ffreq", "soil", "lime", "landuse")])
#' }
#' @export
# FUNCTION #########################################################################################
cramer <-
  function(x) {
    nam <- colnames(x)
    # perform some checks
    check <- sapply(x, is.factor)
    if (any(check == FALSE)) {
      check <- which(check == FALSE)
      for (i in check)
      x[, i] <- as.data.frame(sapply(x[, i], as.factor))
    }
    check <- sapply(x, nlevels)
    if (any(check < 2L)) {
      stop("each variable must have at least 2 levels")
    }
    # the statistics is calculated using a for loop
    res <- matrix(NA, nrow = dim(x)[2], ncol = dim(x)[2])
    for (i in 1:dim(x)[2]) {
      for (j in 1:dim(x)[2]) {
        n <- length(x[, i])
        x2 <- .chisqStat(x[, i], x[, j]) / n
        #x2 <- suppressWarnings(chisq.test(x[, i], x[, j], correct = FALSE))
        #x2 <- x2$statistic / n
        den <- min(length(unique(x[, i])), length(unique(x[, j]))) - 1
        #res[i, j] <- as.numeric(sqrt(x2 / den))
        res[i, j] <- sqrt(x2 / den)
      }
    }
    colnames(res) <- nam
    rownames(res) <- nam
    return(res)
  }
# Pearson's Chi-squared statistic ##############################################
.chisqStat <-
  function(x, y) {
    #x <- table(x, y)
    OBSERVED <- table(x, y)
    # OBSERVED <- bigtabulate::bigtabulate(cbind(x, y), ccols = c(1, 2))
    n <- sum(OBSERVED)
    sr <- rowSums(OBSERVED)
    sc <- colSums(OBSERVED)
    EXPECTED <- outer(sr, sc, "*") / n
    #YATES <- 0
    #STATISTIC <- sum((abs(OBSERVED - EXPECTED) - YATES) ^ 2 / EXPECTED)
    #STATISTIC <- sum(abs(OBSERVED - EXPECTED) ^ 2 / EXPECTED)
    STATISTIC <- sum(abs(OBSERVED - EXPECTED) ^ 2 / EXPECTED, na.rm = TRUE)
    return(STATISTIC)
  }
