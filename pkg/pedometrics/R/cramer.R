#' Association between categorical variables
#' 
#' Computes the Cramer's V, a measure of association between categorical 
#' variables
#' 
#' @param x data.frame or matrix.
#' @details Any integer variable is internally converted to a factor. 
#' @return A matrix with the Cramer's V between the categorical variables.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @note The original code was available at \url{http://sas-and-r.blogspot.nl/},
#' Example 8.39: calculating Cramer's V, posted by Ken Kleinman on Friday, June
#' 3, 2011. As such, Ken Kleinman <\email{Ken_Kleinman@@hms.harvard.edu}> is 
#' entitled a \sQuote{contributor} to the R-package \pkg{pedometrics}.
#' @references
#' Cramér, H. Mathematical methods of statistics. Princeton: Princeton 
#' University Press, p. 575, 1946.
#' 
#' Everitt, B. S. The Cambridge dictionary of statistics. Cambridge: Cambridge 
#' University Press, p. 432, 2006.
#' @seealso \code{\link[vcd]{assocstats}}
#' @export
#' @examples
#' helpdata <- read.csv("http://www.math.smith.edu/r/data/help.csv")
#' data <- helpdata[, c("female", "homeless", "racegrp")]
#' cramer(data)
# FUNCTION #####################################################################
cramer <-
  function (x) {
    nam <- colnames(x)
    
    # perform some checks
    check <- sapply(x, is.factor)
    if (any(check != TRUE)) {
      x <- as.data.frame(sapply(x, as.factor))
    }
    check <- sapply(x, nlevels)
    if (any(check < 2L)) {
      stop("the columns of 'x' must have at least 2 levels")
    }
    
    # the statistics is calculated using a for loop
    res <- matrix(NA, nrow = dim(x)[2], ncol = dim(x)[2])
    for (i in 1:dim(x)[2]) {
      for (j in 1:dim(x)[2]) {
        n <- length(x[, i])
        x2 <- .chisqTest(x[, i], x[, j]) / n
        #x2 <- suppressWarnings(chisq.test(x[, i], x[, j], correct = FALSE))
        #x2 <- x2$statistic / n
        den <- min(length(unique(x[, i])), length(unique(x[, j]))) - 1
        res[i, j] <- as.numeric(sqrt(x2 / den))
      }
    }
    colnames(res) <- nam
    rownames(res) <- nam
    return (res) 
  }
# Pearson's Chi-squared Test ###################################################
.chisqTest <-
  function (x, y) {
    #x <- table(x, y)
    x <- bigtabulate(cbind(x, y), ccols = c(1, 2))
    n <- sum(x)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*") / n
    YATES <- 0
    STATISTIC <- sum((abs(x - E) - YATES) ^ 2 / E)
    return (STATISTIC)
  }
