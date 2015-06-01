# Population statistics
# 
# Calculate population statistics (variance and covariance).
# 
# @param x a numeric vector
# @param y a numeric vector
# 
# @references
# Rencher, A. C. Methods of multivariate analysis. New York: John Wiley & Sons,
# p. 708, 2002.
# @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
# @seealso \code{\link[stats]{cor}}
# @examples
# x <- rnorm(10)
# y <- rnorm(10)
# res <- data.frame(Variance = c(var(x), VAR(x)),
#                   Covariance = c(cov(x, y), COV(x, y)),
#                   row.names = c("Sample", "Population"))
# res
# Covariance ###################################################################
COV <-
  function (x, y) {
    # ASR: The second formulation is faster
    #sum((x - mean(x)) * (y - mean(y))) / length(x)
    mean(x * y) - mean(x) * mean(y)
  }
# Variance #####################################################################
VAR <-
  function (x) {
    # ASR: The second formulation is faster
    #mean((x - mean(x)) ^ 2)
    mean(x ^ 2) - mean(x) ^ 2
  }
