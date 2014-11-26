\name{rowMinCpp}
\alias{rowMinCpp}
\title{
Return the minimum value in each row of a numeric matrix
}
\description{
This function returns the minimum value in each row of a numeric matrix.
}
\usage{
rowMinCpp(x)
}

\arguments{
  \item{x}{
Numeric matrix with two or more rows and/or columns.
}
}
\details{
This function is implemented in C++ to speed-up the computation time for large matrices.
}
\value{
A numeric vector with the minimum value of each row if the matrix.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\seealso{
\code{\link[matrixStats]{rowMins}}
}
\examples{
x <- matrix(rnorm(20), nrow = 5)
rowMinCpp(x)
}
\keyword{univar}
% End!