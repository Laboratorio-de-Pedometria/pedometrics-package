\name{adjR2}
\alias{adjR2}
\title{
Adjusted coefficient of determination
}
\description{
Calculates the adjusted coefficient of determination of a multiple linear 
regression model.
}
\usage{
adjR2(r2, n, p)
}

\arguments{
  \item{r2}{
  Numeric vector with the coeficient of deternmination to be adjusted.
  }
  \item{n}{
  Numeric vector providing the number of observations used to fit the multiple
  linear regression model.
  }
  \item{p}{
  Numeric vector providing the number of parameters included in the multiple
  linear regression model.
  }
}
\details{
Details will be added soon.
}
\value{
A numeric vector with the adjusted coefficient of determination.
}
\references{
Coefficient of determination. Wikipedia, The Free Encyclopedia.
Available at <\url{http://en.wikipedia.org/wiki/Coefficient_of_determination#Adjusted_R2}>.
[Online; accessed 31-July-2014]
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\examples{
\dontrun{
adjR2(r2 = 0.95, n = 100, p = 80)
}
}
\keyword{misc}%RShowDoc("KEYWORDS")
% End!