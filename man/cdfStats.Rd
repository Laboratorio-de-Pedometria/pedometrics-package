\name{cdfStats}
\alias{cdfStats}
\title{
Descriptive statistics of the cumulative distribution function of a
continuos variable
}
\description{
This function returns summary statistics of the cumulative distribution function
of a continuous variable estimated with \pkg{spsurvey}-package.
}
\usage{
cdfStats(obj, ind, all = TRUE)
}
\arguments{
  \item{obj}{
  Object containing the estimated cumulative distribution function of the
  continuous variable. The resulting object of \code{cont.analysis()} of
  \pkg{spsurvey}-package.
  }
  \item{ind}{
  Indicator variable. The name of the continuous variable as displayed in the
  resulting object of \code{cont.analysis()}.
  }
  \item{all}{
  Summary statistics to be returned. The default option (\code{all = TRUE})
  returns all summary statistics available. If \code{all = FALSE}, then only
  estimated population mean and standard deviation are returned. See
  \sQuote{Details}.
  }
}
\details{
The function \code{cont.analysis()} of \pkg{spsurvey}-package estimates the
population total, mean, variance, and standard deviation of a continuous 
variable. It also estimates the standard error and confidence bounds of these 
population estimates. In some cases it may be interesting to see all estimates, 
for which one uses \code{all = TRUE}. However, in other circumstances there
might be interest only in taking a look at the estimated population mean and
standard deviation. Then the argument \code{all} has to be set to \code{FALSE}.
}
\value{
A \code{data.frame} containing summary statistics of the cumulative
distribution function of a continuous variable.
}
\references{
Kincaid, T. M. and Olsen, A. R. (2013). spsurvey: Spatial Survey
  Design and Analysis. R package version 2.6. URL: 
  <http://www.epa.gov/nheerl/arm/>.
}
\author{
Alessandro Samuel-Rosa <\email{alessandrosamuelrosa@gmail.com}>
}
\seealso{
  \code{\link[spsurvey]{cont.analysis}}.
}
\examples{
\dontrun{
## Estimate the CDF
my.cdf <- cont.analysis(spsurvey.obj = my.spsurvey)

## See indicator levels in the resulting object
levels(my.cdf$Pct$Indicator)

## Return all summary statistics of indicator variable 'dx'
cdfStats(my.cdf, "dx", all = TRUE)
}
}
\keyword{methods}
\keyword{print}