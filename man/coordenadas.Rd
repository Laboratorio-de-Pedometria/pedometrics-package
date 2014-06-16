\name{coordenadas}
\alias{coordenadas}
\title{Prepare object for argument 'design' of spsurvey.analysis
       function}
\description{
  This function returns an object to feed the argument 'design' when
  creating an object of class \code{spsurvey.analysis}.
}
\usage{
coordenadas(x)
}
\arguments{
  \item{x}{object from which id and xy coordinates are to be returned.
        An object of class \code{SpatialPointsDataFrame}.
}
}
\details{
  The argument 'design' used to create object of class
  \code{spsurvey.analysis} requires a series of inputs. However, it can
  be feed with data about site ID and coordinates. This function
  returns a data frame that provides this information, assuming that
  all other design variables are provided manualy in the arguments
  list.
}
\value{
  An object of class \code{data.frame} containing three columns with
  names \code{siteID}, \code{xcoord}, and \code{ycoord}.
}
\references{
Kincaid, T. M. and Olsen, A. R. (2013). spsurvey: Spatial Survey
  Design and Analysis. R package version 2.6. URL: 
  <\url{http://www.epa.gov/nheerl/arm/}>.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\seealso{
  \code{\link[pedometrics]{gcpDiff}},
  \code{\link[spsurvey]{cont.analysis}}.
}
\examples{
\dontrun{
## Create an spsurvey.analysis object
my.spsurvey <- 
  spsurvey.analysis(design = coordenadas(my.data),
                    data.cont = delta(ref.data, my.data),
                    popcorrect = TRUE, pcfsize = length(my.data$id),
                    support = rep(1, length(my.data$id)),
                    wgt = rep(1, length(my.data$id)), vartype = "SRS")
}
}
\keyword{methods}