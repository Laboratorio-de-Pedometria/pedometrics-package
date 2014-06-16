\name{gcpVector}
\alias{gcpVector}
\title{
Calculate module and azimuth
}
\description{
This function calculates the module and azimuth of the difference on x
and y coordinates between two sets of ground control points.
}
\usage{
gcpVector(dx, dy)
}
\arguments{
  \item{dx}{difference on the \sQuote{x} coordinate. An object of
    class \code{vector} containing the difference on the \sQuote{x}
    coordinate between two sets of ground control points.}
  \item{dy}{difference on the \sQuote{y} coordinate. An object of
    class \code{vector} containing the difference on the \sQuote{y}
    coordinate between two sets of ground control points.}
}
\details{
This function is suited to perform calculations for topographical
coordinates only. The origin is set in the y coordinate, and rotation
performed clockwise.
}
\value{
An object of the class \code{data.frame} containing the module, its
square and azimuth. These three columns are named \sQuote{module},
\sQuote{sq.module} and \sQuote{azimuth}.
}
\references{
Ruiz-Cuetos J.C., Polo M.E. and Rodriguez P.G. (2012).
  \emph{VecStatGraphs2D: Vector analysis using graphical and analytical
  methods in 2D}. R package version 1.6.
  \url{http://CRAN.R-project.org/package=VecStatGraphs2D}
}
\author{
Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}\cr
Maria Eugenia Polo Garcia \email{mepolo@unex.es}\cr
Pablo Garcia Rodriguez \email{pablogr@unex.es}\cr
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\note{
This function was addapted from the function
\code{\link[VecStatGraphs2D]{LoadData}} of
\pkg{VecStatGraphs2D}-package. It is internally called when running
\code{deltagcp()}. Its first version was named \code{gcpstat()}.
}
\seealso{
\pkg{VecStatGraphs2D}-package,
\code{\link[pedometrics]{gcpDiff}}
}
\examples{
\dontrun{
gcpVector(dx = rnorm(3, 5, 10), dy = rnorm(3, 5, 10))
}
}
\keyword{methods}