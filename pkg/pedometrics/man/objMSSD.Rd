\name{objMSSD}
\alias{objMSSD}
\alias{distMat}
\alias{distToNearestPoint}
\alias{nearestPoint}
\title{
Mean (squared) shortest distance
}
\description{
Functions to calculate the mean (squared) shortest distance between a set of points and all grid cells.
}
\usage{
distMat(candidates, exponent = 1, diagonal = 0)
objMSSD(points, pred.grid, dist.mat)
distToNearestPoint(dist.mat, which.pts)
nearestPoint(dist.mat, which.pts)
}
\arguments{
\item{candidates, pred.grid}{
A matrix or data.frame. The population of all grid locations in the spatial domain. See \code{spJitterFinite} and \sQuote{Details} for more information.
}
\item{exponent}{
Defaults to \code{exponent = 1}.
}
\item{diagonal}{
Defaults to \code{diagonal = 0}.
}
\item{points}{
A data.frame or matrix with the points to be perturbed. \code{points} must be a subset of \code{candidates}. See \sQuote{Details} for more information.
}
\item{dist.mat}{
Distance matrix of the population of all grid locations in the spatial domain as defined by \code{candidates}. A matrix returned by \code{distMat}.
}
\item{which.pts}{
A vector of the indexes defining the subset of \code{candidates} that corresponds to the set of \code{points}. It indicates the columns of the distance matrix \code{dist.mat} that correspond to the points to which distances should be computed.
}
}
\details{
Square the distance values to get the mean squared shortest distance.

More details will be added soon!
}
\value{
A numeric value: the mean (squared) shortest distance between a set of points and all grid cells.
}
\references{
Brus, D. J.; de Gruijter, J. J.; van Groenigen, J. W. Designing spatial coverage samples using the k-means clustering algorithm. In: P. Lagacherie, A. M.; Voltz, M. (Eds.) \emph{Digital soil mapping - an introductory perspective}. Elsevier, v. 31, p. 183-192, 2006.

De Gruijter, J. J.; Brus, D.; Bierkens, M.; Knotters, M. \emph{Sampling for natural resource monitoring}. Berlin: Springer, p. 332, 2006.

Walvoort, D. J. J.; Brus, D. J.; de Gruijter, J. J. An R package for spatial coverage sampling and random sampling from compact geographical strata by k-means. \emph{Computers and Geosciences}. v. 36, p. 1261-1267, 2010.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}

Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
}
\note{
Different results may be returned by different spatial domains (finite vs. infinite). We recomend using a finite spatial domain whenever possible!

Some of the solutions used to build this function were found in the source code of the R-package \pkg{raster}. As such, the author of that package, Robert J. Hijmans \email{r.hijmans@gmail.com}, is entitled \sQuote{contributor} to the R-package \pkg{pedometrics}.
}
\seealso{
\code{\link[raster]{distanceFromPoints}}, \code{\link[raster]{mask}}
}
\examples{
require(sp)
data(meuse.grid)
meuse.grid <- meuse.grid[, 1:2]
d <- .distMat(meuse.grid, exponent = 2)
obj <- sample(rownames(meuse.grid), 155)
a <- objMSSD(obj = obj, dist.mat = d, finite = TRUE)
plot(meuse.grid, asp = 1, pch = 15)
points(meuse.grid[obj, ], pch = 15, col = 20)
}
\keyword{spatial}
% End!