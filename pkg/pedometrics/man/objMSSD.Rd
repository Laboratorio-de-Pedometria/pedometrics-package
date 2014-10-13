\name{objMSSD}
\alias{objMSSD}
\title{
Mean (squared) shortest distance
}
\description{
This function calculates the mean (squared) shortest distance between a set of points and all grid cells.
}
\usage{
objMSSD(obj, pred.grid, mask, dist.mat, finite)
}
\arguments{
\item{obj}{
An object of class SpatialPoints*. Used only when \code{finite = FALSE}. A vector of integer values indicating the columns of the distance matrix \code{dist.mat} that correspond to the points to which distances should be computed. Use only when \code{finite = TRUE}.
}
\item{pred.grid}{
Object of class Raster. Use with \code{finite = FALSE}.
}
\item{mask}{
Object of class SpatialPolygons*. Use with \code{finite = FALSE}.
}
\item{dist.mat}{
Distance matrix of the population of all grid locations in the spatial domain. Use with \code{finite = TRUE}.
}
\item{finite}{
Mandatory character value defining if the spatial domain is finite (\code{TRUE}) or infinite (\code{FALSE}).
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
# require(raster)
# require(sp)
# data(meuse.grid)
# meuse.grid <- meuse.grid[, 1:2]
# n <- 100
# set.seed(123)
# pts_id <- sample(c(1:dim(meuse.grid)[1]), n)
# dist_mat <- .distMat(meuse.grid, exponent = 2, diagonal = 0)
# min_dist <- .distanceToNearestPoint(dist_mat, points = pts_id)
# near_pts <- .nearestPoint(dist_mat, points = pts_id)
# mean(min_dist, na.rm = TRUE)
# meuse.grid$min.dist <- min_dist
# coordinates(meuse.grid) <- ~ x + y
# gridded(meuse.grid) <- TRUE
# x <- raster(meuse.grid)
# image(x, asp = 1)
# points(meuse.grid[pts_id, ], pch = 20, cex = 0.8)
}
\keyword{spatial}
% End!