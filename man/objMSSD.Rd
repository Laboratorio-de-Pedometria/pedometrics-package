\name{objMSSD}
\alias{objMSSD}
\alias{distMat}
\alias{distToNearestPoint}
\alias{nearestPoint}
\title{
Mean (squared) shortest distance
}
\description{
Function to calculate the distance matrix between all grid cells. Function to calculate the distance to the nearest point. Function to identify the nearest point. Function to calculate the mean (squared) shortest distance between a set of points and all grid cells.
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
Numeric value denoting the power to which the distances are to be raised. Defaults to \code{exponent = 1}.
}
\item{diagonal}{
Numeric value setting the diagonal of the distance matrix. Defaults to \code{diagonal = 0}.
}
\item{points}{
Data frame or matrix containing the projected coordinates (x and y) of a set of points. \code{points} must be a subset of \code{pred.grid}. See \sQuote{Details} for more information.
}
\item{dist.mat}{
A square matrix returned by function \code{distMat}.
}
\item{which.pts}{
A vector of the indexes defining the subset of \code{pred.grid} that corresponds to the set of \code{points}. It indicates the columns of the distance matrix \code{dist.mat} that correspond to the points to which distances should be computed.
}
}
\details{
\subsection{Distances}{
Euclidean distances between points are calculated using the function \code{\link[stats]{dist}}. This computation requires the coordinates to be projected. The user is responsible for making sure that this requirement is attained.
}
\subsection{Mean (squared) shortest distance}{
The function \code{objMSSD} is used in the optimization of spatial points for sampling. In a previous implementation, \code{objMSSD} would calculate the distance matrix at each iteration of the optimization algorithm. This is computationally expensive. Thus, we decided to separate the calculation of the distance matrix, which is done using the function \code{distMat}. The user has to square the distance values to get the mean squared shortest distance -- this is done setting \code{exponent = 2} in \code{distMat}.

Once the distance matrix has been calculated, the algorithm has only to identify the subset of points in the prediction grid. Both \code{distToNearestPoint} and \code{nearestPoint} perform this operation. The calculation of the mean (squared) shortest distance does not require to know which is the nearest point -- we are only interested in knowing the distance to the nearest point. As such, \code{distToNearestPoint} is called internally by \code{objMSSD}.

\code{distToNearestPoint} and \code{nearestPoint} were constructed separately because they are useful for other operations. For instance, \code{distToNearestPoint} can be used to build a map of distances to the nearest point, while \code{nearestPoint} can be used to define geographic strata.
}
\subsection{Utopia and nadir points}{
Knowledge of the utopia and nadir points can help in the construction of multi-objective optimization problems.

\code{objMSSD} is a bi-dimensional criterion because it explicitly takes into account both y and x coordinates. It aims at the spread of points in the geographic space. This is completely different from \code{objPairs} and \code{objPoints} which are uni-dimensional objective functions. They aim at the spread on points in the variogram space. It is more difficult to calculate the utopia and nadir points of a bi-dimensional criterion.

The \strong{utopia} (\eqn{f^{\circ}_{i}}) point of \code{objMSSD} is only known to be larger than zero. The \strong{nadir} (\eqn{f^{max}_{i}}) point is obtained when all points are clustered in one of the \dQuote{corners} of the spatial domain. This cannot be calculated and has to be simulated.

One strategy is to first optimize the set of points using \code{objMSSD} and then create geographic strata. For the multi-objective optimization one would then have to define an unidimensional criterion aiming at matching the optimal solution obtained by minimizing \code{objMSSD}.

One such unidimensional criterion would be the difference between the expected distribution and the observed distribution of points per geographic strata. This criterion would aim at having at least one point per geographic strata. This is similar to what is done when using \code{objPairs} or \code{objPoints} -- there we use lag distance classes.

A second uni-dimensional criterion would be the difference between the expected MSSD and the observed MSSD. This criterion would aim at having the points coinciding with the optimal solution obtained by minimizing \code{objMSSD}. In both cases the utopia point would be exactly zero (\eqn{f^{\circ}_{i} = 0}). The nadir point could be easily calculated for the first, but not for the second.
}
}
\value{
\code{objMSSD} returns a numeric value: the mean (squared) shortest distance between a set of points and all grid cells. \code{distMat} returns a square matrix. \code{distToNearestPoint} and \code{nearestPoint} return a matrix or data.frama.
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
The previous version included the possibility of using an infinite spatial domain. Some of the solutions were found in the source code of the R-package \pkg{raster} from Robert J. Hijmans \email{r.hijmans@gmail.com}. Due to operational issues, the use of an infinite spatial domain is no longer supported.
}
\seealso{
\code{\link[raster]{distanceFromPoints}}, \code{\link[spcosa]{stratify}}
}
\examples{
require(sp)
require(raster)
data(meuse.grid)
meuse.grid <- meuse.grid[, 1:2]
#
# Distance matrix
d <- distMat(meuse.grid, exponent = 2)
obj <- sample(dim(meuse.grid)[1], 15)
pts <- meuse.grid[obj, ]
plot(meuse.grid, asp = 1, pch = 15, col = "gray")
points(pts, pch = 19, col = 20, cex = 0.5)
#
# Means squared shortest distance
#
a <- objMSSD(points = pts, dist.mat = d, pred.grid = meuse.grid)
#
# Distance to the nearest point
#
b <- distToNearestPoint(dist.mat = d, which.pts = obj)
b <- cbind(meuse.grid, b)
coordinates(b) <- ~ x + y
gridded(b) <- TRUE
image(b)
points(pts, pch = 19, cex = 0.5)
#
# Nearest point (geographic strata)
#
e <- nearestPoint(dist.mat = d, which.pts = obj)
e <- cbind(meuse.grid, e)
coordinates(e) <- ~ x + y
gridded(e) <- TRUE
image(e)
points(pts, pch = 19, cex = 0.5)
}
\keyword{spatial}
% End!