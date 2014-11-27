\name{spJitter}
%\alias{spJitter}
%\alias{spJitter.control}
\alias{spJitterFinite}
\title{
Perturbation of spatial points
}
\description{
This function perturbs the coordinates of spatial points (also known as \sQuote{jittering}).
}
\usage{
spJitterFinite(points, candidates, x.max, x.min, y.max, y.min, which.pts)
}
\arguments{
\item{points}{
A data.frame or matrix with the points to be perturbed. \code{points} must be a subset of \code{candidates}. See \sQuote{Details} for more information.
}
\item{candidates}{
A data.frame or matrix with the candidate locations for the perturbed points. \code{points} must be a subset of \code{candidates}. See \sQuote{Details} for more information.
}
\item{x.min, x.max,
      y.min, y.max}{
The minimum and maximum quantity of random noise to be added to the x and y coordinates. The minimum quantity should be equal to, at least, the minimum distance between two neighboring candidate locations. The units are the same as of the coordinates. See \sQuote{Details} for more information.
}
\item{which.pts}{
Vector of integer values defining which point should be perturbed. The current version accepts only one point to be perturbed at a time. See \sQuote{Details} for more information.
}

%\item{where}{An object of class SpatialPolygons defining the spatial domain to which the perturbed points should be constrained. Used only when \code{finite = FALSE}. See \sQuote{Details} for more information.}
%\item{finite}{Logical value defining if the spatial domain is finite or infinite. This is a mandatory argument. See \sQuote{Details} for more information.}
%\item{x.coord}{List with two sub-arguments defining how the x coordinate should be perturbed. \code{min} and \code{max} define the minimum and maximum quantity of random noise to be added to the x coordinate. If \code{finite = TRUE}, then \code{min} should be equal to, at least, the minimum distance between two neighboring candidate locations. If \code{finite = FALSE}, then \code{min} should be equal to, at least, the value passed to the argument \code{zero}. This is a mandatory argument. See \sQuote{Details} for more information.}
%\item{y.coord}{The same as for \code{x.coord}.}
%\item{zero}{Numeric value defining the distance less than or equal to which two points are considered to have zero distance. See more information at \code{\link[sp]{zerodist}}. Used only when \code{finite = FALSE}. Defaults to \code{zero = 1}.}
%\item{iterations}{Integer value defining the maximum number of iterations that should be used when constraining the perturbed points to the spatial domain defined by the argument \code{where}. Defaults to \code{iterations = 10000}. Used only when \code{finite = FALSE}. See \sQuote{Details} for more information.}
%\item{verbose}{Logical for printing details about the success of the algorithm when constraining the perturbed points to the spatial domain defined by the argument \code{where}. Used only when \code{finite = FALSE}.}
%\item{size}{Integer value defining the number of points that should be perturbed at each iteration of the simulated annealing exercise (\code{spSANN}). Defaults to \code{size = 1}. See \code{spSANN} for more information.}
%\item{size.factor}{Numeric value defining the factor by which the number of points that should be perturbed at each iteration of the simulated annealing exercise (\code{spSANN}) is decreased. Used only when \code{size} is larger than 1. See \code{spSANN} for more information.}

}
\details{
This function perturbs the coordinates of spatial points adding random noise, a process also known as \sQuote{jittering}. There are two ways of jittering the coordinates. They differ on how the the set of candidate locations is defined.

\subsection{Finite set of candidate locations}{

The first method uses a finite set of candidate locations for the perturbed points. This method usually is the fastest because it does not require the use of complex routines to check if the perturbed point falls inside the spatial domain. Since the candidate locations is a finite set, any perturbed point will inexorably fall inside the spatial domain. This is a very important feature in optimization exercises with complex objective functions such as simulated annealing when repetitive perturbation is required.

The arguments \code{x.min}, \code{y.min}, \code{x.max}, and \code{y.max} are used to define a rectangular window containing the set of potential candidate locations for the point defined with the argument \code{which.pts}. We then eliminate the candidate locations that already are cointaned in the set of points being perturbed to avoid having duplicated points. The new location is then randomly sampled from the set of effective candidate locations. The current implementation does not enable to define the direction of the perturbation, nor to perturb more than one point at a time.
}
\subsection{Infinite set of candidate locations}{

The current version does not accept using an infinite set of candidate locations.

%The second method can be much slower than the first depending on the number of points, on the shape of the area and on how the other arguments are set. This method does not use a finite set of candidate locations. Instead, the number of candidate locations is infinite. Its domain can be defined using the argument \code{where}. The reason for the larger amount of time demanded is that the method has two internal steps to 1) check if the perturbed point falls inside the spatial domain, and b) check if two of more points have coincident coordinates (set using argument \code{zero}). Using an infinite set of candidate locations will usually allow obtaining better results in optimization exercises such as spatial simulated annealing. However, the amount of time may be prohibitive depending on the complexity of the problem.

%The sub-argument \code{max} in both arguments \code{x.coord} and \code{y.coord} defines the lower and upper limits of a uniform distribution (\code{runif(n, min = -max, max = max)}). The quantity of noise added to the coordinates of the point being perturbed is sampled from this uniform distribution. By default, the maximum quantity of random noise added to the x and y coordinates is, respectively, equal to half the width and height of the bounding box of the set of points. This is equivalent to a vector \strong{h} of length equal to half the diagonal of the bounding box. Therefore, a larger jittering is allowed in the longer coordinate axis (x or y).

%The direction of the perturbation is defined by the sign of the values sampled from the uniform distribution. This means that the perturbation can assume any direction from 0 to 360 degrees. By contrast, the function \code{\link[geoR]{jitter2d}} in the R-package \pkg{geoR} samples from a uniform distribution a value for the length of the vector \strong{h} and a value for the direction of the perturbation.

%\code{spJitter} allows to set the minimum quantity of random noise added to a coordinate with the sub-argument \code{min}. The absolute difference between the original coordinate value and the jittered coordinate value is used to evaluate this constraint. If the constraint is not met, \code{min} receives the sign of the value sample from the uniform distribution and is added to the original coordinate value. This does not guarantee that the perturbation will be in the same direction, but in the same quadrant.

%When a spatial domain is defined, \code{spJitter} evaluates if the perturbed points fall inside it using the function \code{\link[rgeos]{gContains}} from the R-package \pkg{rgeos}. All points falling outside the spatial domain are identified and have their original coordinates jittered one again. Every new coordinate falling inside the spatial domain is accepted. Every point falling outside the spatial domain has its coordinates jittered till it falls inside the spatial domain. The number of iterations necessary to meet this constraint depends on the complexity of the shape of the spatial domain. \code{spJitter} tries to speed up the process by linearly decreasing the maximum quantity of noise added to the coordinates at each iteration. If the number of iterations was not enough to guarantee all points inside the spatial domain, \code{spJitter} returns the jittered SpatialPoints with a warning message informing how many points do not meet the constraint.
}
}
\value{
A data.frame or matrix with jittered coordinates.
}
\references{
Edzer Pebesma, Jon Skoien with contributions from Olivier Baume, A. Chorti, D.T. Hristopulos, S.J. Melles and G. Spiliopoulos (2013). \emph{intamapInteractive: procedures for automated interpolation - methods only to be used interactively, not included in intamap package.} R package version 1.1-10. \url{http://CRAN.R-project.org/package=intamapInteractive}

Van Groenigen, J.-W. \emph{Constrained optimization of spatial sampling: a geostatistical approach.} Wageningen: Wageningen University, p. 148, 1999.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}

Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
}
\note{
This function is under construction! It has gone under sensible changes since the last version. The current version does not accept using an infinite set of candidate locations. The current implementation does not enable to perturb more than one point at a time.

Some of the solutions used to build this function were found in the source code of the R-package \strong{intamapInteractive}. As such, the authors of that package, Edzer Pebesma <\email{edzer.pebesma@uni-muenster.de}> and Jon Skoien <\email{jon.skoien@gmail.com}>, are entitled \sQuote{contributors} to the R-package \pkg{pedometrics}.
}
\seealso{
\code{ssaOptim}, \code{\link[sp]{zerodist}}, \code{\link[base]{jitter}}, \code{\link[geoR]{jitter2d}}, \code{\link[rgeos]{gContains}}
}
\examples{
require(sp)
data(meuse.grid)
meuse.grid <- meuse.grid[, 1:2]
pts1 <- sample(c(1:dim(meuse.grid)[1]), 155)
pts2 <- meuse.grid[pts1, ]
pts3 <- spJitterFinite(points = pts2, candidates = meuse.grid, which.pts = 10,
                       x.min = 40, x.max = 100, y.min = 40, y.max = 100)
plot(meuse.grid, asp = 1, pch = 15, col = "gray")
points(pts2, col = "red", cex = 0.5)
points(pts3, pch = 19, col = "blue", cex = 0.5)
}
\keyword{iteration}
\keyword{spatial}
% End!
