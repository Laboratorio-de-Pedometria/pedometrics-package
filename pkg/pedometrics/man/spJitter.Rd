\name{spJitter}
\alias{spJitter}
\alias{spJitter.control}
\title{
Perturbation of spatial points
}
\description{
This function perturbs the coordinates of spatial points (also known as
\sQuote{jittering}).
}
\usage{
spJitter(points, candidates, where, which, finite,
         x.coord = list(min = NULL, max = NULL), 
         y.coord = list(min = NULL, max = NULL), 
         zero = 1, iterations = 10000, verbose = TRUE)
spJitter.control(x.coord = list(min = 1, max = NULL, factor = 0.5),
                 y.coord = list(min = 1, max = NULL, factor = 0.5),
                 candidates = NULL, zero = 1, where = NULL, 
                 iterations = 10000, verbose = FALSE)
}
\arguments{
\item{points}{
Object containing the points to be perturbed. An object of class SpatialPoints, if \code{finite = FALSE}, or a vector of indexes, if \code{finite = TRUE}. The indexes must correspond to a subset of the rownames of the \code{data.frame} passed to the argument \code{candidates}. This argument is mandatory. See \sQuote{Details} for more information.
}
\item{candidates}{
Data frame with the candidate locations for the perturbed points. Columns must be named \code{x} and \code{y}. Used only when \code{finite = TRUE}. See \sQuote{Details} for more information.
}
\item{where}{
An object of class SpatialPolygons defining the spatial domain to which the perturbed points should be constrained. Used only when \code{finite = FALSE}. See \sQuote{Details} for more information.
}
\item{which}{
Vector of integer values or a character value \code{which = "all"} defining which points should be perturbed. See \sQuote{Details} for more information.
}
\item{finite}{
Logical value defining if the spatial domain is finite or infinite. This is a mandatory argument. See \sQuote{Details} for more information.
}
\item{x.coord}{
List with two subarguments defining how the x coordinate should be perturbed. \code{min} and \code{max} define the minimum and maximum quantity of random noise to be added to the x coordinate. If \code{finite = TRUE}, then \code{min} should be equal to, at least, the minimum distance between two neighbouring candidate locations. If \code{finite = FALSE}, then \code{min} should be equal to, at least, the value passed to the argument \code{zero}. This is a mandatory argument. See \sQuote{Details} for more information.
}
\item{y.coord}{
The same as for \code{x.coord}.
}
\item{zero}{
Numeric value defining the distance less than or equal to which two points are considered to have zero distance. See more information at \code{\link[sp]{zerodist}}. Used only when \code{finite = FALSE}. Defaults to \code{zero = 1}.
}
\item{iterations}{
Integer value defining the maximum number of iterations that should be used when constraining the perturbed points to the spatial domain defined by the argument \code{where}. Defaults to \code{iterations = 10000}. Used only when \code{finite = FALSE}. See \sQuote{Details} for more information.
}
\item{verbose}{
Logical for printing details about the success of the algorithm when constraining the perturbed points to the spatial domain defined by the argument \code{where}. Used only when \code{finite = FALSE}.
}
}
\details{
This function perturbs the coordinates of spatial points adding random noise, a 
process also known as \sQuote{jittering}. There are two ways of jittering the 
coordinates. They differ on how the the set of candidate locations is defined.

\subsection{Finite set of candidate locations}{

The first method uses a finite set of candidate locations for the perturbed
points. This method is the fastest because it does not require to check if the 
perturbed point falls inside the spatial domain. Since the candidate locations 
is a finite set, any perturbed point will inexorably fall inside the spatial 
domain. This is a very important feature when \code{spJitter} is used in 
optimization exercises such as simulated annealing when repetitive perturbation 
is required.

The subarguments \code{min} and \code{max} in both arguments \code{x.coord} and
\code{y.coord} are used to define a rectangular window containing the set of 
candidate locations. The new location is randomly sampled from the set of
candidate locations. The current implementation does not enable to define the
direction of the perturbation, but this could possibly be done by defining 
quadrants.
}

\subsection{Infinite set of candidate locations}{

The second method can be much slower than the first depending on the number of
points, on the shape of the area and on how the other arguments are set. This 
method does not use a finite set of candidate locations. Intead, the number of
candidate locations is infinite. Its domain can be defined using the argument
\code{where}. The reason for the larger amount of time demanded is that the
method has two internal steps to 1) check if the perturbed point falls inside the
spatial domain, and b) check if two of more points have coincident coordinates
(set using argument \code{zero}). Using an infinite set of condidate locations
will usually allow obtaining better results in optimization exercises such as 
spatial simulated annealing. However, the amount of time may be prohibitive 
depending on the complexity of the problem.

The subargument \code{max} in both arguments \code{x.coord} and \code{y.coord} 
defines the lower and upper limits of a uniform distribution 
(\code{runif(n, min = -max, max = max)}). The quantity of noise added to the
coordinates of the point being perturbed is sampled from this uniform 
distribution. By default, the maximum quantity of random noise added to the x 
and y coordinates is, respectively, equal to half the width and height of the 
bounding box of the set of points. This is equivalent to a vector \strong{h} of 
length equal to half the diagonal of the bounding box. Therefore, a larger 
jittering is allowed in the longer coordinate axis (x or y).

The direction of the perturbation is defined by the sign of the values sampled
from the uniform distribution. This means that the perturbation can assume any 
direction from 0 to 360 degrees. By contrast, the function 
\code{\link[geoR]{jitter2d}} in the R-package \pkg{geoR} samples from a
uniform distribution a value for the length of the vector \strong{h} and a value
for the direction of the perturbation.

\code{spJitter} allows to set the minimum quantity of random noise added to a
coordinate with the subargument \code{min}. The absolute difference between the
original coordinate value and the jittered coordinate value is used to evaluate
this constraint. If the constraint is not met, \code{min} receives the sign of
the value sample from the uniform distribution and is added to the original 
coordinate value. This does not guarantee that the perturbation will be in the
same direction, but in the same quadrant.

When a spatial domain is defined, \code{spJitter} evaluates if the perturbed 
points fall inside it using the function \code{\link[rgeos]{gContains}} from the
R-package \pkg{rgeos}. All points falling outside the spatial domain are 
identified and have their original coodinates jittered one again. Every new 
coordinate falling inside the spatial domain is accepted. Every point falling
outside the spatial domain has its coordinates jittered till it falls inside the
spatial domain. The number of iterations necessary to meet this constraint 
depends on the complexity of the shape of the spatial domain. \code{spJitter} 
tries to speed up the process by linearly decreasing the maximum quantity of
noise added to the coordinates at each iteration. If the number of iterations 
was not enough to guarantee all points inside the spatial domain, \code{spJitter} 
returns the jittered SpatialPoints with a warning message informing how many
points do not meet the constraint.
}
}
\value{
An object of class SpatialPoints with jittered coordinates.
}
\references{
Edzer Pebesma, Jon Skoien with contributions from Olivier Baume, A. Chorti, D.T. Hristopulos, S.J. Melles and G. Spiliopoulos (2013). \emph{intamapInteractive: procedures for automated interpolation - methods only to be used interactively, not included in intamap package.} R package version 1.1-10. \url{http://CRAN.R-project.org/package=intamapInteractive}

Van Groenigen, J.-W. \emph{Constrained optimisation of spatial sampling: a geostatistical approach.} Wageningen: Wageningen University, p. 148, 1999.
}
\author{
Alessandro Samuel-Rosa <\email{alessandrosamuelrosa@gmail.com}>

Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
}
\note{
This function is under construction!

Some of the solutions used to build this function were found in the source code of the R-package \pkg{intamapInteractive}. As such, the authors of that package, Edzer Pebesma <\email{edzer.pebesma@uni-muenster.de}> and Jon Skoien <\email{jon.skoien@gmail.com}>, are entitled \sQuote{contributors} to the R-package \pkg{pedometrics}.
}
\seealso{
\code{\link[intamapInteractive]{ssaOptim}}, \code{\link[sp]{zerodist}},
\code{\link[base]{jitter}}, \code{\link[geoR]{jitter2d}}, 
\code{\link[rgeos]{gContains}}
}
\examples{
require(sp)
data(meuse)
meuse <- meuse[, c("x", "y")]
coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")
obj1 <- spJitter(meuse)
xlim <- c(min(bbox(meuse)["x", "min"], bbox(obj1)["x", "min"]),
         max(bbox(meuse)["x", "max"], bbox(obj1)["x", "max"]))
ylim <- c(min(bbox(meuse)["y", "min"], bbox(obj1)["y", "min"]),
         max(bbox(meuse)["y", "max"], bbox(obj1)["y", "max"]))
plot(meuse, pch = 20, col = "blue", cex = 0.3, xlim = xlim, ylim = ylim)
points(obj1, pch = 20, col = "red", cex = 0.3)
}
\keyword{iteration}
\keyword{spatial}
% End!