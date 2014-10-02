\name{spJitter}
\alias{spJitter}
\title{
Random perturbation of spatial points
}
\description{
This function perturbs the coordinates of spatial points adding random noise. 
This is also known as \sQuote{jittering}.
}
\usage{
spJitter(obj, x.coord = list(min = 1, max = NULL, factor = 0.5),
         y.coord = list(min = 1, max = NULL, factor = 0.5), zero = 1, 
         which = "all", where = NULL, iterations = 100, verbose = TRUE)
}
\arguments{
  \item{obj}{
  Object of class SpatialPoints with the point(s) to be perturbed.
}
  \item{x.coord}{
  List with three subarguments defining how the x coordinate should be perturbed. 
  \code{min} and \code{max} define the minimum and maximum quantity of random
  noise to be added to the x coordinate. \code{factor} is used when
  \code{max} is not set. It is a multiplier used to define the fraction of the 
  total length of the x axis that should be used as \code{max}. The default is 
  \code{fraction = 0.5}, i.e.~half the length of the x axis. See
  \sQuote{Details} for more information.
}
  \item{y.coord}{
  The same as for \code{x.coord}.
}
  \item{zero}{
  Numeric value defining the distance less than or equal to which two points are 
  considered to have zero distance. Units are those of the coordinates for
  projected data or unknown projection, or km if coordinates are defined to be
  longitute/latitude. See more information at \code{\link[sp]{zerodist}}.
  Defaults to \code{zero = 1}.
}
  \item{which}{
  Vector of integer values or a character value defining which points should 
  be perturbed. Defaults to \code{which = "all"}, that is, all points are
  perturbed.
}
  \item{where}{
  An object of class SpatialPolygons* defining the spatial domain to which the
  perturbed points should be constrained. Defaults to \code{where = NULL}.
  See \sQuote{Details} for more information.
}
  \item{iterations}{
  Integer value defining the maximum number of iterations that should be used 
  when constraining the perturbed points to the spatial domain defined by the
  argument \code{where}. The perturbation is not constrained if
  \code{where = "inf"}. Defaults to \code{iterations = 100}. See 
  \sQuote{Details} for more information.
}
  \item{verbose}{
  Logical for printing details about the (lack of) success of the algorithm when
  constraining the perturbed points to the spatial domain defined by the
  argument \code{where}.
}
}
\details{
This function perturbs the coordinates of spatial points adding random noise, a 
process also known as \sQuote{jittering}.

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
\value{
An object of class SpatialPoints with jittered coordinates.
}
\references{
Edzer Pebesma, Jon Skoien with contributions from Olivier Baume, A. Chorti, 
  D.T. Hristopulos, S.J. Melles and G. Spiliopoulos (2013).
  \emph{intamapInteractive: procedures for automated interpolation - methods 
  only to be used interactively, not included in intamap package.}
  R package version 1.1-10.
  \url{http://CRAN.R-project.org/package=intamapInteractive}

van Groenigen, J.-W.
  \emph{Constrained optimisation of spatial sampling: a geostatistical approach.}
  Wageningen: Wageningen University, p. 148, 1999.
}
\author{
Alessandro Samuel-Rosa <\email{alessandrosamuelrosa@gmail.com}>
}
\note{
Some of the solutions used to build this function were found in the source code 
of the R-package \pkg{intamapInteractive}. As such, the authors of that package, 
Edzer Pebesma <\email{edzer.pebesma@uni-muenster.de}> and Jon Skoien 
<\email{jon.skoien@gmail.com}>, are entitled \sQuote{contributors} to the 
R-package \pkg{pedometrics}.
}
\seealso{
\code{\link[intamapInteractive]{ssaOptim}}, \code{\link[sp]{zerodist}},
\code{\link[base]{jitter}}, \code{\link[geoR]{jitter2d}}, 
\code{\link[rgeos]{gContains}}
}
\examples{
require(sp)
data(meuse)
obj <- meuse[, c("x", "y")]
coordinates(obj) <- ~ x + y
obj1 <- spJitter(obj)
xlim <- c(min(bbox(obj)["x", "min"], bbox(obj1)["x", "min"]),
         max(bbox(obj)["x", "max"], bbox(obj1)["x", "max"]))
ylim <- c(min(bbox(obj)["y", "min"], bbox(obj1)["y", "min"]),
         max(bbox(obj)["y", "max"], bbox(obj1)["y", "max"]))
plot(obj, pch = 20, col = "blue", cex = 0.3, xlim = xlim, ylim = ylim)
points(obj1, pch = 20, col = "red", cex = 0.3)
}
\keyword{iteration}
\keyword{spatial}
% End!