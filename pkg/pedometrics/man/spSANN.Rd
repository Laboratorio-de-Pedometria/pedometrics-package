\name{spSANN}
\alias{spSANN}
\alias{energyState}
\title{
Spatial simulated annealing
}
\description{
Funtions to optimize spatial point patterns using spatial simulated annealing.
}
\usage{
energyState(fun, obj, ...)
spSANN(obj, fun, iterations = 10000, spJitter.ctrl = spJitter.control(),
       max.count = 200, initial.prob = 0.2, size = 1, size.factor = 10,
       cooling.factor = iterations / 10,
       progress = TRUE, plotit = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{obj}{
  Object of class SpatialPoints containing the point locations to be optimized.
}
  \item{fun}{
  A \code{\link[base]{function}} used the calculate the criterion that has to be
  \strong{minimized} during the optimization.
}
  \item{...}{
  Arguments passed to the objective function defined with \code{fun}. See 
  \sQuote{Details} for more information.
}
  \item{iterations}{
  Integer value defining the maximum number of iterations that should be used 
  for the optimization. See \sQuote{Details} for more information.
}
  \item{spJitter.ctrl}{
  Controls for the \code{\link[pedometrics]{spJitter}} function.
}
  \item{max.count}{
  Integer value defining the maximum allowable number of iterations without
  improvement of the objective function value. This is also known as the 
  freezing criterion. Defaults to \code{max.count = 200}. See \sQuote{Details} 
  for more information.
}
  \item{initial.prob}{
  Numeric value between 0 and 1 defining the initial acceptance probability.
  Defaults to \code{initial.prob = 0.2}. See \sQuote{Details} for more 
  information.
}
  \item{size}{
  Integer value defining the number of points that should be perturbed at each
  iteration. Defaults to \code{size = 1}. See \sQuote{Details} for more 
  information.
}
  \item{size.factor}{
  Numeric value defining the factor by which the number of points that should be
  perturbed at each iteration is decreased. Useful only when \code{size} is 
  larger than 1. See \sQuote{Details} for more information.
}
  \item{cooling.factor}{
  Numeric value defining the factor by with the acceptance probability decreases
  at each iteration. Defaults to \code{cooling.factor = iterations / 10}. See
  \sQuote{Details} for more information.
}
  \item{progress}{
  Logical value for printing a progress bar. Defaults to \code{progress = TRUE}.
}
  \item{plotit}{
  Logical value for ploting the optimization results, including a) the progress 
  of the objective function values  and b) the perturbed point pattern. The
  plots are updated at each 10 iterations. The plot with the perturbed point
  pattern can include the object of class SpatialPolygons passed to the
  argument \code{where} in \code{spJitter.ctrl}. This is possible even if 
  a SpatialPoints object is passed to the argument \code{candidates} in
  \code{spJitter.ctrl} to be used as candidate locations for the perturbed 
  points. Defaults to \code{plotit = TRUE}.
}
  \item{verbose}{
  Logical value for printing messages about the progress of the optimization.
}
}
\details{
Details will be added soon.
}
\value{
A list with two components: a) the optimized spatial point pattern and b) the
objective function values for the starting pattern and all iterations.
}
\references{
Brus, D. J.; Heuvelink, G. B. M. 
  Optimization of sample patterns for universal kriging of environmental variables.
  \emph{Geoderma}. v. 138, p. 86-95, 2007.

Pebesma, E.; Skoien, J. with contributions from Baume, O.; Chorti, A.; 
  Hristopulos, D.T.; Melles, S.J.; Spiliopoulos, G.
  \emph{intamapInteractive: procedures for automated interpolation - methods 
  only to be used interactively, not included in intamap package.}
  R package version 1.1-10. 2013.
  \url{http://CRAN.R-project.org/package=intamapInteractive}

Press, W. H.; Flannery, B. P.; Teukolsky, S. A.; Vetterling, W. T.
  \emph{Numerical recipes in Fortran 77 - the art of scientific computing}.
  Cambridge: Press Syndicate of the University of Cambridge, v. 1, p. 973, 1992.

Russel, S.; Norvig, P.
  \emph{Artificial intelligence: a modern approach}.
  Upper Saddle River: Prentice Hall, p. 1132, 2010.

van Groenigen, J.-W.
  \emph{Constrained optimisation of spatial sampling: a geostatistical approach.}
  Wageningen: Wageningen University, p. 148, 1999.
  
Vasat, R.; Heuvelink, G. B. M.; Boruvka, L.
  Sampling design optimization for multivariate soil mapping. 
  \emph{Geoderma}. v. 155, p. 147-153, 2010.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}

Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
}
\note{
Some of the solutions used to build this function were found in the source code 
of the R-package \pkg{intamapInteractive}. As such, the authors of that package, 
Edzer Pebesma \email{edzer.pebesma@uni-muenster.de} and Jon Skoien 
\email{jon.skoien@gmail.com}, are entitled \sQuote{contributors} to the 
R-package \pkg{pedometrics}.
}
\seealso{
\code{\link[intamapInteractive]{ssaOptim}}, \code{\link[pedometrics]{spJitter}},
\code{\link[pedometrics]{objPoints}}, \code{\link[pedometrics]{objPairs}},
\code{\link[base]{do.call}}.
}
\examples{
\dontrun{
# required packages
require(sp)
require(rgeos)
## prepare data
data(meuse)
meuse <- meuse[, c("x", "y")]
coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")
data(meuse.grid)
coordinates(meuse.grid) <- ~ x + y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) <- TRUE
bound <- as(meuse.grid, "SpatialPolygons")
bound <- gUnionCascaded(bound)
candidates <- meuse.grid
gridded(candidates) <- FALSE
candidates <- as(candidates, "SpatialPoints")
## settings
spJitter.ctrl <- 
  spJitter.control(where = bound, candidates = candidates,
                   x.coord = list(min = 40, max = NULL, factor = 0.5),
                   y.coord = list(min = 40, max = NULL, factor = 0.5))
cutoff <- (c(dist(t(bbox(meuse)))) / 2)
lags <- 7
iterations <- 100
set.seed(123)
##  run optimization
obj <- spSANN(meuse, iterations = iterations, size = 5, initial.prob = 0.5,
              fun = objPoints, lags = lags, cutoff = cutoff, 
              objective = "distri", lag.type = "log",
              spJitter.ctrl = spJitter.ctrl, verbose = TRUE, plotit = TRUE)
## see results
points_per_lag(meuse, lags = lags, cutoff = cutoff, lag.type = "log")
points_per_lag(obj[[1]], lags = lags, cutoff = cutoff, lag.type = "log")
}
}
\keyword{optimize}
\keyword{spatial}
% End!