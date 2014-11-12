\name{spSANN}
\alias{spSANN}
\title{
Spatial simulated annealing
}
\description{
Funtion to optimize spatial point patterns using spatial simulated annealing.
}
\usage{
spSANN(points, candidates, x.max, x.min, y.max, y.min, fun, ...,
       iterations = 10000, plotit = TRUE, boundary,
       acceptance = list(initial = 0.99, cooling = iterations / 10),
       stopping = list(max.count = 200), progress = TRUE, verbose = TRUE)
}
\arguments{
\item{points}{
A data.frame or matrix containing the set of point locations to be optimized.
}
\item{candidates, x.max, x.min, y.max, y.min}{
See \code{spJitterFinite} and \sQuote{Details} for more information.
}
\item{fun}{
A \code{\link[base]{function}} used the calculate the criterion that has to be   \strong{minimized} during the optimization.
}
\item{...}{
Arguments passed to the objective function defined with \code{fun}. See \sQuote{Details} for more information.
}
\item{iterations}{
Integer value defining the maximum number of iterations that should be used   for the optimization. See \sQuote{Details} for more information.
}
\item{plotit}{
Logical value for ploting the optimization results. This includes a) the progress of the objective function values and acceptance probabilities, and b) the original points, the perturbed points and the progress of the maximum perturbation in the x and y coordinates. The plots are updated at each 10 iterations. The boundary of the spatial domain is passed using the argument \code{boundary}. Defaults to \code{plotit = TRUE}.
}
\item{boundary}{
A SpatialPolygon defining the boundary of the spatial domain. It is mandatory if \code{plotit = TRUE}
}
\item{acceptance}{
A list with two sub-arguments: \code{initial} and . \code{initial} is a numeric value between 0 and 1 defining the initial acceptance probability. Defaults to \code{initial = 0.99}. \code{cooling} is a numeric value defining the exponential factor by with the acceptance probability decreases at each iteration. Defaults to \code{cooling = iterations / 10}. See See \sQuote{Details} for more information.
}
\item{stopping}{
A list with one sub-argument: \code{max.count}. \code{max.count} is an integer value defining the maximum allowable number of iterations without improvement of the objective function value. This is also known as the freezing criterion. Defaults to \code{max.count = 200}. See \sQuote{Details} for more information.
}
\item{progress}{
Logical value for printing a progress bar. Defaults to \code{progress = TRUE}.
}
\item{verbose}{
Logical value for printing messages about the progress of the optimization.
}
}
\details{

\subsection{Search graph}{
The search graph corresponds to the set of effective candidate locations for a  point being jittered in a given iteration. The size of the search graph, i.e. the maximum distance that a point can be moved around, is correlated with the concept of \strong{temperature}. A larger search graph is equivalent to higher temperatures, which potentially result in more movement or \dQuote{agitation} of the set of points or \dQuote{particles}.

The current version of \code{spSANN} implements a \strong{linear cooling schedule} depending upon the number of iterations to control the size of the search graph. The equations are as follows:

\verb{
x.max.b <- x.max.a - k / iterations * (x.max.a - x.min)
y.max.b <- y.max.a - k / iterations * (y.max.a - y.min)
}

where \code{x.max.a} and \code{y.max.a} are the maximum allowed shift in the x and y coordinates in the current iteration, \code{x.min} and \code{y.min} are the minimum required shift in the x and y coordinates, and \code{x.max.b} and \code{y.max.b} are the maximum allowed shift in the x and y coordinates in the next iteration. \code{iterations} is the total number of iterations and \code{k} is the current iteration.
}
\subsection{Acceptance probability}{
The acceptance probability is the chance of accepting a new system configuration that is worse than the current system configuration. The concept of acceptance probability is related with that of \strong{temperature}. A higher acceptance probability is equivalent to higher temperatures, which potentially result in more movement or \dQuote{agitation} of the set of points or \dQuote{particles}.

Using a low initial acceptance probability turn the spatial simulated annealing into a \emph{greedy} algorithm. It will converge in a shorter time, but the solution found is likely to be a local optimum instead of the global optimum. Using a high initial acceptance probability (\code{>0.8}) usually is the wisest choice.

An \strong{exponential cooling schedule} depending upon the number of iterations is used in the current version of \code{spSANN} to control the acceptance probability. The acceptance probability at each iteration is calculates as follows:

\verb{actual_prob <- acceptance$initial * exp(-k / acceptance$cooling)}

where \code{actual_prob} is the acceptance probability at the \code{k}-th iteration, \code{acceptance$initial} is the initial acceptance probability, and \code{acceptance$cooling} is the exponential cooling factor.
}
\subsection{Starting system configuration}{
Unidimensional objective functions such as \code{\link[pedometrics]{objPairs}} and \code{\link[pedometrics]{objPoints}} are dependent on the starting system configuration by definition. This means that, depending on the parameters passed to \code{spSANN}, many points will likely to stay close to their starting positions. It would be reasonable to use a starting system configuration that is close to the global optimal, but such thing is not feasible.

Increasing the initial acceptance probability does not guarantee the independence from the starting system configuration. The most efficient option in the current version of \code{spSANN} is to start using the entire spatial domain as search graph. This is set using the interval of the x and y coodinates to set \code{x.max} and \code{y.max}. See above.

%An alternative is to start jittering (randomly perturbing) several points at a time and use a cooling schedule to \strong{exponentially} decrease the number of points jittered at each iteration. The cooling schedule implemented in \code{spSANN} is as follows: \verb{new.size <- round(c(old.size - 1) * exp(-k / size.factor) + 1)} where \code{old.size} and \code{new.size} are the number of points jittered in the previous and next iterations, \code{size.factor} is the cooling parameter, and \code{k} is the number of the current iteration. The larger the difference between the starting system configuration and the global optimum, the larger the number of points that need to be jittered in the first iterations. This will usually increase the time spent on the first iterations.
}
\subsection{Number of iterations}{
The number of iterations has a large influence on the performance of the spatial simulated annealing algorithm. The larger the number of possible system configurations, the higher should the number of iterations be.

The number of possible system configurations increases with:
\itemize{
\item a high initial acceptance probability
\item the use of an infinite set of candidate locations
\item the use of a very dense finite set of candidate locations
}
}
}
\value{
A data.frame or matrix with the coordinates of the optimized spatial points with attributes:
\itemize{
\item the objective function values for the starting system configuration and all iterations
\item the running time of the algorithm
}
}
\references{
Brus, D. J.; Heuvelink, G. B. M. Optimization of sample patterns for universal kriging of environmental variables. \emph{Geoderma}. v. 138, p. 86-95, 2007.

Pebesma, E.; Skoien, J. with contributions from Baume, O.; Chorti, A.; Hristopulos, D.T.; Melles, S.J.; Spiliopoulos, G. \emph{intamapInteractive: procedures for automated interpolation - methods only to be used interactively, not included in intamap package.} R package version 1.1-10. 2013. \url{http://CRAN.R-project.org/package=intamapInteractive}

Press, W. H.; Flannery, B. P.; Teukolsky, S. A.; Vetterling, W. T. \emph{Numerical recipes in Fortran 77 - the art of scientific computing}. Cambridge: Press Syndicate of the University of Cambridge, v. 1, p. 973, 1992.

Russel, S.; Norvig, P. \emph{Artificial intelligence: a modern approach}. Upper Saddle River: Prentice Hall, p. 1132, 2010.

Van Groenigen, J.-W. \emph{Constrained optimisation of spatial sampling: a geostatistical approach.} Wageningen: Wageningen University, p. 148, 1999.
  
Vasat, R.; Heuvelink, G. B. M.; Boruvka, L. Sampling design optimization for multivariate soil mapping. \emph{Geoderma}. v. 155, p. 147-153, 2010.
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
\code{\link[intamapInteractive]{ssaOptim}}, \code{\link[pedometrics]{spJitterFinite}}, \code{\link[pedometrics]{objPoints}}, \code{\link[pedometrics]{objPairs}}, \code{\link[base]{do.call}}.
}
\examples{
\dontrun{
data(meuse.grid)
boundary <- meuse.grid
coordinates(boundary) <- ~ x + y
proj4string(boundary) <- CRS("+init=epsg:28992")
gridded(boundary) <- TRUE
boundary <- as(boundary, "SpatialPolygons")
boundary <- gUnionCascaded(boundary)
candidates <- meuse.grid[, 1:2]
coordinates(candidates) <- ~ x + y
proj4string(candidates) <- CRS("+init=epsg:28992")
candidates <- coordinates(candidates)
which_pts <- sample(c(1:dim(candidates)[1]), 50)
points <- candidates[which_pts, ]
x.max <- diff(bbox(boundary)[1, ])
x.min <- 40
y.max <- diff(bbox(boundary)[2, ])
y.min <- 40
iterations <- 10000
acceptance <- list(initial = 0.99, cooling = iterations / 10)
opt <-
  spSANN(points = points, candidates, x.max, x.min, y.max, y.min, 
         #fun = objMSSD, dist.mat = dist_mat, pred.grid = candidates,
         fun = objPoints, lags = 7, cutoff = max_dist/2,
         lags.type = "exponential", criterion = "distribution",
         iterations = iterations, boundary, acceptance = acceptance,
         stopping = list(max.count = 200), progress = TRUE, verbose = TRUE,
         plotit = TRUE)
}
}
\keyword{optimize}
\keyword{spatial}
% End!