\name{points_per_lag}
\alias{points_per_lag}
\alias{pairs_per_lag}
\alias{objPairs}
\alias{objPoints}
\title{
Points and point pairs per lag distance class
}
\description{
Functions to counts the number of points or point pairs per lag distance
class. Functions to compute the deviation of the observed distribution of 
counts from a pre-specified distribution. Functions to maximize the minimum 
number of points or point pairs observed across all lag distance classes.
}
\usage{
points_per_lag(obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf)
pairs_per_lag(obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf)
objPoints(obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf,
          criterion = "minimum", pre.distribution, weights = 1)
objPairs(obj, lags, lags.type = "equidistant", lags.base = 2, cutoff = Inf, 
         criterion = "minimum", pre.distribution, weights = 1)
}
\arguments{
\item{obj}{
Object of class \sQuote{SpatialPoints} with a projected coordinate reference system (CRS).
}
\item{lags}{
Integer value defining the number of lag distance classes. Alternatively, a vector of numeric values defining the lower and upper limits of each lag distance class.
}
\item{lags.type}{
Character value defining the type of lag distance classes. Available options are \code{"equidistant"}, for equidistant lag distance classes, and \code{"exponential"}, for exponentially spaced lag distance classes. Defaults to \code{lags.type = "exponential"}. See \sQuote{Details} for more information.
}
\item{lags.base}{
Numeric value defining the creation of exponentially spaced lag distance classes. Defaults to \code{lags.base = 2}. See \sQuote{Details} for more information.
}
\item{cutoff}{
Numeric value defining the maximum distance value up to which lag distance classes are created. It is used when an integer value is passed to \code{lags}. Defaults to \code{cutoff = Inf}, that is, the maximum distance value up to which lag distance classes are created is equal to the diagonal of the bounding box of \code{obj}.
}
\item{criterion}{
Character value defining the measure that should be returned to describe the energy state of the current system configuration. Available options are \code{"minimum"} and \code{"distribution"}. The first returns the minimum number of points or point pairs observed across all lag distance classes. The second returns the sum of the squared differences between a pre-specified distribution and the observed distribution of counts of points or point pairs per lag distance class. Defaults to \code{objective = "minimum"}. See \sQuote{Details} for more information.
}
\item{pre.distribution}{
A vector of numeric values used to pre-specify the distribution of points or point pairs against which the observed counts of points or point pairs per lag distance class is compared. Used only when \code{criterion = "distribution"}. Defaults to a uniform distribution. See \sQuote{Details} for more information.
}
\item{weights}{
Vector of numeric values defining the importance of each lag distance class when computing the deviation of the observed counts from the pre-specified distribution. The length must match the number of lag classes, and the sum must be equal to one. Used only when \code{criterion = "distribution"}. Defaults to \code{weights = 1}, that is, all lag distance classes have the same weight.
}
}
\details{
\subsection{Distances}{
Euclidean distances between points are calculated using the function \code{\link[stats]{dist}}. This computation requires the coordinates to be projected.
}
\subsection{Distribution}{
Using the default uniform distribution of point pairs within \code{objPairs} means that the number of point pairs per lag distance class is equal to \eqn{n \times (n - 1) / (2 \times lag)}, where \eqn{n} is the total number of points in \code{obj}, and \eqn{lag} is the number of lag distance classes.

Using the default uniform distribution of points within \code{objPoints} means that the number of points per lag distance class is equal to the total number of points in \code{obj}. This is the same as expecting that each point contributes to every lag distance class.

Distributions other that the default options can be easily implemented changing the arguments \code{lags}, \code{lags.type}, \code{lags.base}, \code{pre.distribution} and \code{weights}.
}
\subsection{Type of lags}{
Two types of lag distance classes can be created by default. The first are (\code{lags.type = "equidistant"}) are evenly spaced lags. They are created by simply dividing the distance interval from zero to \code{cutoff} by the required number of lags.

The second type (\code{lags.type = "exponential"}) of lag distance classes is defined by exponential spacings. The spacings are defined by the base \eqn{b} of the exponential expression \eqn{b^n}, where \eqn{n} is the required number of lags. The base is defined using argument \code{lags.base}. The default \code{lags.base = 2} creates lags that are sequentially defined as half of the immediately preceding larger lag. For example, if \code{cutoff = 100} and \code{lags = 4}, the upper limits of the lag distance classes will be

\verb{
> 100 / (2 ^ c(1:4))
[1] 50.00 25.00 12.50  6.25
}
}
\subsection{Criteria}{

The functions \code{objPairs} and \code{objPoints} were designed to be used in spatial simulated annealing to optimize spatial sample configurations. Both of them have two criteria implemented. The first is called using \code{criterion = "distribution"} and is used to minimize the sum of squared differences between a pre-specified distribution and the observed distribution of points or point pairs per lag distance class.

Consider that we aim at having the following distribution of points per lag distance class:

\code{points_per_lag <- c(10, 10, 10, 10, 10)}, 

and that the observed distribution of points per lag distance class is the following:

\code{points_per_lag <- c(1, 2, 5, 10, 10)}.

The objective at each iteration of the optimization will be to match the two distributions. This criterion is of the same type as the one proposed by Warrick and Myers (1987).

The second criterion is called using \code{criterion = "minimum"}. It corresponds to maximizing the minimum number of points or point pairs observed across all lag distance classes. Consider we observe the following distribution of points per lag distance classes in the first iteration:

\code{points_per_lag <- c(1, 2, 5, 10, 10)}.

The objective in the next iteration will be to maximize the number of points in the first lag distance class (\eqn{n = 1}). Consider we then have the following resulting distribution: 

\code{points_per_lag <- c(5, 2, 5, 10, 10)}.

Now the objective will be to maximize the number of points in the second lag distance class (\eqn{n = 2}). The optimization continues until it is not possible to maximize the number of points in any of the lag distance classes, that is, when

\code{points_per_lag <- c(10, 10, 10, 10, 10)}.

This shows that the result of using \code{criterion = "minimum"} is similar to using \code{criterion = "distribution"}. However, the calculations and the resulting sample pattern are significantly different. The running time of the optimization algorithm is a bit longer when using \code{criterion = "distribution"}. This can be important in multi-objective optimization exercises that consider several sample points.

It is important to note that using the first criterion (\code{"distribution"}) in simulated annealing corresponds to a \strong{minimization} problem. On the other hand, using the second criterion (\code{"minimum"}) corresponds to a \strong{maximization} problem. We solve this inconsistency adopting the standard procedure of substituting the criterion that has to be maximized by its inverse. This allows to define both problems as minimization problems.

Both criteria have a similar \strong{utopia} (\eqn{f^{\circ}_{i}}) point. When \code{criterion = "minimum"}, the utopia point is close to zero (\eqn{f^{\circ}_{i} \rightarrow 0}). When \code{criterion = "distribution"}, the utopia point is exactly zero (\eqn{f^{\circ}_{i} = 0}).

The \strong{nadir} (\eqn{f^{max}_{i}}) point depends on a series of elements. For instance, when \code{criterion = "distribution"}, if the desired distribution of point or point pairs per lag distance class is \code{pre.distribution <- c(10, 10, 10, 10, 10)}, the worst case scenario would be to have all points or point pairs in a single lag distance class, that is, \code{obs.distribution <- c(0, 50, 0, 0, 0)}. In this case, the nadir point 
is equal to the sum of squared differences between the two distributions:

\code{sum((c(10, 10, 10, 10, 10) - c(0, 50, 0, 0, 0)) ^ 2) = 2000}.

When \code{objective = "minimum"}, the nadir point of \code{objPoints} is equal to the immediately higher order of magnitude of the maximum number of points that could possibly contribute to a lag distance class. For example, if \code{obj} is composed by 155 points, the nadir point is \eqn{f^{max}_{i} = 1000}.

The nadir point of \code{objPairs} is equal to the immediately higher order of magnitude of the maximum number of point pairs that should idealy contribute to each lag distance class. For example, if there are 155 points and 10 lag distance classes, each lag distance class should idealy have \eqn{155 \times (155 - 1) / (2 \times 10) = 1193.5} contributing point pairs. The nadir point is \eqn{f^{max}_{i} = 10000}. Scaling the results between zero and the immediately higher order of magnitude of the number of points or number of point pairs that should idealy contribute to each lag distance class helps in the construction of multi-objective optimization problems.
}
}
\value{
\code{pairs_per_lag} and \code{points_per_lag} return a data.frame with three columns: a) the lower and b) upper limits of each lag distance class, and c) the number of points or point pairs per lag distance class.

\code{objPairs} and \code{objPoints} return a numeric value depending on the choice of \code{criterion}. If \code{criterion = "distribution"}, the weighted sum of squared differences between the pre-specified and observed distribution of counts of points or point pairs per lag distance class. If \code{criterion = "minimum"}, the inverse of the minimum count of points or point pairs across all lag distance classes.
}
\references{
Bresler, E.; Green, R. E.
   \emph{Soil parameters and sampling scheme for characterizing soil hydraulic 
   properties of a watershed}.
   Honolulu: University of Hawaii at Manoa, p. 42, 1982.

Marler, R. T.; Arora, J. S. 
   Function-transformation methods for multi-objective optimization. 
   \emph{Engineering Optimization}. v. 37, p. 551-570, 2005.

Russo, D. 
   Design of an optimal sampling network for estimating the variogram.
   \emph{Soil Science Society of America Journal}. v. 48, p. 708-716, 1984.

Truong, P. N.; Heuvelink, G. B. M.; Gosling, J. P. 
   Web-based tool for expert elicitation of the variogram.
   \emph{Computers and Geosciences}. v. 51, p. 390-399, 2013.

Warrick, A. W.; Myers, D. E. 
   Optimization of sampling locations for variogram calculations.
   \emph{Water Resources Research}. v. 23, p. 496-500, 1987.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}

Gerard Heuvelink \email{gerard.heuvelink@wur.nl}
}
\note{

\code{pairs_per_lag} and \code{points_per_lag} are called internally by 
\code{objPairs} and \code{objPoints}, respectively. 

The more lags you have, the longer the computations will take.

Use \code{lags = 1} with \code{points_per_lag} and \code{pairs_per_lag} to check
if the functions are working correctly. They should return, respectively, the
total number of points in \code{obj} and the total possible number of point
pairs \eqn{n \times (n - 1) / 2}.
}
\seealso{
\code{\link[stats]{dist}}, \code{\link[sp]{is.projected}}, 
\code{\link[base]{which}}.
}
\examples{
require(sp)
data(meuse)
coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")
tmp <- pairs_per_lag(meuse, lags = 10, lags.type = "exponential")
}
% End!