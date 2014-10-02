\name{points_per_lag}
\alias{points_per_lag}
\alias{pairs_per_lag}
\alias{objPairs}
\alias{objPoints}
\title{
Points and point pairs per lag distance class
}
\description{
Functions to counts the number of points and point pairs per lag distance
class. Functions to compute the deviation of the observed distribution of 
counts from a pre-specified distribution.
}
\usage{
points_per_lag(obj, lags = length(obj), cutoff = Inf, factor)
pairs_per_lag(obj, lags = length(obj), cutoff = Inf, factor)
objPoints(obj, lags = length(obj), distribution, cutoff = Inf, factor, weights = 1)
objPairs(obj, lags = length(obj), distribution, cutoff = Inf, factor, weights = 1)
}
\arguments{
  \item{obj}{
  Object of class \sQuote{SpatialPoints*} with projected coordinates.
}
  \item{lags}{
  Integer value defining the number of equidistant lag distance classes.
  Alternatively, a vector of numeric values defining the lower and upper limits
  of each lag distance class. Defaults to \code{lags = length(obj)}, i.e.\~the 
  number of lag distance classes is equal to the number of points in \code{obj}.
}
  \item{cutoff}{
  Numeric value defining the maximum distance value up to which lag distance
  classes are created. It is used when an integer value is passed to the argument
  \code{lags}. Defaults to \code{cutoff = Inf}, i.e.\~the maximum distance value 
  up to which lag distance classes are created is equal to the diagonal of the 
  bounding box of \code{obj}.
}
  \item{factor}{
  Numeric value used as a multiplier of the argument \code{cutoff} when 
  \code{cutoff = Inf}. If \code{factor = 0.5}, then the maximum distance value
  up to which lag distance classes are created is equal to half the diagonal of
  the bounding box of \code{obj}. Defaults to \code{factor = 1}.
}
  \item{distribution}{
  A vector of numeric values defining a distribution of points or point pairs 
  against which the observed counts of points or point pairs per lag distance 
  class is compared. Defaults to a uniform distribution. See \sQuote{Details} for
  more information.
}
  \item{weights}{
  Vector of numeric values defining the importance of each lag distance class
  when computing the deviation of the observed counts from the pre-specified 
  distribution. The length must match the number of lag classes, and the sum
  must be equal to one. Defaults to \code{weights = 1}, i.e.~all lag distance
  classes have the same weight.
}
}
\details{
Euclidean distances between points are calculated using the function 
\code{\link[stats]{dist}}. This computation requires the coordinates to be 
projected. The more lags you have, the longer the computations will take.

\code{pairs_per_lag} and \code{points_per_lag} are called internally by 
\code{objPairs} and \code{objPoints}, respectivelly.

Pre-specifying a uniform distribution of point pairs within \code{objPairs} 
means that the number of point pairs per lag distance class is
\eqn{n \times (n - 1) / (2 \times lag)}, where \eqn{n} is the total number of 
points in \code{obj}, and \eqn{lag} is the number of lag distance classes.

Pre-specifying a uniform distribution of points within \code{objPoints} means
that the number of points per lag distance class is equal to the total number of
points in \code{obj}.

Distributions other that the uniform can be easily implemented playing with the
arguments \code{lags}, \code{distribution} and \code{weights}.
}
\value{
\code{pairs_per_lag} and \code{points_per_lag} return a data.frame with three 
columns: a) the lower and b) upper limits of each lag distance class, and c) 
the number of points or point pairs per lag distance class.

\code{objPairs} and \code{objPoints} return a numeric value, the weigthed sum of
squared differences between the pre-specified and observed distribution of 
counts of points or point pairs per lag distance class.
}
\references{
Bresler, E. \& Green, R. E.
   \emph{Soil parameters and sampling scheme for characterizing soil hydraulic 
   properties of a watershed}.
   Honolulu: University of Hawaii at Manoa, p. 42, 1982.

Russo, D. 
   Design of an optimal sampling network for estimating the variogram.
   \emph{Soil Science Society of America Journal}. v. 48, p. 708-716, 1984.

Warrick, A. W. \& Myers, D. E. 
   Optimization of sampling locations for variogram calculations.
   \emph{Water Resources Research}, v. 23, p. 496–500, 1987.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\note{
Use \code{lags = 1} with \code{points_per_lag} and \code{pairs_per_lag} to check
if the functions are working correctly. They should return, respectivelly, the
total number of points in \code{obj} and the total possible number of point
pairs \eqn{n \times (n - 1) / 2}.
}
\seealso{
\code{\link[stats]{dist}}, \code{\link[sp]{is.projected}}, 
\code{\link[base]{which}}.
}
\examples{
data(meuse)
coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")
tmp <- pairs_per_lag(meuse)
}
% End!