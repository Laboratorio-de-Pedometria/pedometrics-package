% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vgmICP.R
\name{variogramGuess}
\alias{variogramGuess}
\alias{vgmICP}
\title{Guess the parameters of a spatial covariance function}
\usage{
variogramGuess(
  z,
  coords,
  lags,
  cutoff = 0.5,
  method = "a",
  min.npairs = 30,
  model = "matern",
  nu = 0.5,
  estimator = "qn",
  plotit = FALSE
)

vgmICP(
  z,
  coords,
  lags,
  cutoff = 0.5,
  method = "a",
  min.npairs = 30,
  model = "matern",
  nu = 0.5,
  estimator = "qn",
  plotit = FALSE
)
}
\arguments{
\item{z}{Numeric vector with the values of the regionalized variable for which the values for the
spatial covariance parameters should be guessed.}

\item{coords}{Data frame or matrix with the projected x- and y-coordinates.}

\item{lags}{Numeric scalar defining the width of the variogram bins or a numeric vector with the
lower and upper bounds of the variogram bins. If missing, the variogram bins are computed using
\code{\link[=variogramBins]{variogramBins()}}. See \sQuote{Details} for more information.}

\item{cutoff}{Numeric value defining the fraction of the diagonal of the rectangle that spans the
data (bounding box) that should be used to set the maximum distance up to which variogram bins
should be computed. Defaults to \code{cutoff = 0.5}, i.e. half the diagonal of the bounding box.}

\item{method}{Character keyword defining the method used for guessing the spatial covariance
parameters of the regionalized variable. Defaults to \code{method = "a"}. See \sQuote{Details} for
more information.}

\item{min.npairs}{Positive integer defining the minimum number of point-pairs required so that a
variogram bin is used to guessing the spatial covariance parameters of the  of the regionalized
variable. Defaults to \code{min.npairs = 30}.}

\item{model}{Character keyword defining the spatial covariance function that will be fitted to
the data of the regionalized variable. Currently, most of the basic spatial covariance function
are accepted. See \code{geoR::cov.spatial()} for more information. Defaults to \code{model = "matern"}.}

\item{nu}{Numerical value for the additional smoothness parameter \eqn{\nu} of the spatial
covariance function of the regionalized variable. See \code{RandomFields::RMmodel()} and argument
\code{kappa} of \code{geoR::cov.spatial()} for more information.}

\item{estimator}{Character keyword defining the estimator for computing the sample (experimental)
variogram of the regionalized variable, with options \code{"qn"} (default), \code{"mad"}, \code{"matheron"}, and
\code{"ch"}. See \code{georob::sample.variogram()} for more details.}

\item{plotit}{Should the guessed spatial covariance parameters be plotted along with the sample
(experimental) variogram of the regionalized variable? Defaults to \code{plotit = FALSE}.}
}
\value{
A vector of numerical values, the guesses for the spatial covariance parameters of the
regionalized variable:
\itemize{
\item nugget
\item partial sill
\item range
}
}
\description{
Guess the parameters of the spatial covariance function of a random, regionalized variable. A
guess of such parameters is required to start the fitting functions of many geostatistical
packages such as \strong{gstat}, \strong{geoR}, and \strong{georob}.
}
\details{
There are five methods two guess the covariance parameters. Two of them, \code{"a"} and \code{"c"}, rely on
a sample variogram with exponentially growing variogram bins, while the other three, \code{"b"},
\code{"d"}, and \code{"e"}, use equal-width variogram bins (see \code{\link[=variogramBins]{variogramBins()}}). All of
them are \href{https://en.wikipedia.org/wiki/Heuristic}{heuristic}.

Method \code{"a"} was developed in-house and is the most elaborated of them, specially for guessing
the nugget variance.

Method \code{"b"} was proposed by \doi{10.1016/0098-3004(95)00095-X}{Jian et al. (1996)} and
is implemented in \href{https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_variogram_a0000000593.htm}{SAS/STAT(R) 9.22}.

Method \code{"c"} is implemented in the \strong{automap}-package and was developed by
\doi{10.1016/j.cageo.2008.10.011}{Hiemstra et al. (2009)}.

Method \code{"d"} was developed by \doi{10.1007/s11004-012-9434-1}{Desassis & Renard (2012)}.

Method \code{"e"} was proposed by Larrondo et al. (2003)
\url{http://www.ccgalberta.com/ccgresources/report05/2003-122-varfit.pdf} and is implemented in the
VARFIT module of GSLIB \url{http://www.gslib.com/}.
}
\section{Dependencies}{


The \strong{georob} package, provider of functions for the robust geostatistical analysis of spatial
data in R, is required for \code{\link[=variogramGuess]{variogramGuess()}} to work. The old versions of the
\strong{georob} package are available on the CRAN archive at
\url{https://cran.r-project.org/src/contrib/Archive/georob/}.
}

\examples{
if (all(c(require(sp), require(georob)))) {
  data(meuse, package = "sp")
  icp <- variogramGuess(z = log(meuse$copper), coords = meuse[, 1:2])
}
}
\references{
Desassis, N. & Renard, D. Automatic variogram modelling by iterative least squares: univariate
and multivariate cases. \emph{Mathematical Geosciences}. Springer Science + Business Media, v. 45, p.
453-470, 2012.

Hiemstra, P. H.; Pebesma, E. J.; Twenhöfel, C. J. & Heuvelink, G. B. Real-time automatic
interpolation of ambient gamma dose rates from the Dutch radioactivity monitoring network.
\emph{Computers & Geosciences}. Elsevier BV, v. 35, p. 1711-1721, 2009.

Jian, X.; Olea, R. A. & Yu, Y.-S. Semivariogram modelling by weighted least squares. \emph{Computers &
Geosciences}. Elsevier BV, v. 22, p. 387-397, 1996.

Larrondo, P. F.; Neufeld, C. T. & Deutsch, C. V. \emph{VARFIT: a program for semi-automatic variogram
modelling}. Edmonton: Department of Civil and Environmental Engineering, University of Alberta,
p. 17, 2003.
}
\seealso{
\code{\link[=variogramBins]{variogramBins()}}
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\concept{variogram}
