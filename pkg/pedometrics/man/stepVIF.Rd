\name{stepVIF}
\alias{stepVIF}
\title{
Variable selection using the variance-inflation factor
}
\description{
This function takes a linear model and selects the subset of predictor variables 
that meet a user-specific collinearity threshold measured by the
variance-inflation factor (VIF).
}
\usage{
stepVIF(model, threshold = 10, verbose = FALSE)
}
\arguments{
  \item{model}{
  Linear model (object of class 'lm') containing collinear predictor variables.
  }
  \item{threshold}{
  Positive number defining the maximum allowed VIF. Defaults to
  \code{threshold = 10}.
  }
  \item{verbose}{
  Logical for indicating if iteration results should be printed.
  Defaults to \code{verbose = FALSE}.
  }
}
\details{
\code{stepVIF()} starts computing the VIF of all predictor variables in the
linear model. Because some predictor variables can have more than one degree of
freedom, such as categorical variables, generalized variance-inflation factors 
(Fox and Monette, 1992) are calculated instead using \code{\link[car]{vif}}. 
Generalized variance-inflation factors (GVIF) consist of VIF corrected to the 
number of degrees of freedom (df) of the predictor variable:

\eqn{GVIF = VIF^{1/(2\times df)}}{GVIF = VIF^[1/(2*df)]}

GVIF are interpretable as the inflation in size of the confidence ellipse or
ellipsoid for the coefficients of the predictor variable in comparison with what
would be obtained for orthogonal data (Fox and Weisberg, 2011).

The next step is to evaluate if any of the predictor variables has a VIF larger 
than the specified threshold. Because \code{stepVIF()} estimates GVIF and the
threshold corresponds to a VIF value, the last is transformed to the scale of 
GVIF by taking its square root. If there is only one predictor variable that 
does not meet the VIF threshold, it is authomatically removed from the model 
and no further processing occurs. When there are two or more predictor variables 
that do not meet the VIF threshold, \code{stepVIF()} fits a linear model between 
each of them and the dependent variable. The predictor variable with the lowest
adjusted coefficient of determination is dropped from the model and new
coefficients are calculated, resulting in a new linear model.

This process lasts until all predictor variables included in the new model meet 
the VIF threshold.

Nothing is done if all predictor variables have a VIF value inferior to the
threshold, and \code{stepVIF()} returns the original linear model.
}
\value{
A linear model (object of class 'lm') with low collinearity.
}
\references{
Fox, J. and Monette, G. (1992)
  Generalized collinearity diagnostics.
  \emph{JASA}, \bold{87}, 178--183.

Fox, J. (2008)
  \emph{Applied Regression Analysis and Generalized Linear Models},
  Second Edition. Sage.

Fox, J. and Weisberg, S. (2011)
  \emph{An R Companion to Applied Regression}, Second Edition. Thousand Oaks: Sage.

Hair, J. F., Black, B., Babin, B. and Anderson, R. E. (2010)
   \emph{Multivariate data analysis}. New Jersey: Pearson Prentice Hall.
  
Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition. Springer.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\note{
The function name \code{stepVIF()} is a variant of the widely used function
\code{\link[MASS]{stepAIC}}.
}
\section{TODO}{
\enumerate{
\item Include other criteria (RMSE, AIC, etc) as option to drop collinear 
predictor variables.
}
}
\seealso{
\code{\link[car]{vif}},
\code{\link[MASS]{stepAIC}}.
}
\examples{
#require(car)
#fit <- lm(prestige ~ income + education + type, data = Duncan)
#stepVIF(fit, threshold = 10, verbose = TRUE)
}
\keyword{methods}
\keyword{regression}
% End!