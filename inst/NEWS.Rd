\name{NEWS}

\title{News for Package \pkg{pedometrics}}

\section{Changes in version 0.4-2 (2014-12-08)}{
  \itemize{
    \item Function to compute the Cramer's V, a measure of association between
    categorical variables.
  }
}

\section{Changes in version 0.4-1 (2014-12-01)}{
  \itemize{
    \item Documentation is now generated using roxygen2.
  }
}

\section{Changes in version 0.4-0 (2014-11-26)}{
  \itemize{
    \item Started the fourth season of the initial development of the package.
    Many functions are being implemented in C++ to speed-up the computation.
  }
}

\section{Changes in version 0.3-1 (2014-11-23)}{
  \itemize{
    \item Improving the spatial simulated annealing.
  }
}

\section{Changes in version 0.3-0 (2014-11-15)}{
  \itemize{
    \item Started the third season of the initial development of the package.
    Significant changes were made in functions spSANN(), spJitter(), and those
    used as objective functions in spatial simulated annealing.
  }
}

\section{Changes in version 0.2-4 (2014-10-07)}{
  \itemize{
    \item Spatial simulated annealing is now available: spSANN().
  }
}

\section{Changes in version 0.2-3 (2014-10-07)}{
  \itemize{
    \item Corrected bugs in functions objPairs(), objPoints(), pairs_per_lag(),
    points_per_lag() and spJitter(). Added new criterion to objPairs() and
    objPoints(). Added new method to spJitter() (finite set of candidate
    locations). Most of the changes speed up the functions and increase
    their functionalities.
  }
}

\section{Changes in version 0.2-2 (2014-10-02)}{
  \itemize{
    \item Included functions objPairs(), objPoints(), pairs_per_lag() and 
    points_per_lag(). These functions were designed to be used with spatial
    simulated annealing.
  }
}

\section{Changes in version 0.2-1 (2014-09-30)}{
  \itemize{
    \item Included functions spJitter() and bbox2sp().
  }
}

\section{Changes in version 0.2-0 (2014-09-25)}{
  \itemize{
    \item Started the second season of the initial development of the package.
    Included functions plotHD() and plotESDA().
  }
}

\section{Changes in version 0.1-9 (2014-08-01)}{
  \itemize{
    \item Removed \code{toar()} and \code{readsat()} because they are better
          implemented using GRASS GIS commands directly.
  }
}

\section{Changes in version 0.1-8 (2014-07-03)}{
  \itemize{
    \item Second public release (R-Forge).
    \item Included function \code{buildMS()}, used to build a series of linear 
          models using automated variable selection. Included function
          \code{statsMS()}, used to obtain performance statistics of a series
          of linear models. Included function \code{plotMS}, used to plot the
          statistics of a models series. Corrected bugs detected during the
          first built.
  }
}

\section{Changes in version 0.1-7 (2014-06-16)}{
  \itemize{
    \item First public release (R-Forge).
    \item Several corrections in the documentation, preparing for the first public release.
  }
}

\section{Changes in version 0.1-6 (2014-05-25)}{
  \itemize{
    \item Seventh internal release.
    \item Function \code{cdfTable()}: added argument to return a data.frame object.
  }
}

\section{Changes in version 0.1-5 (2014-05-21)}{
  \itemize{
    \item Sixth internal release.
    \item Function \code{cdfPlot()}: added argument to define if the confidence limits of the CDF should be included in the plot.
  }
}

\section{Changes in version 0.1-4 (2014-05-20)}{
  \itemize{
    \item Fifth internal release.
    \item Function \code{cdfPlot()} now has options to show CDF parameters.
  }
}

\section{Changes in version 0.1-3 (2014-04-21)}{
  \itemize{
    \item Fourth internal release.
    \item Included function \code{stepVIF()}. Renamed many of the functions to comply with the style guide. The package was also renamed to \pkg{pedometrics}.
  }
}


\section{Changes in version 0.1-2 (2014-02-13)}{
  \itemize{
    \item Third internal release.
    \item Included functions \code{readsat()} and \code{toar()}.
  }
}

\section{Changes in version 0.1-1 (2014-01-23)}{
  \itemize{
    \item Second internal release.
    \item Minor correction in function \code{deltagcp()}.
  }
}

\section{Changes in version 0.1-0 (2013-06-12)}{
  \itemize{
    \item First internal release.
    \item Six functions available: \code{cdfplot()}, \code{cdfstats()},
    \code{cdftable()}, \code{coordenadas()}, \code{deltagcp()}, and
    \code{gcpvector()}.
  }
}
