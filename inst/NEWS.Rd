\name{NEWS}

\title{News for Package \pkg{pedometrics}}

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
