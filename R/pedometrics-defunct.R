#' Defunct functions in the __pedometrics__ package
#'
#' The functions listed here are no longer part of the __pedometrics__ package as they are not
#' needed (any more).
#'
#' @param ... Not used.
#'
#' @return No return value, called for side effects.
#'
#' @export
#' @rdname pedometrics-defunct
#' @aliases coordenadas febr febr2spdf febr2xlsx standard header
coordenadas <-
  function(...) {
    msg <- "'pedometrics::coordenadas()' is defunct.\nSee https://github.com/samuel-rosa/ASRtools"
    .Defunct(msg = msg)
  }
#' @export
#' @rdname pedometrics-defunct
cdfPlot <-
  function(...) {
    msg <- "'pedometrics::cdfPlot()' is defunct.\nSee https://github.com/samuel-rosa/ASRtools"
    .Defunct(msg = msg)
  }
#' @export
#' @rdname pedometrics-defunct
cdfStats <-
  function(...) {
    msg <- "'pedometrics::cdfStats()' is defunct.\nSee https://github.com/samuel-rosa/ASRtools"
    .Defunct(msg = msg)
  }
