#'
#' @title toIndex
#' @name toIndex
#'
#' @description Convert series to index based 100 corresponding to the reference observation given in optional parameter \code{base}.
#' @details
#'
#' @param x Time series to be converted to index.
#' @param base (optional) is the index of the observaton to be used as reference.
#'
#' @export toIndex
#'
#' @seealso \code{\link{dlog}}
#' @examples
#' Inflation <- ts( rnorm(13,mean=.2,sd=.1), start=c(2007,12), freq=12 )
#' Cpi <- toIndex(Inflation)
#'
# TODO: allow to pass a date as argument
# Cpi <- toIndex(Inflation,base= ts.date( Inflation, c(2008,1) ) )
#
# ts.date <- function( ts, date ) {
#  step <- (tsp(ts)[2]-tsp(ts)[1])/tsp(ts)[3]
#  (date-tsp(ts)[1])/step  # BUG: converter date
# }

toIndex <- function(x,base=1) {
  if (!is.ts(x)) stop("toIndex must be called with a ts object.")
  rangex <- max(x)-min(x)
  x <- x + rangex
  if (is.null(base))
    basex <- min(x)
  else
    basex <- x[base]
  100*(1+(x-basex)/basex)
}
