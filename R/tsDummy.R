
#'
#' @title Generates a ts object for a dummy variable.
#' @name tsDummy
#'
#' @description Generates a \code{ts} object filled with 0's, with 1's in a period or specific dates.
#' @details Note that more than one period can be specified.
#'
#' @param start      Same as the \code{start} option in a \code{ts} object.
#' @param end        Same as the \code{end} option in a \code{ts} object.
#' @param frequency  Same as the \code{frequency} option in a \code{ts} object.
#' @param period     (optional) List with two elements: the starting and ending dates for the 1's formated as for ex. \code{c(2008,12)}.
#' @param dates      (optional) List of discontinuous dates formated as for ex. \code{c(2008,12)}.
#' @return ts object with 1's and 0's.
#' 
#' @export
#' @seealso \code{\link{tsComplete}}
#' 
#' @examples
#' lehman <- tsDummy( start=c(2000,1), end=c(2015,12), freq=12, period=list( c(2008,9), c(2008,12) ))
#' 
#' oilshocks <- tsDummy( start=c(1970,1), end=c(2015,12), freq=12,
#'                                          dates=list( c(1973,10), c(1979,12), c(1990,10), c(2008,6) )  )
#'

tsDummy <- function( start, end, frequency=12 , period=NULL, dates=NULL ) {
  
  if (missing(start)) stop("supply a start date formated as c(2001,12).")
  if (missing(end)) stop("supply an end date formated as c(2001,12).")
  
  d <- ts( 0, start=start, end=end, frequency=frequency )
  if (!missing(period))
   window( d, start=period[[1]], end=period[[2]] ) <- 1
  else
   if (!is.null(dates))
     for (i in seq(1,length(dates))) window( d, start=dates[[i]], end=dates[[i]] ) <- 1
   else
     warning("No period or dates specified, provide one of them formated as in c(2001,12).")
  d
}















