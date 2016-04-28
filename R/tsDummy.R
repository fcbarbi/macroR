
#'
#' @title tsDummy
#' @name tsDummy
#'
#' @description Generates a \code{ts} object filled with 0's, with 1's in a period or specific dates.
#' @details
#'
#' @param start Same as the \code{start} date in a \code{ts} object.
#' @param end Same as the \code{end} date in a \code{ts} object.
#' @param freq Same as the \code{frequency} date in a \code{ts} object.
#' @param period (optional) List with two elements: the starting and ending dates for the 1's formated as for ex. \code{c(2008,12)}.
#' @param dates (optional) List of discontinuous dates formated as for ex. \code{c(2008,12)}.
#' @export
#' @examples
#' lehman    <- tsDummy( start=c(2000,1), end=c(2015,12), freq=12, period=list( c(2008,9), c(2008,12) )  )
#' oilshocks <- tsDummy( start=c(1970,1), end=c(2015,12), freq=12,
#'                                          dates=list( c(1973,10), c(1979,12), c(1990,10), c(2008,6) )  )
#'
tsDummy <- function( start, end, freq=12 , period=NULL, dates=NULL ) {
  if (missing(start)) stop("supply a start date formated as c(2001,12).")
  if (missing(end)) stop("supply an end date formated as c(2001,12).")
  d <- ts( 0, start=start, end=end, freq=freq )
  if (!missing(period))
   window( d, start=period[[1]], end=period[[2]] ) <- 1
  else
   if (!is.null(dates))
     for (i in seq(1,length(dates))) window( d, start=dates[[i]], end=dates[[i]] ) <- 1
   else
     warning("No period or points specified for the dummy, choose a period of specific dates formated as c(2001,12).")
  d
}

# dummy.ts <- function( ts , period=NULL, points=NULL ) {
#   dts <- ts.dummy( start=tsp[1],end=tsp[2],freq=tsp[3],period=period,points=points )
#   dts
# }
#
# gdp <- ts( 0.1*seq(1:(16*4))+rnorm(16*4,sd=.1),start=c(2000,1), end=c(2015,4), freq=4)
# gdp <- window( gdp,start=c(2008,10),end=c(2009,3), freq=4 ) -0.5
# plot(gdp)
#
# dcrisis2 <- dummy.ts( gdp, period=list(c(2008,3),c(2009,2)) )

# eof














