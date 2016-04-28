
#' @title dlog
#' @name dlog
#'
#' @description Generates series of the percentage variation of the series data passed as parameter.
#' @details It is implicit that data is index by time (time series). If observations are negative or zero, the \code{convert=TRUE} option allows the automatic transformation to an index before passing the \code{log()}.
#'
#' @param x Time series or vector to be converted to a \code{ts} object.
#' @param convert (Optional) Controls the automatia conversion to index before passing \code{log()}. Defaults to \code{TRUE}.
#' @return Time series is the percentage variation with the first position as \code{0}.
#'
#' @export dlog
#'
#' @seealso \code{\link{toIndex}}
#'
#' @examples
#' x <- ts( rnorm(10,mean=100) )
#' dlx <- dlog(x)
#'
#' x1 <- ts( seq(-.3,.6,.1) )
#' dlog( x1, convert=FALSE ) # causes NA when log(x) for x<=0

# log of first differences
dlog <- function( x, convert=TRUE ) {
  if (!is.ts(x) & !zoo::is.zoo(x)) stop('x must be a ts (time series) or zoo object.')
  dlog.ts(x, convert)
}

# Internal use only, do NOT export (no checks done here)
dlog.ts <- function( x, convert=TRUE ) {
  if (any(x<0) & convert) x <- toIndex(x)
  c(0,diff(log(x)))
}

# TODO
# dlog.data.frame <- function( x, convert=TRUE ) {}

# percentage variation, just to check
# dpx <- tsworkflow:::deltaPX(x)
# if (any(abs(dlx-dpx)>1e-3)) warning('significant differences') else warning('no differences')
deltaPX <- function(x) {
  dx <- rep(0,length(x))
  for (i in 2:length(x)) dx[i] <- (x[i]-x[i-1])/x[i-1]
  dx
}
