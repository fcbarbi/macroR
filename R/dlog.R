
#' @title Calculates the percentage variation of a time series.
#' @name dlog
#'
#' @description Generates series of the percentage variation of the series data passed as parameter.
#' @details Data must be time indexed, either as \code{ts} or \code{zoo}. If observation(s) are negative or zero, the algorithm changes to the traditional percentage variation.
#'
#' @param data Time series or vector to be converted to a \code{ts} object.
#' @param lag (Optional) Number of lags to differentiate with \code{diff()}. Defaults to \code{1}.
#' @return Time series of the percentage variation with the first position(s) as \code{NA}.
#'
#' @export dlog
# @importFrom zoo iszoo 
# @importFrom zoo zooreg
#'
#' @seealso \code{\link{pVarToIndex}}
#'
#' @examples
#' CPI <- ts( rnorm(10,mean=100) )
#' Inflation <- dlCPI <- dlog( CPI )  # choose your naming convention!
#'
#' x <- ts( seq(-.3,.6,.1) )
#' dlx <- dlog( x ) # switch algorithm to avoid generating NAs 
#' dl2x <- dlog( x, lag=2 ) 

# x2 <- ts( c(100,rnorm(9,mean=100)) )
# dlog( x2 ) 
# 
# x1 <- ts( seq(-.3,.6,.1) )
# dlog( x1, lag=1 ) 
# dlog( x1, lag=2 ) 

# log of first differences
dlog <- function( data, lag = 1 ) {

  if (!is.ts(data) & !zoo::is.zoo(data)) 
    stop('data must be time indexed: use ts() or zooreg() to convert.')

  fillup<-NA # dynlm() adjusts sample to NAs
  if (any(data<0)) 
    res <- deltaPX(data,lag=lag,fillup=fillup)  # 0 or NA in the fisrt obs ??
  else 
    res <- c(fillup,diff(log(data),lag=lag)) 
  
  if (is.ts(data)) 
     res <- ts( data=res, start=start(data),frequency=frequency(data) )
  else 
    res <- zoo::zooreg( data=res, start=start(data),frequency=frequency(data) )
  res 
  
}

# percentage variation
# Internal use only, do NOT export (as no checks are done)
# dlx <- dlog(x)
# dpx <- deltaPX(x)
# all(abs(dlx-dpx)<1e-3)
deltaPX <- function(x,lag=1,fillup=NA) {
  dx <- rep(fillup,length(x))
  cx <- zoo::coredata(x)
  for (i in (lag+1):length(x)) 
    dx[i] <- (cx[i]-cx[i-lag])/cx[i-lag]
  if (is.ts(x)) res <- ts( data=dx, start=start(x),frequency=frequency(x))
  if (zoo::is.zoo(x)) res <- zoo::zooreg( data=dx, start=start(x),frequency=frequency(x))
  res 
}
