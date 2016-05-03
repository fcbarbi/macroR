#'
#' @title Converts a percentage variation series into a base 100 index series. 
#' @name pVarToIndex
#'
#' @description Compounds a percentage variation series into a 100 based index corresponding to the reference observation.
#' @details The index is based in 100. Reference is by default the first observation.  
#'
#' @param x         Time series to be converted to index.
#' @param base       (optional) Default is 100. 
#' @param reference  (optional) Index of the observation to be used as reference. Default is 1 (first).
#'
#' @export pVarToIndex
#'
#' @seealso \code{\link{dlog}}, \code{\link{numToIndex}}
#' 
#' @examples
#' # Monthly inflation is around 2%
#' Inflation <- ts( rnorm(13,mean=2,sd=.5), start=c(2007,12), freq=12 )
#' CPI_dec07 <- pVarToIndex( Inflation )
# CPI_jan08 <- pVarToIndex( Inflation, reference = c(2008,1) )
# CPI       <- pVarToIndex( Inflation, reference = NULL ) # ref is the smallest value
#'

# t <- ts( rnorm(13,mean=.2,sd=.1), start=c(2007,12), freq=12 )
# toIndex( t, base = 1 )
# toIndex( t, ref = -2 )
# toIndex( t, ref = c(2008,1) )
# toIndex( t, ref = c(2008,0) )
# toIndex( t, ref = c(2008,13) )
# 
# z <- as.zoo( t ) 
# toIndex( z, base = 1 )
# toIndex( z, ref = -2 )
# toIndex( z, ref = c(2008,11) )
# toIndex( z, ref = c(2008,13) )

# 
# z1 <- toIndex(z) 
# z2 <- dlog(z1)

# does not yet implement 'reference'
pVarToIndex <- function( x, base=100, reference=1 ) {
  
  if (!is.ts(x) & !zoo::is.zoo(x))
    stop("toIndex must be called with a ts or zoo object.")
  if (!is.vector(reference) | !is.numeric(reference)) 
    stop("reference is the numeric position in x or a date as in c(2008,1).")
  
  res <- base*c(1,cumprod(1+x[-length(x)]/100))
  res <- round( res,4 )  # round to 4 decimal places 
  if (is.ts(x))   
    res <- ts( data=res, start=start(x),frequency=frequency(x))
  else 
    res <- zoo::zooreg( data=res, start=start(x),frequency=frequency(x))
  res 
}  

#eof
