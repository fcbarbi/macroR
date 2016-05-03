#'
#' @title Converts a percentage variation series into a base 100 index series. 
#' @name tsVarToIndex
#'
#' @description Compounds a percentage variation series into a 100 based index corresponding to the reference observation.
#' @details The index is based in 100. Reference is by default the first observation.  
#'
#' @param x         Time series to be converted to index.
#' @param base       (optional) Default is 100. 
#' @param reference  (optional) Index of the observation to be used as reference. Default is 1 (first).
#'
#' @export tsVarToIndex
#'
#' @seealso \code{\link{dlog}}, \code{\link{tsNumToIndex}}
#' 
#' @examples
#' # Monthly inflation is around 2%
#' Inflation <- ts( rnorm(13,mean=2,sd=.5), start=c(2007,12), freq=12 )
#' CPI_dec07 <- tsVarToIndex( Inflation )
# CPI_jan08 <- tsVarToIndex( Inflation, reference = c(2008,1) )
# CPI       <- tsVarToIndex( Inflation, reference = NULL ) # ref is the smallest value
#'


# does not yet implement 'reference'
tsVarToIndex <- function( x, base=100, reference=1 ) {
  
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
