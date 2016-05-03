#' @title Converts a numeric series to an index using the first observation as reference.
#' @name tsNumToIndex
#' 
#' @description Converts a numeric series to an index using the first observation as reference.
#' @details If using a monetary series be sure to deflate it before indexing. 
#' 
#' @param x         Time-indexed data to convert, must be a ts or zoo object.
#' @param base      (Optional) Default is 100.
#' @param reference (Optional) Observation to be used as proportions are calculated. 
#' @return          Time series (ts or zoo) with the index. 
#' 
#' @export
#' @seealso \code{\link{tsVarToIndex}}
#' @examples
#' # Production averages 1254 units per month  
#' production <- ts( rnorm(30*12,mean=1254,sd=425),start=c(1980,1),freq=12)
#' ip  <- tsNumToIndex( production ) # industrial production index 
#' ip2 <- tsNumToIndex( production, reference = c(1992,6) ) # rebase for June 1992
#
# implements 'reference' but uses a proportional rule to calculate the result
tsNumToIndex <- function( x, base=100, reference=1 ) {
  
  if (!is.ts(x) & !zoo::is.zoo(x)) 
    stop("tsNumToIndex must be called with a ts or zoo object.")
  if (!is.vector(reference) | !is.numeric(reference)) 
    stop("reference is the numeric position in x or a date as in c(2008,1).")
  
  position <- 1L
  if (is.null(reference))
    position <- which(x==min(x))
  else {
    if (length(reference)>=2L) {
      # is the reference date inside x ?
      refnum <- reference[1]+(reference[2]-1)/frequency(x)
      position <- which(abs(refnum-zoo::index(x))<1e-6)
      if (length(position)==0L) { 
        warning("reference out of range, assuming the first") 
        position <- 1L 
      }
    }  
    if (length(reference)==1) 
      position <- abs(reference[1L]) 
  }
  refx <- x[position]
  if (is.na(refx)) { 
    refx <- x[1]; 
    warning("reference position not found, assuming the first") 
  }
  if (zoo::is.zoo(refx)) 
    refx <- zoo::coredata(refx)
  
  res <- base*(1+(x-refx)/refx)
  res <- round( res,4 ) # rounding to 4 decimal places 
  if (is.ts(x))   
    res <- ts( data=res, start=start(x),frequency=frequency(x) )
  else 
    res <- zoo::zooreg( data=res, start=start(x),frequency=frequency(x) )
  res 
}
