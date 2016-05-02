#' @title numToIndex
#' @name numToIndex
#' 
#' @description Converts a numeric series to an index using the first observation as reference.
#' @details If using a monetary series be sure to deflate it before indexing. 
#' 
#' @param x         Time-indexed data to convert, must be a ts or zoo object.
#' @param reference (Optional) Observation to be used as proportions are calculated. 
#' @param base      (Optional) Defaults to 100.
#' @return          Time series (ts or zoo) with the index. 
#' 
#' @export
#' @seealso \code{\link{pVarToIndex}}
#' @examples
#' # Production averages 1254 units per month  
#' production <- ts( rnorm(30*12,mean=1254,sd=425),start=c(1980,1),freq=12)
#' ip <- numToIndex( production ) # industrial production index 
#
# implements 'reference' but uses a proportional rule to calculate the result
numToIndex <- function( x, reference=1, base=100 ) {
  
  if (!is.ts(x) & !zoo::is.zoo(x)) 
    stop("toIndex must be called with a ts or zoo object.")
  if (!is.vector(reference) | !is.numeric(reference)) 
    stop("reference is the numeric position in x or a date as in c(2008,1).")
  
#   if (zoo::is.zoo(x)) {
#     index_x <- zoo::index(x)
#     frequency_x <- zoo::frequency(x)
#   }
#   else {
    index_x <- zoo::index(x)
    frequency_x <- frequency(x)
  # }  
  
  position <- 1L
  if (is.null(reference))
    position <- which(x==min(x))
  else {
    if (length(reference)>=2L) {
      # is the reference date inside x ?
      refnum <- reference[1]+(reference[2]-1)/frequency_x
      position <- which(abs(refnum-index_x)<1e-6)
      #position <- window(x,start=reference,end=reference)
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
  res <- round( res,4 )
  if (is.ts(x))   
    res <- ts( data=res, start=start(x),frequency=frequency(x))
  else 
    res <- zoo::zooreg( data=res, start=start(x),frequency=frequency(x))
  res 
}
