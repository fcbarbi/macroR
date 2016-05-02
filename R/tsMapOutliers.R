
#'
#' @title Maps outliers to a dummy series. 
#' @name tsMapOutliers
#'
#' @description Creates a \code{ts} or \code{zoo} object with 1's in outlier positions and 0's elsewhere.
#'
#' @param x The time series data may not be stationary.
#' @param range Quantile range of acceptable values, defaults to \code{range=c(0.01,0.99)}.
#' @return ots a dummy \code{ts} object with 1 when the outlier is detected and 0 everywhere else.
#'
#' @details uses \code{tsDummy()} in identified outliers.
#' @export
#' @importFrom stats quantile 
#' 
# @seealso \code{\link{removeOutliers}}
#' @examples
#' x <- ts( c(8,rnorm(10),-15), start=c(2000,1),freq=12 )
#' xo1 <- tsMapOutliers( x )
#' xo2 <- tsMapOutliers( x, c(.05,.95) )

tsMapOutliers <- function( x,range=c(.01,.99) ){
  #res <- ifelse( is.na(tsworkflow::removeOutliers(x,range)), 1, 0 )
  if (missing(x)) stop("x must be supplied")
  qnt <- quantile( x, probs=range, na.rm = FALSE )
  res <- x*0 # ts or zoo object replicated and zeroed 
  res[x < qnt[1]] <- 1
  res[x > qnt[2]] <- 1
  res
}


# eof