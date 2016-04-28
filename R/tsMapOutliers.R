
#'
#' @title tsMapOutliers
#' @name tsMapOutliers
#'
#' @description Creates a \code{ts} object with 1 in outlier positions and 0 elsewhere.
#'
#' @param x A \code{ts} object with data that may not be stationary, either I(2), I(1) or I(0).
#' @param range Quantile range of acceptable values, defaults to \code{range=c(0.01,0.99)}.
#' @return ots a dummy \code{ts} object with 1 when the outlier is detected and 0 everywhere else.
#'
#' @details uses \code{tsDummy()} in identified outliers.
#' @export
#' @seealso \code{\link{removeOutliers}}
#' @examples
#' x <- ts( c(8,rnorm(10),-15), start=c(2000,1),freq=12 )
#' xo1 <- tsMapOutliers( x )
#' xo2 <- tsMapOutliers( x, c(.05,.95) )

tsMapOutliers <- function( x,range=NULL ){
  res <- ifelse( is.na(tsworkflow::removeOutliers(x,range)), 1, 0 )
  res
}

#install.packages("outliers")
# tsoutliers::outlier



# ---------------------------------------------------------------
