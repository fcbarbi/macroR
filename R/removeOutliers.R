
#'
#' @title removeOutliers
#' @name removeOutliers
#'
#' @description Function to remove "outlier" observations as defined by quantile range of acceptable values.
#' @details Source: \url{http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset}
#'
#' @param x Series to be filtered
#' @param ok_range Quantile range of accepted values
#' @param na.rm Control NA removal with TRUE (to remove NAs) or FALSE (to keep them).
#'
#' @export
#' @importFrom stats quantile
#'
#' @examples
#'
#' set.seed(123)
#' x <- rnorm(100)
#' x <- c(-10, x, 10)
#' y <- removeOutliers(x)
# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(y)
#'
removeOutliers <- function( x, range=c(.01,.99), na.rm = TRUE, ... ) {
  if (missing(x)) stop("x must be supplied")
  qnt <- quantile(x, probs=range, na.rm = na.rm, ...)
  y <- x
  y[x < qnt[1]] <- NA
  y[x > qnt[2]] <- NA
  y
}
