
#' @title tsComplete
#' @name tsComplete
#'
#' @description Function to complete a time-indexed series (either \code{ts} or \code{zoo}) by combining it with another series to avoid NAs. The function has a third argument "fill" to chose whether to leave NA, interpolate or repeate the last value when both vectors are missing.
#' @details Supports both \code{ts} and \code{zoo}. Converts to \code{zoo} if one of the parameters is \code{zoo}.

#' @param x main data series (either \code{ts} or \code{zoo}) to be completed for missing observations.
#' @param y secondary series (either \code{ts} or \code{zoo}) to use only when missing in the main series.
#' @param fill method to treat NA's after combining \code{x} and \code{y}, can be "na","fill","interpolate" or "repeat"
#'
#' @return Returns either a \code{ts} (default) or \code{zoo} object.
#'
#' @export
#'
#' @examples
#' a  <- c(NA, 1, 2,NA,NA, 5,NA)
#' b  <- c(   NA,12,NA,13,NA,15,16)
#' a2 <- ts(a,start=c(2000,1),freq=4)
#' b2 <- ts(b,start=c(2000,2),freq=4)
#' b3 <- zoo::zoo(b2)
#' aa  <- tsComplete(a2)
#' ab1 <- tsComplete(a2,b2)
#' ab2 <- tsComplete(a2,b2,fill="na")
#' ab3 <- tsComplete(a2,b3)
#' ab4 <- tsComplete(a2,b3,fill="fill")

tsComplete <- function( x,y,fill="fill" )
{
  if (missing(x)) stop("x must be supplied")
  if (!is.ts(x) & !zoo::is.zoo(x)) stop("x must be a 'ts' or 'zoo' object.")
  if (!missing(y)) if (!is.ts(y) & !zoo::is.zoo(y)) stop("y is optional but if supplied it must be a 'ts' or 'zoo' object.")
  if (!missing(y)) if (is.ts(x) & is.ts(y)) if (tsp(x)[3]!=tsp(y)[3]) stop("Cannot combine TS with different frequencies.")
  # TODO: check freq match for zoo objects

  # if x not zoo but y is zoo then transform x into zoo
  if (!zoo::is.zoo(x) & !missing(y))
    if (zoo::is.zoo(y)) x <- zoo::zooreg(x, start=tsp(x)[1], freq=tsp(x)[3] )

  # synchronize series
  sx <- sync <- x
  lCombined <- FALSE
  if (is.ts(x) & !missing(y)) { sync <- ts.union(x,y); lCombined <- TRUE }
  if (zoo::is.zoo(x) & !missing(y)) { sync <- zoo::merge.zoo(x,y); lCombined <- TRUE }
  #print(sync) # debug
  if (lCombined) {
    sx <- sync[,1]
    if (!missing(y)) sy <- sync[,2]
  }
  # combine only if sx is NA and sy not NA
  temp <- sx
  if (!missing(y)) {
    mask <- which(is.na(sx))
    temp[mask] <- sy[mask]
  }

  # complete the remaining NA according to a user defined rule
  fill.options <- c("na","fill","interpolate","repeat")
  fill <- match.arg(fill,fill.options)
  choice <- which(fill==fill.options)
  # we rely on the fact that a ts object can implicitly be transformed into a zoo and back to ts
  res <- switch( choice, temp,  zoo::na.fill(temp, "extend"),
                                zoo::na.approx(temp), zoo::na.locf(temp) )
  # check that (ts,ts) -> ts, (zoo,ts) or (ts,zoo) -> zoo and (zoo,zoo) -> zoo
  res
}



