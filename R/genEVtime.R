
#' @title Generates a vector of Eviews date formatted strings (eg.2008M12).
#' @name genEVtime
#'
#' @description Function to generate an Eviews-formatted vector of strings with time references.
#' @details Eviews formats dates of regularly spaced time series as YYYYPSS where YYYY is the year, P is the period (Month,Quarter) and SS is the subperiod (eg.1 to 12 for months)
#'
#' @param start String with the starting date formatted as \code{"2000M12"} or \code{"2002Q4"}.
#' @param end   String with the ending date, uses the same format as \code{start}. Optional if \code{qobs} specified. Defaults to NULL.
#' @param qobs  Number of observations. Optional if \code{end} specified. Defaults to NULL.
#'
#' @export
#'
#' @examples
#' genEVtime("1973Y1","1975Y1") # yeap, must explicit "Y1"
#' genEVtime("1947q3","1948Q02") # note the 0 before 2 is optional
#' genEVtime("1947M1",qobs=6) #
#'
genEVtime <- function( start, end=NULL, qobs=NULL )
{
  start_y <- substr(start,1,4) #year
  start_p <- toupper(substr(start,5,5)) #period
  # if (start_p=="") start_p <- "Y"  # to support year with Y and subperiod
  start_s <- substr(start,6,8) #subperiod
  # if (start_s=="") start_s <- "0"

  starts <- as.integer(start_s)
  starty <- as.integer(start_y)

  freq <- 0
  freq <- switch(start_p,"Y"=1,"S"=2,"Q"=4,"M"=12)
  if (freq==0) stop('frequencies supported are Y,S,Q,M ')

  if (starts>freq) stop('start subperiod not compatible with frequency')
  if (is.null(qobs) & is.null(end)) stop('either end or number obs must be supplied')

  endy <- 0
  if (!is.null(end)) {
    end_y <- substr(end,1,4)
    end_s <- substr(end,6,8)
    endy  <- as.integer(end_y)
    ends  <- as.integer(end_s)
    if(endy<starty) stop('end year is before than start year')
    if(ends>freq) stop('end subperiod not compatible with frequency')
  }

  time_out <- NULL
  sub  <- starts # subperiod
  year <- starty
  cobs <- 1 # counter
  cond <- FALSE
  if (!is.null(qobs))
    cond <- expression(cobs<=qobs)
  if (!is.null(end))
    cond <- expression((year<endy)||(year==endy && sub<=ends))

  while (eval(cond)) {
    if (freq==1)
      time_out <- c(time_out,paste0(year)) # year has no subperiod
    else
      time_out <- c(time_out,paste0(year,start_p,sub))
    sub <- sub+1
    cobs <- cobs+1
    if (sub>freq){ sub<-1; year<-year+1 }
  }
  time_out
}

