
#'
#' @title Generates a table with Unit Roor test results 
#' @name urTable
#'
#' @description Returns a datatable and generates a .csv or .tex file with Unit Root test results.
#' @details Supports ADF, PP and KPSS unit root tests. 
#'
#' @param df        Dataframe with series to test
#' @param tests     List of tests to be performed, by default tests are \code{c("adf","pp","kpss")}. 
#' @param order     Maximum order of integration to test, usually noted as I(?). It can be \code{c(0,1,2)}.
# @param structure List of models to test, can be \code{c("c","ct","nc")}: \code{"c"} assumes model with intercept,"ct" assumes intercept and deterministic trend,"nc" assumes no intercept and no trend.
#' @param file      (Optional) Filename to be generated according to \code{format}.
#' @param format    (Optional) Format of the output table, can be \code{c("csv","latex")}. Defaults to \code{"txt"}.
#'
#' @return A dataframe with the test results and a file (optional).
#'
#' @importFrom tseries adf.test
#' @importFrom tseries pp.test
#' @importFrom tseries kpss.test
#' @importFrom xtable  xtable
#' @export
#' 
#' @examples
#' 
#' require(tseries)
#' data(USeconomic,package="tseries")
#' data <- data.frame( USeconomic ) 
#' dft1 <- urTable( data, tests=c("adf","pp","kpss"), file="US.csv" )
#' dft2 <- urTable( data, tests=c("pp","kpss"), order=2, file="US.tex", format="latex" )
#' 

urTable <- function( df ,tests=c("adf","pp","kpss"), order=1, file=NULL, format="csv" ){

  # TODO: include UR test for structural changes, maybe 
  # fUnitRoots::unitrootTest()
  
  # validate arguments 
  if (!(order %in% c(0,1,2))) stop("order must be 0, 1 or 2")
  intorder <- seq(0,order) 

  format <- match.arg(format, c("csv","latex"))
  
  if (!is.data.frame(df)) stop("first argument must be a data frame")

  for (i in 1:length(tests)) 
    match.arg(tests[i],c("adf","pp","kpss"))

  # prepare the structure for the output dataframe 
  nTest <- length(tests)
  nVar  <- length(colnames(df))
  nInt  <- length(intorder)
  core <- matrix( rep(NA,nTest*nVar*nInt), ncol=nTest*nInt )

  # response data frame has one row for each variable and one colum for each test 
  dft <- data.frame( core )
  rownames(dft) <- colnames(df)
  colnames <- c()
  for (i in intorder) colnames <- c(colnames,paste0(tests,"(",i,")"))
  colnames(dft) <- colnames 
 
  for (i in intorder)  
   for (test in tests)
    for (series in colnames(df)) { 
      data <- df[[series]]
      if (i>0) data <- diff(data,i)
      col <- paste0(test,"(",i,")")  # cols = test(intorder) as in "adf(0)" or "kpss(2)"
      if (test=="adf")   dft[series,col] <- tseries::adf.test(data)$p.value
      if (test=="pp")    dft[series,col] <- tseries::pp.test(data)$p.value
      if (test=="kpss")  dft[series,col] <- tseries::kpss.test(data)$p.value
    }

  # adf  <- "Augmented Dickey-Fuller Test"
  # pp   <- "Phillips-Perron Unit Root Test"
  # kpss <- "KPSS Test for Level Stationarity"
 
  if (!is.null(file)) {
    if (format=="latex") 
      print( xtable::xtable(dft), type="latex", file=file ) 
    else 
      write.csv( dft, file=file )
  }
  dft
}
