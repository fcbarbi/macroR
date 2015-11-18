```
gentime <- function( start, end=NULL, qobs=NULL )
{

  start_y <- substr(start,1,4)
  start_p <- toupper(substr(start,5,5))
  start_s <- substr(start,6,8)

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
# test: command and output 
# gentime("1947Q3","1948Q2") # 1947Q3, 1947Q3, 1948Q1, 1948Q2
# gentime("1947M4","1949M1")
# gentime("1947Y1","1949Y1") # 1947, 1948, 1949
# gentime("1947S2","1949S1")
```
