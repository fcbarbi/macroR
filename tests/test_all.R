
# test_all.R
# unloadNamespace(c("zoo","RUnit")) # do not require RUnit to be loaded but installed
# @importFrom RUnit checkIdentical
# @importFrom RUnit checkException

#require(zoo)
#require(RUnit)
#require(macror)

# dlog ---------------------------------------

core <- rnorm(13,mean=.2,sd=.1)
z <- zoo::zooreg( core, start=c(2007,12), freq=12 )
z1 <- macror:::deltaPX(z)
t <- as.ts( z )
t1 <- macror:::deltaPX(t)
RUnit::checkIdentical( zoo::coredata(z1), zoo::coredata(t1) )

x <- ts( rnorm(10,mean=100) )
RUnit::checkIdentical( macror::dlog(x), ts(c(NA,diff(log(x)))))
RUnit::checkIdentical( macror::dlog(x,2), ts(c(NA,diff(log(x),2))))
                                                          
x <- zoo::zooreg( rnorm(10,mean=100) )
RUnit::checkIdentical( macror::dlog(x), zoo::zooreg(c(NA,diff(log(x)))))
                                              
# x1 <- ts( seq(-.3,.6,.1) )
# x11<-dlog( x1, lag=1 ) 
# x12<-dlog( x1, lag=2 )
# cbind(x1,x11,x12)

#x <- ts( c(-1,0,1,10,1e2,1e3) )
#ix <- toIndex(x)
#RUnit::checkIdentical( dlog(x), dlog(ix) )

# genEVtime  ---------------------------------------

RUnit::checkIdentical( macror::genEVtime("1947Y1","1949Y1"), c("1947","1948","1949"))
RUnit::checkIdentical( macror::genEVtime("1947q3","1948Q02"), c("1947Q3", "1947Q4", "1948Q1", "1948Q2") )
RUnit::checkIdentical( macror::genEVtime("1947S2","1949S1"), c("1947S2","1948S1","1948S2","1949S1") )
RUnit::checkIdentical( macror::genEVtime("1947m1",qobs=6), c("1947M1","1947M2","1947M3","1947M4","1947M5","1947M6") )

# removeOutliers ---------------------------------------
# 
# xx <- rnorm(10)
# x <- c(0,-10, xx, 10)
# y <- removeOutliers(x)
# RUnit::checkIdentical( y, c(0,NA,xx,NA) )

# toIndex ---------------------------------------
# 
# x <- ts( c(-1,0,1,10,1e2,1e3) )
# ix <- numToIndex(x)
# RUnit::checkIdentical( ix, ts( c(100,100.1,100.2,101.1,110.1,200.1) ) )

x2 <- ts( c(1,2,3,-1,-2,0,4))
ix2 <- macror::tsVarToIndex(x2)
ok <- c(100,101.0,103.02,106.1106,105.0495,102.9485,102.9485)
RUnit::checkIdentical( ix2, ts(ok)  ) 

x3 <- zoo::as.zoo(x2)
ix3 <- macror::tsVarToIndex(x3)
RUnit::checkIdentical( ix3, zoo::zooreg(ok)  ) 

# x <- ts( c(1000,1010,999,1200.1) )
# RUnit::checkIdentical( tsNumToIndex( x ), ts( c(100,101,99.9,120.01) ) ) 
# 
# x <- zoo::zooreg( c(1000,1010,999,1200.1) )
# RUnit::checkIdentical( tsNumToIndex( x ), zoo::zooreg( c(100,101,99.9,120.01) ) ) 


# tsComplete --------------------------------------------

a  <- c(NA, 1, 2,NA,NA, 5,NA)
b  <- c(   NA,12,NA,13,NA,15,16)
a2 <- ts(a,start=c(2000,1),freq=4)
b2 <- ts(b,start=c(2000,2),freq=4)
a3 <- zoo::zooreg(a,start=c(2000,1),freq=4)
b3 <- zoo::zooreg(b,start=c(2000,2),freq=4)
aa2 <- macror::tsComplete(a2)
ab1 <- macror::tsComplete(a2,b2)
ab2 <- macror::tsComplete(a2,b2,fill="na")
ab3 <- macror::tsComplete(a2,b3)
ab4 <- macror::tsComplete(b3,a2)
ab5 <- macror::tsComplete(a3,b3)
ab6 <- macror::tsComplete(a3,b3,fill="fill")
b2m <- ts(b,start=c(2000,2),freq=12)

RUnit::checkIdentical( aa2, ts(c(1,1,2,3,4,5,5),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab1, ts(c(1.0,1.0,2.0,7.5,13.0,5.0,15.0,16.0),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab2, ts(c(NA,1,2,NA,13,5,15,16),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab3, zoo::zooreg( c(1,1,2,7.5,13,5,15,16),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab4, zoo::zooreg( c(1,1,12,12.5,13,5,15,16),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab5, zoo::zooreg( c(1,1,2,7.5,13,5,15,16),start=c(2000,1),freq=4) )
RUnit::checkIdentical( ab6, zoo::zooreg( c(1,1,2,7.5,13,5,15,16),start=c(2000,1),freq=4) )

RUnit::checkException( macror::tsCombine(b2m,a2), silent=TRUE ) # cant combine diff freq

#RUnit::checkIdentical( tsCombine(a,b,fill="int"),  ts(c(1,2,13,9,5)) )    # DANGER shorter!
#RUnit::checkIdentical( tsCombine(a,b,fill="rep"),  ts(c(1,2,13,13,5,5)) ) # DANGER shorter!

# tsDummy ---------------------------------------

d1 <- macror::tsDummy( start=c(2000,1), end=c(2001,12), freq=12, period=list( c(2001,9), c(2001,11) )  )
d2 <- macror::tsDummy( start=c(2000,1), end=c(2001,12), freq=12, dates=list( c(2001,9), c(2001,10), c(2001,11) )  )
RUnit::checkIdentical( d1, d2 )

# tsMapOutliers ---------------------------------------

x <- ts( c(8,rnorm(10),-5) )
xo <- macror::tsMapOutliers( x, c(.05,.95) )
RUnit::checkIdentical( xo, ts(c(1,rep(0,10),1)) )

# urTable ---------------------------------------

# data(USeconomic) # from tseries 
# data <- data.frame( USeconomic ) 
# dft1 <- macror::urTable( data, tests=c("adf","pp","kpss"), file="USeconomic.csv" )
# dft2 <- macror::urTable( data, tests=c("pp","kpss"), order=2, file="USeconomic.tex", format="latex" )

bad <- data.frame( var1=rnorm(100), var2=cumsum(rnorm(100)) ) 
bad[seq(95,100),"var1"] <- bad[seq(1,5),"var2"] <- bad[seq(50,55),"var2"] <- NA
dft3 <- macror::urTable( bad )


#eof