
# Tools for R programming

### gentime( start, end=NULL, nobs=NULL )
Generates a time vector as string in Eviews format. Supports Month, Quarter, Semester and Year frequencies.
Start and end are structured as YYYYPSS, where YYYY is year, F is frequency and SS is subperiod (months if frequency is M). 

Example: 
```
gentime("1947Q3","1948Q02") # returns "1947Q3", "1947Q4", "1948Q1", "1948Q2"
gentime("1947Y1",nobs=5) # returns "1947", "1948", "1949", "1950", "1951"
```

### dlog( x ) 
Generates a series with the numeric vector **x** in first difference log (interpreted as % change of **x**).
Negative and zeros generate NA.

Example: 
```
asset_return <- dlog(asset_price)
```
Implementation:
```
dlog <- function(x) { diff(log(x)) } 
```

### adjvalues( strings )
Convert strings with space and points ("123 456" and "123.456.789") to double.

Examples:
```
adjvalue( "456.000" ) # 456000
adjvalue( 456.000 )   # 456000
adjvalue( "4.698.449" ) # 4698449
adjvalue( "123 456.789,0001" ) # 123456789
adjvalue( " " ) # NA
adjvalues( c("34 567", 451) )  # 34567 451
```
Implementation:
```
adjvalues <- function( values ){
   value <- sapply( values, adjvalue, simplify = TRUE)
   as.vector( value )
}

adjvalue <- function(v) {
  if (!is.numeric(v)) {
    v <- gsub("\\.",'',v ) 
    v <- gsub(" ",'',v ) 
    v <- as.numeric( gsub(',','.',v) )
  }
  if (is.na(v)) 
    v <- 0
  else 
    if (v<1000) { v <- v*1000 }
  v
}
```

