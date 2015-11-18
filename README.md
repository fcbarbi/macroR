
## "Stuff" (ie.small tweaks) for R programming

### gen_ev_time( start, end=NULL, qobs=NULL )

Generates a time vector as string in **Eviews** format. Useful when exporting data to Eviews.  
Supports Month, Quarter, Semester and Year frequencies.
Start and end are structured as YYYYFSS, where YYYY is year, F is frequency (M, Q, S or Y) and SS is subperiod (1 to 12 if if frequency is M).  

Two ways of calling: either provide start and end or start and the number of periods to generate. 

Example: 
```
gen_ev_time("1947Q3","1948Q02") # returns "1947Q3", "1947Q4", "1948Q1", "1948Q2"
gen_ev_time("1947Y1",nobs=5) # returns "1947", "1948", "1949", "1950", "1951"
```
Implementation: [link](https://github.com/fcbarbi/R_Stuff/blob/master/gentime.md)

### dlog( ts ) 
Generates the first difference log (interpreted as % change of **ts**) from a time series (ts, xts, zoo) object **ts**.
Negative and zeros generate NaN, so you may want to transform **ts** into an index before dlogging it.  

Example: 
```
asset_return <- dlog(asset_price)
dlcpi <- dlog(cpi) # DL informs on the transformation of the series 
```
Implementation:
```
dlog <- function(x) { 
   if ("ts" %in% class(x) 
     diff(log(x)) 
   else 
   stop("Must be a time series, use ts().") 
} 
```

### clean_num_values( strings, dec_sep=",", k_sep=c("."," ") )
Convert strings with spaces, points and commas ("123 456" and "123.456,78") to numeric.
The decimal and thousand separators can be passed as parameters.  
This is useful when importing values from textual sources (scrapping web pages, for example) that use european formatting rules. 

Examples:
```
clean_num_values( "456.000" ) # 456000
clean_num_values( "456,000" ) # 456
clean_num_values( "123 456.789,0001" ) # 123456789.0001
clean_num_values( " " ) # NA
clean_num_values( c("34 567", 451) )  # 34567, 451
```
Implementation:
```
(TBC)
```

### remove_outliers( num, na.rm = TRUE, ... )
Remove outlier values, those below 1% and above 99% quantiles, from a numeric vector. 

Example:
```
set.seed(123)
x <- rnorm(100)
x <- c(-10, x, 10)
y <- remove_outliers(x)
par(mfrow = c(1, 2))
boxplot(x)
boxplot(y)
```
Implementation:
```
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.01, .99), na.rm = na.rm, ...)  
  y <- x
  y[x < qnt[1]] <- NA
  y[x > qnt[2]] <- NA
  y
}
```
