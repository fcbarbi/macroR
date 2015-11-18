
## "Stuff" (small tweaks) for R programming

A lot of the data scientist's time is spent on collecting, cleaning and formatting data from different sources. These are routines I use frequently, they may speed up your work with little adjustments or serve as inspiration. If you have issues drop me a line at fcbarbi AT gmail.com. Enjoy! 

### gen_eviews_time( start, end=NULL, qobs=NULL )

Generates a time vector as string in **Eviews** format. Useful when exporting data to Eviews.  
Supports Month, Quarter, Semester and Year frequencies.
Start and end are structured as YYYYFSS, where YYYY is year, F is frequency (M, Q, S or Y) and SS is subperiod (1 to 12 if frequency is M, or 1 to 4 for Q). You can call in two ways, either by providing **start** and **end** or **start** and the **quantity of observations** to generate. 

Example: 
```
gen_eviews_time("1947Q3","1948Q02") # returns "1947Q3", "1947Q4", "1948Q1", "1948Q2"
gen_eviews_time("1947Y1",nobs=5) # returns "1947", "1948", "1949", "1950", "1951"
```
Implementation: [link](https://github.com/fcbarbi/R_Stuff/blob/master/gen_eviews_time.md)

### dlog( ts ) 

Generates the first difference log (interpreted as % change of **ts**) from a time series (class "ts") object **ts**.
Negative and zeros generate NaN, so you may want to transform **ts** into an index before dlogging it.  

Example: 
```
asset_return <- dlog(asset_price) # I(1) data made into I(0)
dlcpi <- dlog(cpi) # DL prefix tips on the transformation  
```
Implementation:
```
dlog <- function(x) { 
   if ("ts" %in% class(x))
     diff(log(x)) 
   else 
   stop("Must be a time series, use ts().") 
} 
```

### clean_num_values( dirty_strings )
Converts **dirty_strings** with spaces, points and commas ("123 456" and "123.456,78") into floating numbers.
The decimal and thousand separators may have to be adjusted accordingly. This is useful when importing values from textual sources (scrapping web pages, for example) that use different number formatting rules. 

Examples:
```
dirty <- c( "34.567,89", "34 567", 451, "", "123 456.789,0001" )
clean_num_values(dirty[1])
clean_num_values(dirty)
```
Implementation:
```
clean_num_values <- function( dirty )
{
   clean_this <- function( v ) {
     if (!is.numeric(v)) {
       v <- gsub("\\.",'',v )  # thousand separator
       v <- gsub(" ",'',v ) 
       v <- as.numeric( gsub(',','.',v) )   # decimal separator
     }
     if (is.na(v)) v <- 0
     v
   }

   if (length(dirty)>1) 
	 clean <- as.numeric( lapply( dirty, clean_this ))
   else	
    clean <- clean_this(dirty)
	
   clean	
}
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
Inspired by [link](http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset)
