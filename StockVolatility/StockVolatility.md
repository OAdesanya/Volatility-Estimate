
---
title: Stock Volatility Analysis
author: "OLufemi Adesanya"
date: "July 21, 2016"
output:
  html_document:
    Keep_md: true
    ---
### Analysis of CO stock
#### China Corp Blood Corporation
```{r}

install.packages("tseries", repos ="http://cran.rstudio.com/") #installing tseries package
library(tseries)
```
#### Import Data
```{r}
COdata <- get.hist.quote('co',quote="Close")
head(COdata)
```
#### Calculating volatility using the number of work days multiply by 100 to get the percentage. *250* is the total number of days stock market is opened.
```{r}
COret <- log(lag(COdata)) - log(COdata)
COvol <- sd(COret) * sqrt(250) * 100
```
 
 
#### Volatility
```{r}
COvol
```
 
 
#### Function to calculate volatitlity with weight
```{r}
Vol <- function(d, logrets)
{
 
  var = 0
 
  lam = 0
 
  varlist <- c()
 
  for (r in logrets) {
   
    lam = lam*(1 - 1/d) + 1
   
    var = (1 - 1/lam)*var + (1/lam)*r^2
   
    varlist <- c(varlist, var)
   
  }
 
  sqrt(varlist)
}
```
 
#### Decay Factor of .9
```{r}
volest <- Vol(10,COret)
```
 
#### Decay Factor of .6
```{r}
volest2 <- Vol(40,COret)
 
```
 
#### Decay Factor of .6
```{r}
volest3 <- Vol(80,COret)
```
 
####Plots including all three decay factors estimating the volatility.
There is a high volatility estimate change around the 600 index point .The estimate for the .2 decay factor (blue line) is smoother compared to the other two, the spike is not as sharp as the other decay factors.The plot shows that the volatility is fairly stable majority of the time except for some small spikes.
 
```{r}
plot(volest,type="l")
lines(volest2,type="l",col="red") 
 
lines(volest3, type = "l", col="blue")
 
legend("topright",legend = c(".9 decay factor", ".6 decay factor", ".2 decay factor"),
       col = c( "black","red", "blue"), lty =1.2,cex =0.8)
```