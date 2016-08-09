# >Perform time series analysis on the data for one of Milk Production, or Ice Cream Production (your choice), 
# in the CADairyProduction.csv file to answer the following questions 
# 
# –Is this time series stationary? 
# 
# –Is there a significant seasonal component? 
# 
# –For the remainder from the decomposition of the time series what is the order of the ARMA(p,q) process that best fits. 
# 
# –Forecast production for 12 months and examine numeric values andplot the confidence intervals. 
# Are the confidence intervals reasonably small compared to the forecast means.



library(dplyr)
library(zoo)


#==================
#### Load data ####
#==================

# Import the dataset
dairy.data <- read.csv('CADairyProduction.csv', header = TRUE)

# Format a date column
dairy.data$Date <- paste(dairy.data$Month, dairy.data$Year)
dairy.data$Date <- as.yearmon(dairy.data$Date, "%b %Y")
dairy.data$Date <- as.Date(dairy.data$Date)

## Make a time series object with ice cream production
icecream.ts <- ts(dairy.data$Icecream.Prod, start = 1995, frequency = 12)
plot(icecream.ts)

#=====================================
#### Stationarity test            ####
#=====================================

## Apply stationarity tests
library(tseries)
icecream.adf <- adf.test(icecream.ts)
icecream.adf
icecream.kpss <- kpss.test(icecream.ts)
icecream.kpss

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: Based on the p-values (<0.05 for adf, >0.05 for kpss)
# we cannot reject the null hypothesis that the data
# are stationary
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

f_acfplots <- function(ts) {
  print(acf(ts, na.action = na.pass))
  print(pacf(ts, na.action = na.pass))
}

f_acfplots(icecream.ts)

#&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: The current data show autocorrelation. Not surprising,
# due to possible seasonality.
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#=====================================
#### Seasonality  test            ####
#=====================================

# Run seasonality test
icecream.decompose <- decompose(icecream.ts)
plot(icecream.decompose)

## Do the residuals look like white noise?
qqnorm(icecream.decompose$random)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: There is a clear seasonal component, as seen in the 
# seasonal plot. The amplitude is ~30 with a period of 12mo.
# Residuals look like white noise, good.
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

## Plot acf plots
f_acfplots(ts(icecream.decompose$random))

#&&&&&&&&&&&&
# Insight: Though better, we can still not say that this is white noise.
#&&&&&&&&&&&&&&&&&





#=====================================
#### Model                     ####
#=====================================

## Find the best arma order:
f_best_arma <- function(decomp.random, p.range = 0:4, d.range = 0, q.range = 0:4) {
  arma.variants <- expand.grid(p.range, d.range, q.range)
  colnames(arma.variants) <- c("p", "d", "q")
  
  arma.variants$arma <- Inf
  for (i in 1:nrow(arma.variants)) {
    arma <- arima(decomp.random, order = c(arma.variants$p[i], arma.variants$d[i], arma.variants$q[i]))
    arma.variants$arma[i] <- arma$aic
  }
  
  return(arma.variants)
}

best.rand.arma <- f_best_arma(icecream.decompose$random)
# Best random arma is (4, 0, 4)

best.seasonal.arma <- f_best_arma(icecream.decompose$seasonal, p.range = 0:2, d.range = 0:2, q.range = 0)
# Best seasonal coefficients are (2,0,0)

## Make the final arma model
arma.model <- arima(icecream.ts,
                    order = c(4, 0, 4),
                    seasonal = list(order = c(2, 0, 0), period = 12))

## acf plots of arma model
f_acfplots(arma.model$residuals)
qqnorm(arma.model$residuals)




#=====================================
#### Forecast                     ####
#=====================================

# Forecast ice cream
ts.forecast <- forecast(arma.model, h = 12)
ts.forecast

## Plot the forecast:
plot(ts.forecast)

#&&&&&&&&&&&&&&&&
# Insight: The forecast shows as expected, with SEs that are reasonable
# given the magnitude of the signal.
#&&&&&&&&&&&&&&&&&&