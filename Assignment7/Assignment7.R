
library(dplyr)
library(tseries)
library(forecast)


#==================
#### Load data ####
#==================

## Import the dataset
dairy.data <- read.csv('CADairyProduction.csv', header = TRUE)

## Make a time series object with ice cream production
icecream.ts <- ts(dairy.data$Icecream.Prod, start = 1995, frequency = 12)
plot(icecream.ts, xlab = "Date", ylab = "Ice Cream Sales", main = "Ice Cream Sales over Time")




#=====================================
#### Stationarity test            ####
#=====================================

## Apply stationarity tests
# Stationarity on raw data
icecream.adf <- adf.test(icecream.ts, k = 12)
icecream.adf
# Stationarity on diff of time series
icecream.diff.adf <- adf.test(diff(icecream.ts), k = 12)
icecream.diff.adf


## Based on above test, we will use the diff of the time series
## for modeling
arma.data <- diff(icecream.ts)

## Print the autocorrelation plots of the chosen time series
f_acfplots <- function(ts) {
  print(acf(ts, na.action = na.pass))
  print(pacf(ts, na.action = na.pass))
}
f_acfplots(arma.data)




#=====================================
#### Seasonality  test            ####
#=====================================

## Extract seasonality from decomposition
icecream.decompose <- decompose(arma.data)
plot(icecream.decompose)

## Do the residuals look like white noise?
qqnorm(icecream.decompose$random)

## Plot acf plots
f_acfplots(ts(icecream.decompose$random))



#=====================================
#### Model                     ####
#=====================================

## Make the final arma model
arma.model <- auto.arima(arma.data, 
                         max.p=3, max.d=3, max.q=0,
                         max.P=0, max.D=3, max.Q=3,
                         max.order=5,
                         start.p=0, start.q=0,
                         start.P=0, start.Q=0)
summary(arma.model)

## acf plots of arma model
f_acfplots(arma.model$residuals)
## QQ Plot of residuals
qqnorm(arma.model$residuals)




#=====================================
#### Forecast                     ####
#=====================================

# Forecast ice cream
ts.forecast <- forecast(arma.model, h = 24)
summary(ts.forecast)

## Plot the forecast:
plot(ts.forecast)

