# Final Project

library(lubridate)
library(dplyr)
library(ggplot2)
library(car)

#~~~~~~~~~~~~~~~~~~~~
#### Data Import ####
#~~~~~~~~~~~~~~~~~~~~

## Import raw data
air.q.data <- read.csv("data/AirQualityUCI.csv", header = TRUE, stringsAsFactors = FALSE)

## Column definitions:
# 0 Date	(DD/MM/YYYY) 
# 1 Time	(HH.MM.SS) 
# 2 True hourly averaged concentration CO in mg/m^3 (reference analyzer) 
# 3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)	
# 4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer) 
# 5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer) 
# 6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)	
# 7 True hourly averaged NOx concentration in ppb (reference analyzer) 
# 8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted) 
# 9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)	
# 10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)	
# 11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted) 
# 12 Temperature in Â°C	
# 13 Relative Humidity (%) 
# 14 AH Absolute Humidity 

## Note: "GT" collumns contain ground truth measures, while PT08 columns are sensor readings


## Build clean date column
air.q.data$timestamp <- paste(air.q.data$Date, air.q.data$Time, sep = " ") 
air.q.data$timestamp <-mdy_hms(air.q.data$timestamp)





#~~~~~~~~~~~~~~~~~~~~
#### Exploration ####
#~~~~~~~~~~~~~~~~~~~~

## Temp & Humidity
qplot(x = timestamp, y = T, data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = RH, data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = AH, data = air.q.data, geom = c("line", "point"))

## CO measures
qplot(x = timestamp, y = CO.GT., data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = PT08.S1.CO., data = air.q.data, geom = c("line", "point"))

## NMHC Measures
qplot(x = timestamp, y = NMHC.GT., data = air.q.data, geom = c("line", "point"))  # Broken?
qplot(x = timestamp, y = PT08.S2.NMHC., data = air.q.data, geom = c("line", "point"))

## NOX measures
qplot(x = timestamp, y = NOx.GT., data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = PT08.S3.NOx., data = air.q.data, geom = c("line", "point"))

## NO2 measures
qplot(x = timestamp, y = NO2.GT., data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = PT08.S4.NO2., data = air.q.data, geom = c("line", "point"))
qplot(x = timestamp, y = PT08.S5.O3., data = air.q.data, geom = c("line", "point"))  # O3 aligns to NO2 GT

## C6H6 Measures (Benzene)
qplot(x = timestamp, y = C6H6.GT., data = air.q.data, geom = c("line", "point"))







##################
# Questions:
# 1) How well do the sensors match the ground truth recordings?
# 2) Forecast polution levels
# 3) Bootstrap differences in day/night, days, months
# 4) How does benzene correlate to pollutants?


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 1) Sensor Performance ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


f_build_data <- function(data, GT, PT08) {
  new.data <- data[ , c("timestamp", substitute(GT), substitute(PT08))]
  new.data <- new.data[which(new.data[,2] > -200), ]
  new.data <- new.data[which(new.data[,3] > -200), ]
  
  return(new.data)
}

f_normplots <- function(data) {
  par(mfrow = c(2,2))
  hist(data[,2])
  qqPlot(data[,2])
  hist(log(data[,2]))
  qqPlot(log(data[,2]))
  par(mfrow = c(1,1))
  
  par(mfrow = c(2,2))
  hist(data[,3])
  qqPlot(data[,3])
  hist(log(data[,3]))
  qqPlot(log(data[,3]))
  par(mfrow = c(1,1))

}

f_shapiro <- function(data) {
  if (length(data) > 5000) {
    data <- data[sample(1:length(data), size = 5000)]
  }
  
  print(shapiro.test(data))
  
  print(shapiro.test(log(data)))
}

f_error_plots <- function(data, GT, PT08) {
  print(plot(data[,substitute(GT)], data$error))
  print(qqPlot(data$error))
  print(plot(data[, substitute(GT)], data[, substitute(PT08)]))
}


## CO measures
CO.data <- f_build_data(air.q.data, GT = "CO.GT.", PT08 = "PT08.S1.CO.")
f_normplots(CO.data)

# Transformations
f_shapiro(CO.data$CO.GT.)  # Use log
CO.data$log.CO.GT. <- log(CO.data$CO.GT.)

f_shapiro(CO.data$PT08.S1.CO.)  # Use log
CO.data$log.PT08.S1.CO. <- log(CO.data$PT08.S1.CO.)

# Measure GT-PT error
CO.data$error <- CO.data$log.CO.GT. - CO.data$log.PT08.S1.CO.
f_error_plots(CO.data, GT = "log.CO.GT.", PT08 = "log.PT08.S1.CO.")




## NMHC Measures
NMHC.data <- f_build_data(air.q.data, GT = "NMHC.GT.", PT08 = "PT08.S2.NMHC.")
f_normplots(NMHC.data)

# Transformations
f_shapiro(NMHC.data$NMHC.GT.)  # Use log
NMHC.data$log.NMHC.GT. <- log(NMHC.data$NMHC.GT.)

f_shapiro(NMHC.data$PT08.S2.NMHC.)  # Use log
NMHC.data$log.PT08.S2.NMHC. <- log(NMHC.data$PT08.S2.NMHC.)

# Measure sensor error
NMHC.data$error <- NMHC.data$log.NMHC.GT. - NMHC.data$log.PT08.S2.NMHC.
f_error_plots(NMHC.data, GT = "log.NMHC.GT.", PT08 = "log.PT08.S2.NMHC.")





## NOX measures
NOx.data <- f_build_data(air.q.data, GT = "NOx.GT.", PT08 = "PT08.S3.NOx.")
f_normplots(NOx.data)

# Transformations
f_shapiro(NOx.data$NOx.GT.)
NOx.data$log.NOx.GT. <- log(NOx.data$NOx.GT.)

f_shapiro(NOx.data$PT08.S3.NOx.)
NOx.data$log.PT08.S3.NOx. <- log(NOx.data$PT08.S3.NOx.)

# Measure sensor error
NOx.data$error <- NOx.data$log.NOx.GT. - NOx.data$log.PT08.S3.NOx.
f_error_plots(NOx.data, GT = "log.NOx.GT.", PT08 = "log.PT08.S3.NOx.")





## NO2 measures
NO2.S4.data <- f_build_data(air.q.data, GT = "NO2.GT.", PT08 = "PT08.S4.NO2.")
f_normplots(NO2.S4.data)
NO2.S5.data <- f_build_data(air.q.data, GT = "NO2.GT.", PT08 = "PT08.S5.O3.")
f_normplots(NO2.S5.data)

# Transformations

# Measure sensor errors




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Correlation of benzene to pollutants ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C6H6.NMHC <- f_build_data(air.q.data, GT = "C6H6.GT.", PT08 = "NMHC.GT.")
C6H6.CO <- f_build_data(air.q.data, GT = "C6H6.GT.", PT08 = "CO.GT.")
C6H6.NOx <- f_build_data(air.q.data, GT = "C6H6.GT.", PT08 = "NOx.GT.")
C6H6.NO2 <- f_build_data(air.q.data, GT = "C6H6.GT.", PT08 = "NO2.GT.")
