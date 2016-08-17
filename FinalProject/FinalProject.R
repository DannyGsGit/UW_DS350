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

f_predict <- function(data, model, independent) {
  prediction <- as.data.frame(predict(model, data, interval = "predict"))
  data <- cbind(data, prediction)
  data$residuals <- data[,substitute(independent)] - data$fit
  
  return(data)
}

f_plot_prediction_ci <- function(data, feature, target) {
  plot(data[, substitute(feature)], data[, substitute(target)])
  lines(data[, substitute(feature)], data$fit, col = "blue", lwd = 2)
  lines(data[, substitute(feature)], data$lwr, col = "red", lwd = 2)
  lines(data[, substitute(feature)], data$upr, col = "red", lwd = 2)
}


## CO measures
CO.data <- f_build_data(air.q.data, GT = "CO.GT.", PT08 = "PT08.S1.CO.")
f_normplots(CO.data)

# Transformations
f_shapiro(CO.data$CO.GT.)  # Use log
CO.data$log.CO.GT. <- log(CO.data$CO.GT.)

f_shapiro(CO.data$PT08.S1.CO.)  # Use log
CO.data$log.PT08.S1.CO. <- log(CO.data$PT08.S1.CO.)

# Plot relationship between transformed variables
qplot(log.PT08.S1.CO., log.CO.GT., data = CO.data)

# Model relationship
CO.model <- lm(log.CO.GT. ~ log.PT08.S1.CO., data = CO.data)
summary(CO.model)
plot(CO.model)

# Remove outliers & re-train model
outliers <- c(2764:2765, 4513:4518, 5703, 5785:5786, 5793, 6800, 6815:6819, 6825, 6837, 7123)
CO.data.no.outliers <- CO.data[-outliers, ]

CO.model.2 <- lm(log.CO.GT. ~ log.PT08.S1.CO., data = CO.data.no.outliers)
summary(CO.model.2)
plot(CO.model.2)

# Measure performance of new model
CO.data <- f_predict(CO.data, CO.model.2, "log.CO.GT.")

f_plot_prediction_ci(CO.data, "log.PT08.S1.CO.", "log.CO.GT.")











## NOX measures
NOx.data <- f_build_data(air.q.data, GT = "NOx.GT.", PT08 = "PT08.S3.NOx.")
f_normplots(NOx.data)

# Transformations
f_shapiro(NOx.data$NOx.GT.)
NOx.data$log.NOx.GT. <- log(NOx.data$NOx.GT.)

f_shapiro(NOx.data$PT08.S3.NOx.)
NOx.data$log.PT08.S3.NOx. <- log(NOx.data$PT08.S3.NOx.)

# Plot relationship between transformed variables
qplot(log.PT08.S3.NOx., log.NOx.GT., data = NOx.data)

# Model relationship
NOx.model <- lm(log.NOx.GT. ~ log.PT08.S3.NOx., data = NOx.data)
summary(NOx.model)
plot(NOx.model)

# Remove outliers & re-train model
outliers <- c(2897, 5027:5029)
NOx.data.no.outliers <- NOx.data[-outliers, ]

NOx.model.2 <- lm(log.NOx.GT. ~ log.PT08.S3.NOx., data = NOx.data.no.outliers)
summary(CO.model.2)
plot(CO.model.2)

# Measure performance of new model
NOx.data <- f_predict(NOx.data, NOx.model.2, "log.NOx.GT.")

f_plot_prediction_ci(NOx.data, "log.PT08.S3.NOx.", "log.NOx.GT.")


















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 4) Correlation of benzene to pollutants ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Benzene - CO
C6H6.CO <- f_build_data(air.q.data, GT = "CO.GT.", PT08 = "C6H6.GT.")
f_normplots(C6H6.CO)

# Transformations
f_shapiro(C6H6.CO$CO.GT.)
C6H6.CO$log.CO.GT. <- log(C6H6.CO$CO.GT.)

f_shapiro(C6H6.CO$C6H6.GT.)
C6H6.CO$log.C6H6.GT. <- log(C6H6.CO$C6H6.GT.)

# Add time labels
C6H6.CO$time <- hour(C6H6.CO$timestamp)
C6H6.CO$wday <- wday(C6H6.CO$timestamp)
C6H6.CO$day <- day(C6H6.CO$timestamp)
C6H6.CO$month <- month(C6H6.CO$timestamp)
C6H6.CO$year <- year(C6H6.CO$timestamp)

# Plot relationship between transformed variables
qplot(log.C6H6.GT., log.CO.GT., data = C6H6.CO)

co.by.time <- ggplot(C6H6.CO, aes(log.C6H6.GT., log.CO.GT.)) +
  geom_point(aes(colour = month, alpha = 0.3, size = 1)) +
  scale_color_gradient2(mid="red", high="blue", low="blue", midpoint = 6)
co.by.time





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 2) Polution Forecasts ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 3) Bootstrap differences- daytime, weekday, etc. ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###
# Tasks:
# * Calculate hourly level as proportion of daily average
# * Daily level as proportion of weekly average
# * Bootstrap differences

#### CO Differences

qplot(timestamp, log.CO.GT., data = C6H6.CO)

### By time of day
co.by.time <- ggplot(C6H6.CO, aes(jitter(time), log.CO.GT.)) +
  geom_point(aes(colour = month, alpha = 0.5, size = 1)) +
  scale_color_gradient2(mid="red", high="blue", low="blue", midpoint = 6) +
  geom_smooth()
co.by.time

## Adjust hourly values by daily means

# Get daily means
daily.CO.averages <- C6H6.CO %>% select(day, month, year, log.CO.GT.)
daily.CO.averages <- group_by(daily.CO.averages, day, month, year)
daily.CO.averages <- summarise(daily.CO.averages, 
                               daily.CO.mean = mean(log.CO.GT.),
                               daily.CO.sd = sd(log.CO.GT.))

# Merge daily means back into dataset
C6H6.CO <- merge(C6H6.CO, daily.CO.averages, by = c("day", "month", "year"))

# Calculate relative CO concentration
C6H6.CO <- C6H6.CO %>% mutate(rel.daily.CO = log.CO.GT. - daily.CO.mean)

rel.co.by.time <- ggplot(C6H6.CO, aes(jitter(time), rel.daily.CO)) +
  geom_point(aes(colour = month, alpha = 0.5, size = 1)) +
  scale_color_gradient2(mid="red", high="blue", low="blue", midpoint = 6) +
  geom_smooth()
rel.co.by.time

# Bootsrap differences between hours






# By weekday
co.by.wday <- ggplot(C6H6.CO, aes(jitter(as.numeric(wday)), log.CO.GT.)) +
  geom_point(aes(colour = month, alpha = 0.5, size = 1)) +
  scale_color_gradient2(mid="red", high="blue", low="blue", midpoint = 6) +
  geom_smooth()
co.by.wday



## NOx forecasts