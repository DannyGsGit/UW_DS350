# Final Project

library(lubridate)
library(dplyr)
library(ggplot2)
library(car)
library(simpleboot)
library(tseries)
library(knitr)
library(RColorBrewer)

source('FinalProject_Functions.R')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Data Import ####

## Import raw data
air.q.data <- read.csv("data/AirQualityUCI.csv", header = TRUE, stringsAsFactors = FALSE)

## Build clean date column
air.q.data$timestamp <- paste(air.q.data$Date, air.q.data$Time, sep = " ") 
air.q.data$timestamp <-mdy_hms(air.q.data$timestamp)

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





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Exploration ####

## View the data structure
str(air.q.data)

## Investigate pair plots
pairs(air.q.data[,3:15])







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1) Generate Data Subsets ####

## CO measures
CO.data <- f_build_data(air.q.data, GT = "CO.GT.", PT08 = "PT08.S1.CO.")

# Check normality of variables
f_normplots(CO.data)
f_shapiro(CO.data$CO.GT.)  # Use log
f_shapiro(CO.data$PT08.S1.CO.)  # Use log

# Apply transforms
CO.data$log.CO.GT. <- log(CO.data$CO.GT.)
CO.data$log.PT08.S1.CO. <- log(CO.data$PT08.S1.CO.)

# Add time labels
CO.data <- f_time_labels(CO.data)

# Plot relationship between transformed variables
qplot(log.PT08.S1.CO., log.CO.GT., data = CO.data)




## NOX measures
NOx.data <- f_build_data(air.q.data, GT = "NOx.GT.", PT08 = "PT08.S3.NOx.")

# Check normality of variables
f_normplots(NOx.data)
f_shapiro(NOx.data$NOx.GT.)
f_shapiro(NOx.data$PT08.S3.NOx.)

# Apply transforms
NOx.data$log.NOx.GT. <- log(NOx.data$NOx.GT.)
NOx.data$log.PT08.S3.NOx. <- log(NOx.data$PT08.S3.NOx.)

# Add time labels
NOx.data <- f_time_labels(NOx.data)

# Plot relationship between transformed variables
qplot(log.PT08.S3.NOx., log.NOx.GT., data = NOx.data)














#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2) Sensor Performance ####

# Model relationship
CO.model <- lm(log.CO.GT. ~ log.PT08.S1.CO., data = CO.data)
summary(CO.model)
plot(CO.model)

# Remove outliers & re-train model
CO.data.no.outliers <- f_remove_outliers(CO.data, c(2764:2765, 4513:4518, 5703, 5785:5786, 5793, 6800, 6815:6819, 6825, 6837, 7123))

CO.model.2 <- lm(log.CO.GT. ~ log.PT08.S1.CO., data = CO.data.no.outliers)
summary(CO.model.2)
plot(CO.model.2)

# Measure performance of new model
CO.data <- f_predict(CO.data, CO.model.2, "log.CO.GT.")

f_plot_prediction_ci(CO.data, "log.PT08.S1.CO.", "log.CO.GT.")







# Model relationship
NOx.model <- lm(log.NOx.GT. ~ log.PT08.S3.NOx., data = NOx.data)
summary(NOx.model)
plot(NOx.model)

# Remove outliers & re-train model
NOx.data.no.outliers <- f_remove_outliers(NOx.data, c(2897, 3031, 3044, 5026:5029))

NOx.model.2 <- lm(log.NOx.GT. ~ log.PT08.S3.NOx., data = NOx.data.no.outliers)
summary(NOx.model.2)
plot(NOx.model.2)

# Measure performance of new model
NOx.data <- f_predict(NOx.data, NOx.model.2, "log.NOx.GT.")

f_plot_prediction_ci(NOx.data, "log.PT08.S3.NOx.", "log.NOx.GT.")














#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2) Compare differences- daytime, weekday, etc. ####


#### CO Differences

### By time of day
CO.data.periodic <- f_adjust_hourly(CO.data, "log.CO.GT.")
f_timeplot(CO.data.periodic, x = "time", y = "rel.daily.target", color = "month", midpoint = 6)

# Bootsrap differences between hours
CO.hourly.boot <- f_multilevel_one.boot(CO.data.periodic, "time", "rel.daily.target")
f_multi_hist(CO.hourly.boot, simplify = TRUE)

# Compare bootstrapped means
CO.hourly.two.boot <- f_multilevel_two.boot(CO.data.periodic, "time", "rel.daily.target")
f_multi_hist(CO.hourly.two.boot, simplify = TRUE)

# ANOVA Comparison
CO.data.periodic$time <- as.factor(CO.data.periodic$time)
CO.hourly.ANOVA <- f_anova_analyses(data = CO.data.periodic, formula = formula(log.CO.GT. ~ time),
                                    truncate = TRUE)


### By weekday
wday.averages <- f_adjust_daily(CO.data.periodic, target.col = "log.CO.GT.")
f_timeplot(wday.averages, x = "wday", y = "rel.daily.target", color = "week", midpoint = 26)

# Bootsrap differences between hours
CO.daily.boot <- f_multilevel_one.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(CO.daily.boot, simplify = TRUE)

# Compare bootstrapped means
CO.daily.two.boot <- f_multilevel_two.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(CO.daily.two.boot, simplify = TRUE)

# ANOVA comparison
CO.data.periodic$wday <- as.factor(CO.data.periodic$wday)
CO.daily.ANOVA <- f_anova_analyses(data = CO.data.periodic, formula = formula(log.CO.GT. ~ wday),
                                    truncate = TRUE)





#### NOx differences
### By time of day
NOx.data.periodic <- f_adjust_hourly(NOx.data, "log.NOx.GT.")
f_timeplot(NOx.data.periodic, x = "time", y = "rel.daily.target", color = "month", midpoint = 6)

# Bootsrap differences between hours
NOx.hourly.boot <- f_multilevel_one.boot(NOx.data.periodic, "time", "rel.daily.target")
f_multi_hist(NOx.hourly.boot, simplify = TRUE)

# Compare bootstrapped means
NOx.hourly.two.boot <- f_multilevel_two.boot(NOx.data.periodic, "time", "rel.daily.target")
f_multi_hist(NOx.hourly.two.boot, simplify = TRUE)

# ANOVA Comparison
NOx.data.periodic$time <- as.factor(NOx.data.periodic$time)
NOx.hourly.ANOVA <- f_anova_analyses(data = NOx.data.periodic, formula = formula(log.NOx.GT. ~ time),
                                    truncate = TRUE)


### By weekday
# Build dataset with daily averages
wday.averages <- f_adjust_daily(NOx.data.periodic, target.col = "log.NOx.GT.")
f_timeplot(wday.averages, x = "wday", y = "rel.daily.target", color = "week", midpoint = 26)

# Bootsrap differences between hours
NOx.daily.boot <- f_multilevel_one.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(NOx.daily.boot, simplify = TRUE)

# Compare bootstrapped means
NOx.daily.two.boot <- f_multilevel_two.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(NOx.daily.two.boot, simplify = TRUE)

# ANOVA Comparison
NOx.data.periodic$wday <- as.factor(NOx.data.periodic$wday)
NOx.daily.ANOVA <- f_anova_analyses(data = NOx.data.periodic, formula = formula(log.NOx.GT. ~ wday),
                                    truncate = TRUE)











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3) Effect of Benzene on NOx (VW scandal) ####

## NOx-Benzene measures
NOx.Benzene.data <- air.q.data %>% select(NOx.GT., C6H6.GT., T, RH, AH) %>%
  mutate(log.NOx.GT. = log(NOx.GT.)) %>%
  mutate(log.C6H6.GT. = log(C6H6.GT.)) %>%
  mutate(log.T.Kelvin = log((T + 273.15))) %>%
  mutate(log.RH = log(RH)) %>%
  mutate(log.AH = log(AH))


qplot(log.NOx.GT., log.C6H6.GT., color = log.T.Kelvin, data = NOx.Benzene.data)
qplot(log.NOx.GT., log.C6H6.GT., color = log.RH, data = NOx.Benzene.data)
qplot(log.NOx.GT., log.C6H6.GT., color = log.AH, data = NOx.Benzene.data)

# Model basic relationship
Benzene.NOx.model <- lm(log.NOx.GT. ~ log.C6H6.GT., data = NOx.Benzene.data)
summary(Benzene.NOx.model)
plot(Benzene.NOx.model)

# Add environmental variables (Temp, Humidity)
Benzene.NOx.Temp.model <- lm(log.NOx.GT. ~ log.C6H6.GT. + log.T.Kelvin + log.AH + log.RH, data = NOx.Benzene.data)
summary(Benzene.NOx.Temp.model)
plot(Benzene.NOx.Temp.model)

# Remove outliers & re-train model
Benzene.data.no.outliers <- f_remove_outliers(NOx.Benzene.data, c(1951, 1953, 3467, 3640, 3657, 4759, 7693, 8178))

Benzene.model.final <- lm(log.NOx.GT. ~ log.C6H6.GT. + log.T.Kelvin + log.AH + log.RH, data = Benzene.data.no.outliers)
summary(Benzene.model.final)
plot(Benzene.model.final)




