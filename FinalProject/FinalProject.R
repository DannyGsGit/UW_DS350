# Final Project

library(lubridate)
library(dplyr)
library(ggplot2)
library(car)
library(simpleboot)

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

# Add time labels
CO.data$time <- hour(CO.data$timestamp)
CO.data$wday <- wday(CO.data$timestamp)
CO.data$day <- day(CO.data$timestamp)
CO.data$week <- week(CO.data$timestamp)
CO.data$month <- month(CO.data$timestamp)
CO.data$year <- year(CO.data$timestamp)

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

# Add time labels
NOx.data$time <- hour(NOx.data$timestamp)
NOx.data$wday <- wday(NOx.data$timestamp)
NOx.data$day <- day(NOx.data$timestamp)
NOx.data$week <- week(NOx.data$timestamp)
NOx.data$month <- month(NOx.data$timestamp)
NOx.data$year <- year(NOx.data$timestamp)

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
C6H6.CO$week <- week(C6H6.CO$timestamp)
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

#### Functions

f_adjust_hourly <- function(data, target.col) {
  
  data$target <- data[,target.col]
  
  # Get daily means
  daily.averages <- data %>% select(day, month, year, target) %>%
    group_by(day, month, year) %>% 
    summarise(daily.mean = mean(target)) %>%
    ungroup()
  
  # Merge daily means back into dataset
  data <- merge(data, daily.averages, by = c("day", "month", "year"))
  
  # Calculate relative CO concentration
  data <- data %>% mutate(rel.daily.target = target - daily.mean)
  
  return(data)
}

f_adjust_daily <- function(data, target.col) {
  data$target <- data[,target.col]
  
  # Build dataset with daily averages
  wday.averages <- data %>% select(wday, week, year, target) %>%
    group_by(wday, week, year) %>%
    summarise(daily.mean = mean(target)) %>%
    ungroup()
  
  # Get daily means
  weekly.averages <- wday.averages %>% select(week, year, daily.mean) %>%
    group_by(week, year) %>%
    summarise(weekly.mean = mean(daily.mean)) %>%
    ungroup()
  
  # Merge daily means back into dataset
  wday.averages <- merge(wday.averages, weekly.averages, by = c("week", "year"))
  
  # Calculate relative CO concentration
  wday.averages <- wday.averages %>% mutate(rel.daily.target = daily.mean - weekly.mean)
  
  return(wday.averages)
}

f_timeplot <- function(data, x, y, color, alpha = 0.5, midpoint) {
  data$fun.x <- data[,x]
  data$fun.y <- data[,y]
  data$fun.color <- data[,color]
  
  g.plot <- ggplot(data, aes(jitter(fun.x), fun.y)) +
    geom_point(aes(color = fun.color, alpha = alpha, size = 1)) +
    scale_color_gradient2(mid = "red", high = "blue", low = "blue", midpoint = midpoint) +
    geom_smooth()
  
  print(g.plot)
}

f_multilevel_one.boot <- function(data, independent, dependent, type = "mean", R.count = 10000) {
  ## Description: This function generates bootstrap samples of each level
  ##              of an independent variable (e.g. "ford" and "gm" levels of variable "make").
  ## Arguments:
  ##    data: dataframe
  ##    independent: independent variable column name
  ##    dependent: dependent variable column name
  ##    type: mean or median bootsrap
  ##    R.count: number of bootstrap samples
  
  # Break out target levels
  if(class(data[, independent]) != "factor") {
    data[,independent] <- as.factor(data[,independent])
  }
  
  boot.levels <- levels(data[, independent])
  
  # Bootstrap means/medians of each level of independent variable
  boots <- data.frame()  # Pre-allocate results dataframe
  for (i in 1:length(boot.levels)) {
    current.level.data <- data[which(data[, independent] == boot.levels[i]), ]  # Data for current level of independent variable
    boot.level <- one.boot(current.level.data[, dependent], substitute(type), R = R.count)  # Bootstrap R.count iterations of mean/median of current data
    boots <- rbind(boots, t(boot.level$t))  # Append results to boots dataframe
  }
  
  # Re-orient results and name columns with independent variable levels
  boots <- as.data.frame(t(boots))
  colnames(boots) <- boot.levels
  
  return(boots)
}

f_multilevel_two.boot <- function(data, independent, dependent, type = "mean", R.count = 10000) {
  ## Description: This function generates bootstrap samples of DIFFERENCES between each level
  ##              of an independent variable (e.g. "ford" and "gm" levels of variable "make").
  ## Arguments:
  ##    data: dataframe
  ##    independent: independent variable column name
  ##    dependent: dependent variable column name
  ##    type: mean or median bootsrap
  ##    R.count: number of bootstrap samples
  
  
  # List out all levels of independent variable
  if(class(data[, independent]) != "factor") {
    data[,independent] <- as.factor(data[,independent])
  }
  
  boot.levels <- levels(data[, independent])
  
  # Generate all combinations of levels
  # Example:
  #     1    2    3
  # 1  1,1  1,2  1,3
  # 2  2,1  2,2  2,3
  # 3  3,1  3,2  3,3
  boot.combinations <- expand.grid(boot.levels, boot.levels)
  
  # Remove comparisons of a level to itself
  # Example: Remove 1,1  2,2  and 3,3
  boot.combinations <- boot.combinations[which(boot.combinations$Var1 != boot.combinations$Var2),]
  
  # Generate numeric columns from factor levels for easy manipulation
  boot.combinations$Num1 <- as.numeric(boot.combinations$Var1)
  boot.combinations$Num2 <- as.numeric(boot.combinations$Var2)
  # Use numeric values to remove duplicate, but reversed comparisons
  # Example: 1,2 and 2,1 are the same. Note that in the upper half of the diagonal matrix, the
  #          row number is always lower than the column number. This makes a simple filtering rule:
  #     1     2      3
  # 1  1,1  [1,2]  [1,3]
  # 2  2,1   2,2   [2,3]
  # 3  3,1   3,2    3,3
  boot.combinations <- boot.combinations[which(boot.combinations$Num1 < boot.combinations$Num2), ]
  # Remove numeric columns now that we're done filtering.
  boot.combinations <- boot.combinations %>% select(Var1, Var2)
  
  # Generate bootstraps for the pairs.
  boots <- data.frame()
  for (i in 1:nrow(boot.combinations)) {
    sample.a <- data[which(data[, independent] == boot.combinations$Var1[i]), ]
    sample.b <- data[which(data[, independent] == boot.combinations$Var2[i]), ]
    
    two.boot.temp <- two.boot(sample.a[, dependent], sample.b[, dependent], substitute(type), R = R.count)
    
    boots <- rbind(boots, t(two.boot.temp$t))
  }
  
  # Re-orient results and add column names
  boots <- as.data.frame(t(boots))
  colnames(boots) <- paste(boot.combinations$Var1, boot.combinations$Var2, sep = ".")
  
  return(boots)
}

plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  ## Description: Histogram plotting function copied from DS350 course code. 
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

f_multi_hist <- function(data, simplify = FALSE, nbins = 80, p = 0.05,
                         plot.title = "Histogram", y.label = "mean target", x.label = "Level"){
  ## Description: Builds multiple histograms of bootstrap results for variables with multiple levels.
  ##              There is a "simplify" option to collapse histograms into simpler point-ranges for 
  ##              cases where too many levels (more than ~5) generate an overwhelming number of histograms.
  ##
  ## Arguments:
  ##    data: Bootstrap results
  ##    simplify: Generates point-ranges instead of histograms when TRUE
  ##    nbins: Number of histogram bins
  ##    p: Confidence interval threshold
  ##    plot.title: Plot title
  ##    y.label: X label
  ##    x.label: Y label
  
  # Get max and min values for plot limits
  maxs = max(data)
  mins = min(data)
  
  
  # Show histograms when simplify == FALSE
  if (simplify == FALSE) {
    # Use par to merge multiple plots into a single plot window. Use ncol(data) to make as many plots as there are levels.
    par(mfrow = c(ncol(data), 1))
    # Generate each individual plot and add to the plot layout initiated in the previous line.
    for (i in 1:ncol(data)) {
      plot.hist(data[, i], maxs, mins, cols = colnames(data)[i])
    }
    title(plot.title, outer = TRUE, cex.main = 2)
    # Reset plot window to 1x1 for future plots
    par(mfrow = c(1, 1))
  }
  
  
  # Condense into pointrange plots when simplify == TRUE
  if (simplify == TRUE) {
    # Calculate mean, upr, lwr values
    mean <- apply(data, 2, mean)
    upr <- apply(data, 2, function(x) quantile(x, probs = 1 - p/2))
    lwr <- apply(data, 2, function(x) quantile(x, probs = p/2))
    # Combine basic stats into a dataframe
    plot.data <- data.frame("name" = colnames(data), mean, upr, lwr)
    # Make name column a factor type, and order factor levels by their means for ordered plotting
    plot.data$name <- factor(plot.data$name, levels = plot.data$name[order(plot.data$mean)])
    
    # Generate pointrange plot of bootstrap results
    p.multi <- ggplot(plot.data, aes(x = name, y = mean, ymin = lwr, ymax = upr)) +
      geom_pointrange() +
      coord_flip() +
      geom_hline(yintercept = 0) +
      lims(y = c(min(plot.data$lwr), max(plot.data$upr))) +
      labs(title = plot.title, y = y.label, x = x.label)
    print(p.multi)
  }
}


#### CO Differences

### By time of day
CO.data <- f_adjust_hourly(CO.data, "log.CO.GT.")
f_timeplot(CO.data, x = "time", y = "rel.daily.target", color = "month", midpoint = 6)

# Bootsrap differences between hours
CO.hourly.boot <- f_multilevel_one.boot(CO.data, "time", "rel.daily.target")
f_multi_hist(CO.hourly.boot, simplify = TRUE)

# Compare bootstrapped means
CO.hourly.two.boot <- f_multilevel_two.boot(CO.data, "time", "rel.daily.target")
f_multi_hist(CO.hourly.two.boot, simplify = TRUE)


### By weekday
wday.averages <- f_adjust_daily(CO.data, target.col = "log.CO.GT.")
f_timeplot(wday.averages, x = "wday", y = "rel.daily.target", color = "week", midpoint = 26)

# Bootsrap differences between hours
CO.daily.boot <- f_multilevel_one.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(CO.daily.boot, simplify = TRUE)

# Compare bootstrapped means
CO.daily.two.boot <- f_multilevel_two.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(CO.daily.two.boot, simplify = TRUE)







#### NOx differences
### By time of day
NOx.data <- f_adjust_hourly(NOx.data, "log.NOx.GT.")
f_timeplot(NOx.data, x = "time", y = "rel.daily.target", color = "month", midpoint = 6)

# Bootsrap differences between hours
NOx.hourly.boot <- f_multilevel_one.boot(NOx.data, "time", "rel.daily.target")
f_multi_hist(NOx.hourly.boot, simplify = TRUE)

# Compare bootstrapped means
NOx.hourly.two.boot <- f_multilevel_two.boot(NOx.data, "time", "rel.daily.target")
f_multi_hist(NOx.hourly.two.boot, simplify = TRUE)


### By weekday
# Build dataset with daily averages
wday.averages <- f_adjust_daily(NOx.data, target.col = "log.NOx.GT.")
f_timeplot(wday.averages, x = "wday", y = "rel.daily.target", color = "week", midpoint = 26)

# Bootsrap differences between hours
NOx.daily.boot <- f_multilevel_one.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(NOx.daily.boot, simplify = TRUE)

# Compare bootstrapped means
NOx.daily.two.boot <- f_multilevel_two.boot(wday.averages, "wday", "rel.daily.target")
f_multi_hist(NOx.daily.two.boot, simplify = TRUE)
