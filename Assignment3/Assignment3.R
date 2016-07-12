#### Load libraries ####
library(dplyr)
library(reshape2)



#### Load automobile dataset ####
auto.data <- read.csv('Automobile price data _Raw_.csv', stringsAsFactors = FALSE)





#### Select proper columns and set format ####

## Select columns we will use for stratification
auto.data <- auto.data %>% select(price, fuel.type, aspiration, drive.wheels)

## Set types on factor columns
factor.cols <- c("fuel.type", "aspiration", "drive.wheels")
auto.data[, factor.cols] <- lapply(auto.data[factor.cols], as.factor)

## Set types on numeric columns
numeric.cols <- c("price")
auto.data[, numeric.cols] <- lapply(auto.data[numeric.cols], as.numeric)





#### Create log-price column ####
auto.data <- auto.data %>% mutate(log.price = log(price))





#### Test price and log-price for normality ####

## To DO: Pass through names for plots and normality results

f_normality_tests <- function(target) {
  ## Document function here ##

  print(shapiro.test(target))
  qqnorm(scale(target), pch=16)
  abline(0,1, lwd=2, col="red")
  
}

## Run Shapiro Wilk's test on target columns
test.cols <- c("price", "log.price")
lapply(auto.data[test.cols], f_normality_tests)
## Note: log-price returns a more normal W value, but the small p-value << 0.05 indicates lack of normality.
## Looking at the qq-plot, we seen this occuring in the upper and lower extremities of pricing quartiles.






#### Test significance of price stratified on specific features ####

## Note that Welch's t-test is more robust than Student's to skewed distributions. Since we are not normal,
## it is a good test to use to compare pricing means. Welch is the default implementation of t.test().


f_welch_ttest <- function(data, melt.id, target.var) {
  stratified.auto.data <- data %>% melt(id = melt.id, na.rm = TRUE) %>% filter(variable == target.var)
  colnames(stratified.auto.data) <- c("melt.var", "variable", "value")
  stratified.auto.data$value <- as.numeric(stratified.auto.data$value)
  
  print(paste("Stratification variable: ", melt.id, sep = ""))
  print(paste("Target value: ", target.var, sep = ""))
  t.test(value ~ melt.var, data = stratified.auto.data)
}


f_welch_ttest(data = auto.data, melt.id = "fuel.type", target.var = "price")
f_welch_ttest(data = auto.data, melt.id = "fuel.type", target.var = "log.price")


f_welch_ttest(data = auto.data, melt.id = "aspiration", target.var = "price")
f_welch_ttest(data = auto.data, melt.id = "aspiration", target.var = "log.price")


drive.wheels.data <- auto.data %>% filter(drive.wheels != "4wd")
f_welch_ttest(data = drive.wheels.data, melt.id = "drive.wheels", target.var = "price")
f_welch_ttest(data = drive.wheels.data, melt.id = "drive.wheels", target.var = "log.price")