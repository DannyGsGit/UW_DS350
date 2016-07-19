#### Load libraries ####
library(dplyr)
library(reshape2)
library(ggplot2)



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








#### Calculate log-price column ####

auto.data <- auto.data %>% mutate(log.price = log(price))


#### Test price and log-price for normality ####

f_normality_tests <- function(target) {
  ## This function will perform a Shapiro-Wilk's test and produce a qq-plot
  ## for a target column from a dataframe
  ##
  ## Input args: 
  ##    target- dataframe column to be tested
  ## Outputs:
  ##    Prints results of shapiro-wilk's test
  ##    Prints qqplot of target
  
  print(substitute(target))

  # Perform shapiro-wilk's normality test
  print(shapiro.test(target))
  
  # Produce qq-plot
  qqnorm(scale(target), pch=16)
  abline(0,1, lwd=2, col="red")
  
}

## Run Shapiro Wilk's test on target columns

f_normality_tests(auto.data$price)

f_normality_tests(auto.data$log.price)

## Note: log-price returns a more normal W value, but the small p-value << 0.05 indicates lack of normality.
## Looking at the qq-plot, we seen this occuring in the upper and lower extremities of pricing quartiles.






#### Test significance of price stratified on specific features ####

f_welch_t_test <- function(data, melt.id) {
  ## This function performs t-tests and produced boxplots on price and log-price of data stratified on a "melt.id".
  ## A Welch's t-test is used due to the following assumptions:
  ##    * Skewness appears in normality test, Welch's is more robust than Student's to skewness
  ##    * Two-tailed test as we are interested in difference in means between two groups, regardless of direction
  ##
  ## Input args:
  ##    data - input dataframe with "price" and "log.price" columns
  ##    melt.id - column name of stratification variable
  ## Outputs:
  ##    Print boxplots of price and logprice data
  ##    t.test.results- table of t.test results for price and logprice
  
  # Filter input dataframe down to the stratification variable and price/logprice columns, and melt into EAV format
  target.vars <- c("price", "log.price")
  stratified.auto.data <- data %>% melt(id = melt.id, na.rm = TRUE) %>% filter(variable %in% target.vars)
  colnames(stratified.auto.data) <- c("melt.var", "variable", "value")
  stratified.auto.data$value <- as.numeric(stratified.auto.data$value)
  
  # Perform t-test on price data
  price.data <- stratified.auto.data %>% filter(variable == "price")
  price.t.test <- t.test(value ~ melt.var, data = price.data)
  
  # Perform t-test on logprice data
  log.price.data <- stratified.auto.data %>% filter(variable == "log.price")
  log.price.t.test <- t.test(value ~ melt.var, data = log.price.data)
 
  # Generate boxplots for price and logprice data
  p.boxplot <- ggplot(stratified.auto.data, aes(melt.var, value)) +
    geom_boxplot() +
    facet_grid(variable ~ ., scales = "free") +
    labs(title = paste("Box plots of ", melt.id, sep = ""),
         x = melt.id)
  print(p.boxplot)
  
  
  # Combine t-test results into a single table

  t.test.results <- data.frame("comparison" = c("price", "log.price"),
                               "t" = c(price.t.test$statistic, log.price.t.test$statistic),
                               "p" = c(price.t.test$p.value, log.price.t.test$p.value),
                               "ci.95.low" = c(price.t.test$conf.int[1], exp(log.price.t.test$conf.int[1])),
                               "ci.95.hi" = c(price.t.test$conf.int[2], exp(log.price.t.test$conf.int[2])),
                               "mean.estimate.A" = c(price.t.test$estimate[1], exp(log.price.t.test$estimate[1])),
                               "mean.estimate.B" = c(price.t.test$estimate[2], exp(log.price.t.test$estimate[2])))
  
  # Set names on t.test.results to use the melt.id. This can't be done programatically when building the DF, so it's done
  # separately using the colnames function()
  groups <- levels(as.factor(stratified.auto.data$melt.var))
  
  colnames(t.test.results) <- c("comparison", "t", "p",
                                "ci.95.lo", "ci.95.hi",
                                paste("mean.estimate.", groups[1], sep = ""),
                                paste("mean.estimate.", groups[2], sep = ""))
  
  # Return results table
  return(t.test.results)
}


# Perform t-test on fuel type, aspiration, and drive wheels variables
f_welch_t_test(data = auto.data, melt.id = "fuel.type")

f_welch_t_test(data = auto.data, melt.id = "aspiration")

drive.wheels.data <- auto.data %>% filter(drive.wheels != "4wd") %>% droplevels()
f_welch_t_test(data = drive.wheels.data, melt.id = "drive.wheels")
