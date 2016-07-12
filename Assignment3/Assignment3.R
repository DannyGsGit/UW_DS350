#### Load libraries ####
library(dplyr)



#### Load automobile dataset ####
auto.data <- read.csv('Automobile price data _Raw_.csv')


#### Select proper columns and set format ####

auto.data <- auto.data %>% select(price, fuel.type, aspiration, drive.wheels)

## Set types on factor columns
factor.cols <- c("fuel.type", "aspiration", "drive.wheels")
auto.data[, factor.cols] <- lapply(auto.data[, factor.cols], as.factor)

## Set types on numeric columns
# Intermediate conversion to character column, since as.numeric() on a factor
# will return the numeric factor level rather than factor value.
numeric.cols <- c("price")
auto.data[, numeric.cols] <- lapply(auto.data[, numeric.cols], as.character)
auto.data[, numeric.cols] <- lapply(auto.data[, numeric.cols], as.numeric)

#### Create log-price column ####
auto.data <- auto.data %>% filter(price != "?")
auto.data$price <- as.numeric(as.character(auto.data$price))

auto.data <- auto.data %>% mutate(log.price = log(price))
