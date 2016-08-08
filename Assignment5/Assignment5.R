#### Assignment 5 ####
## Prepared By: Danny Godbout
## Date: 7/26/2015

library(dplyr)
library(car)
library(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Import & Format Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import
auto.data <- read.csv('Automobile price data _Raw_.csv', stringsAsFactors = FALSE)

# Set types on factor columns
factor.cols <- c("make", "fuel.type", "aspiration", "num.of.doors",
                 "body.style", "drive.wheels", "engine.location",
                 "engine.type", "num.of.cylinders", "fuel.system")
auto.data[, factor.cols] <- lapply(auto.data[factor.cols], as.factor)

# Set types on numeric columns
numeric.cols <- c("wheel.base", "length", "width", "height",
                  "curb.weight", "engine.size", "bore", "stroke",
                  "compression.ratio", "horsepower", "peak.rpm",
                  "city.mpg", "highway.mpg", "price")
auto.data[, numeric.cols] <- lapply(auto.data[numeric.cols], as.numeric)

# Transform price column
auto.data <- auto.data %>% mutate(log.price = log(price))

# Transform engine size column
auto.data <- auto.data %>% mutate(log.engine.size = log(engine.size))

# Remove rows with NAs
auto.data <- na.omit(auto.data)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Review Model Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check for normality of individual columns
qqPlot(auto.data$price, main = "Price")
qqPlot(auto.data$log.price, main = "Log-Price")

qqPlot(auto.data$engine.size, main = "Engine Size")
qqPlot(auto.data$log.engine.size, main = "Log Engine Size")

qqPlot(auto.data$curb.weight, main = "Curb Weight")

qqPlot(auto.data$city.mpg, main = "City MPG")



# Check linearity of relationships between features and response (& log-response)
f_plot_pairs <- function(independent, dependent1, dependent2){
  par(mfrow=c(2,1))
  plot(independent, dependent1,
       main = "Scatterplot of feature",
       xlab = substitute(independent)[3],
       ylab = substitute(dependent1)[3])
  plot(independent, dependent2,
       xlab = substitute(independent)[3],
       ylab = substitute(dependent2)[3])
  par(mfrow=c(1,1))
  print(substitute(independent))
}

f_plot_pairs(auto.data$engine.size, auto.data$price, auto.data$log.price)

f_plot_pairs(auto.data$log.engine.size, auto.data$price, auto.data$log.price)

f_plot_pairs(auto.data$curb.weight, auto.data$price, auto.data$log.price)

f_plot_pairs(auto.data$city.mpg, auto.data$price, auto.data$log.price)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Model Price by Engine Size, Weight, and City MPG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define model formula
model.formula <- formula(log.price ~ log.engine.size + curb.weight + city.mpg)

# Train linear regression using training data
price.model <- lm(formula = model.formula, data = auto.data)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Evaluate the significance of the model coefficients ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Review the summary of the model
summary(price.model)

# For context, also review the modeling data
summary(auto.data[, c("log.price", "log.engine.size",
                      "curb.weight", "city.mpg")])







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Evaluate model performance ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prediction
auto.data$prediction <- predict(price.model)

# Calculate residuals, squared residuals, and squared distances to means
auto.data <- auto.data %>%
  mutate(residuals = log.price - prediction) %>%
  mutate(squared.residuals = residuals ^ 2) %>%
  mutate(squared.mean.distance = (log.price - mean(auto.data$log.price)) ^ 2)

# R-Squared
r.squared <- 1 - (sum(auto.data$squared.residuals) / sum(auto.data$squared.mean.distance))

# RMSE
rmse <- sqrt(mean(auto.data$squared.residuals))






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Test normality of residuals ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Normality tests of residuals
qqPlot(auto.data$residuals)
shapiro.test(auto.data$residuals)

# Review metric plots
plot(price.model)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Refit model without outliers ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove outliers from dataset
outliers <- c(120:122)
auto.data.no.outliers <- auto.data[-outliers, ]

# Fit model to cleansed data
price.model.no.outliers <- lm(formula = model.formula, data = auto.data.no.outliers)
summary(price.model.no.outliers)

# Review diagnostic plots of new model
plot(price.model.no.outliers)

# Check normality of residuals
auto.data.no.outliers$prediction <- predict(price.model.no.outliers)
auto.data.no.outliers <- auto.data.no.outliers %>% mutate(residuals = log.price - prediction)
shapiro.test(auto.data.no.outliers$residuals)