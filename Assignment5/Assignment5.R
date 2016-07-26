# Goal: Construct and evaluate a linear model of automotive price. 
# 
# Model price by engine size, curb weight, and city mpg 
#     –Hint1:  the R model formula is something like: price ~ engine.size+ curb.weight+ city.mpg 
#     –Hint 2: a transformation of either the label (dependent variable) or the features (independent variables) is required.   
# Evaluate the significance of the model coefficients from the model summary 
# Evaluate the performance of the model fit using boththe diagnostic plots and the model summary. 
# Test normality of residuals (e,g, SW test)


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

## Insights: It appears that we want to transform both price and engine
#             size.

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

## Insights: Between QQ plots and paired scatter plots, we will indeed
#             transform price and engine size for linear relationships.





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

## Insights:
# All coefficients are statistically significant
# Note that log-price ranges from 8.541-10.72

# While engine size is significant, the coefficient is very small given the scale of the feature (range ~ 1.5)
# The range of engine size only explains ~ 0.00615 of the price. 
# By contrast, curb.weight will cover ~1.1 and city.mpg covers ~ 0.63





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

## Insights:
# We see a pretty good r^2 value of 0.847 with a RMSE of 0.198 on
# a range of ~2.18 (~9%). 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Test normality of residuals ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Plot actual vs. predicted values
qplot(log.price, prediction, data = auto.data)
qplot(log.price, residuals, data = auto.data)
qplot(prediction, residuals, data = auto.data)

# Normality tests of residuals
qqPlot(auto.data$residuals)
shapiro.test(auto.data$residuals)

# Review metric plots
plot(price.model)

## Insights:
# Initial visual inspection appears to indicate homoscedasticity
# As we look at the qqplot, it appears that we have some non-normality, as supported by the SW test
# We see a few points highlighted as outliers in the fitted-residual plot.
# Looking at the leverage plot, we see that our outliers have high residual and moderate leverage.





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Refit model without outliers ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outliers <- c(120:122)
auto.data.no.outliers <- auto.data[-outliers, ]

price.model.no.outliers <- lm(formula = model.formula, data = auto.data.no.outliers)
summary(price.model.no.outliers)
# The re-fitted model shows a mild improvement in R^2

plot(price.model.no.outliers)
# Plot results show improvement in normality of residuals