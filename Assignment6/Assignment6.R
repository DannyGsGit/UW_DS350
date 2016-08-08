#### Assignment 6 ####
# Submitted By: Danny Godbout

# –Perform SVD regression on the auto price data 
# >Use the following features for your initial model: 
# make, fuel.type, aspiration, body.style, drive.wheels, 
# length, curb.weight, engine.type, num.of.cylinders, engine.size, 
# city.mpg 
# 
# >Apply SVD to a model matrix created with model.matrix(), 
# and report the increase in dimensionality 
# 
# >Report how many orthogonal 
# features you used for you model 
#
# >Evaluate your model performance with 
# plots and by computing RMS error. Hint see my demo code for plots. 
# 
# –Use stepwise regression to select features from the aforementioned set 
# >Compare model performance with full model using summary statistics, plots and ANOVA


library(dplyr)
library(ggplot2)
library(car)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  Import and set typing + basic calculated columns ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# Calculate log-price column
auto.data <- auto.data %>% mutate(log.price = log(price))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  Set model features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

model.features <- c("make", "fuel.type", "aspiration", "body.style",
                    "drive.wheels", "length", "curb.weight", "engine.type",
                    "num.of.cylinders", "engine.size", "city.mpg", "log.price")

auto.data.ext <- na.exclude(auto.data[, model.features])

model.formula <- log.price ~ 0 + make + fuel.type + aspiration + body.style +
  drive.wheels + length + curb.weight + engine.type + num.of.cylinders +
  engine.size + city.mpg


# Standardize numeric columns
numeric.features <- c("length", "curb.weight", "engine.size", "city.mpg")

auto.data.ext[, numeric.features] <- scale(auto.data.ext[, numeric.features])


## Select modeling columns
model.features <- c("make", "fuel.type", "aspiration", "body.style",
                    "drive.wheels", "length", "curb.weight", "engine.type",
                    "num.of.cylinders", "engine.size", "city.mpg", "log.price")

model.data <- auto.data[, model.features]


## Remove NAs
model.data <- na.exclude(model.data)


## Standardize numeric columns
numeric.features <- c("length", "curb.weight", "engine.size", "city.mpg")

model.data[, numeric.features] <- scale(model.data[, numeric.features])


#~~~~~~~~~~~~~~~~~~~~
#### Perform SVD ####
#~~~~~~~~~~~~~~~~~~~~

## Set formula to predict log price on all features. Remove intercept with 0:
model.formula <- log.price ~ 0 + .

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Apply SVD to a model matrix created with model.matrix(), 
# and report the increase in dimensionality
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
## Build the model matrix from formula
auto.model.matrix <- model.matrix(model.formula, data = model.data)

## Perform SVD on model matrix
auto.svd <- svd(auto.model.matrix)

## Save the d-matrix
d <- diag(auto.svd$d)

## Plot singular values
f_singular_value_plot <- function(values) {
  ## Plots singular values of a matrix diagonal.
  ## Args:
  ##  values- matrix of values
  
  # Get list of diagonal values and format for plotting
  val_list <- data.frame("value" = diag(values), "index" = 1:length(diag(values)))
  
  # Build plot
  sv_plot <- ggplot(val_list, aes(index, value)) +
    geom_point(size = 2.5) +
    ggtitle("Singular Value Plot")
  print(sv_plot)
  
}
f_singular_value_plot(d)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We have increased the number of dimensions from 12 to 46.
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# How many orthogonal features used for model
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


f_svd_coefficients <- function(df, svd, remove.features = NULL) {
  ## Function computes the SVD model coefficients
  ## Args:
  ##  df- data set to model on
  ##  svd- an svd object
  ##  remove.features- list of features to remove
  
  ## Compute the inverse the singular values
  dInv = diag(1/svd$d) 
  print(dInv)
  f_singular_value_plot(dInv)
  
  ## If features have been chosen for removal, set to 0
  if(!is.null(remove.features)) {
    dInv[remove.features, remove.features] = 0.0
    print(dInv)
    f_singular_value_plot(dInv)
  }
  
  ## Compute the pseudo inverse
  pInv = svd$v %*% t(dInv) %*% t(svd$u)
  
  ## Compute the model coeficients
  b = pInv %*% as.matrix(df$log.price)
  
  return(b)
}

## Calculate model coefficients
b <- f_svd_coefficients(model.data, auto.svd)

## Re-calculate model coefficients, with features 44-46 removed
b.elim <- f_svd_coefficients(model.data, auto.svd, remove.features = c(44, 45, 46))

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We Drop 3 features from the model
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Evaluate performance with plots and RMS error
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

## Evaluate model performance with and without dropped features
f_svd_predict <- function(df, df.matrix, model) {
  # Prediction
  df$pred <- as.matrix(df.matrix) %*% model
  
  # Residuals
  df$residual <- df$pred - df$log.price
  
  # Square of residuals
  df$sq.res <- df$residual ^ 2
  
  return(df)
}

b.prediction <- f_svd_predict(auto.data.ext, auto.model.matrix, b)
b.elim.prediction <- f_svd_predict(auto.data.ext, auto.model.matrix, b.elim)

## RMSE
f_rmse <- function(residuals) {
  rmse <- sqrt(mean(residuals))
  return(rmse)
}

rmse.b <- f_rmse(b.prediction$sq.res)
rmse.b
rmse.b.elim <- f_rmse(b.elim.prediction$sq.res)
rmse.b.elim

## Print diagnostic plots
plot.diagnostic = function(df){

    ## Histogram of residuals
  hist_residuals <- ggplot(df, aes(residual)) +
    geom_histogram(bins = 30) +
    ggtitle("Histogram of Residuals")
  print(hist_residuals)
  
  ## QQ Plot
  qqPlot(df$residual, main = "QQ Plot of Residuals")
  
  ## Residuals-Predicted
  residual_predicted_plot <- ggplot(df, aes(pred, residual)) +
    geom_point() +
    labs(title = "Residuals vs Predicted Values", x = "Predicted", y = "Actual")
  print(residual_predicted_plot)
  
}

plot.diagnostic(b.prediction)
plot.diagnostic(b.elim.prediction)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We see that the performance of the model with dropped
# variables improves over the initial model by 23%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Stepwise regression to select features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compare model performance with full model using summary statistics, plots and ANOVA

library(MASS)
## Apply step wise regression to the new model
lm.auto <- lm(model.formula, data = auto.data.ext)
auto.stepwise = stepAIC(lm.auto, direction = 'both')

auto.stepwise$anova
summary(auto.stepwise)
plot(auto.stepwise)

