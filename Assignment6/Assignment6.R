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



#~~~~~~~~~~~~~~~~~~~~
#### Perform SVD ####
#~~~~~~~~~~~~~~~~~~~~

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Apply SVD to a model matrix created with model.matrix(), 
# and report the increase in dimensionality
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

auto.model.matrix <- model.matrix(model.formula, data = auto.data.ext)

auto.svd <- svd(auto.model.matrix)

d <- diag(auto.svd$d)

## Plot the singular vectors
plot.vec = function(u, n = 5){
  par(mfrow = c(n,1))
  par(mar=c(1,6,1,2))
  for(i in 1:n){
    barplot(u[,i])
    abline(h = 0, lwd = 2, col = 'blue')
  }
  par(mfrow = c(1,1))
}
plot.vec(auto.svd$u)
plot.vec(t(auto.svd$v))

## Plot the singular values
plot.sing = function(u){
  par(mar=c(5,5,5,5))
  nrows = nrow(u)
  d = rep(0,nrows)
  for(i in 1:nrows) d[i] = u[i,i]
  plot(1:nrows, d, col = 'red',
       main = ('Singular values'),
       xlab = 'Singular value order',
       ylab = 'Singular value')
}
plot.sing(d)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We have increased the number of dimensions from 12 to 46.
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# How many orthogonal features used for model
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


f_svd_coefficients <- function(df, svd, remove.features = NULL) {
  ## Compute the inverse the singular values
  dInv = diag(1/svd$d) 
  print(dInv)
  plot.sing(dInv)
  
  if(!is.null(remove.features)) {
    dInv[remove.features, remove.features] = 0.0
    print(dInv)
    plot.sing(dInv)
  }
  
  ## Compute the pseudo inverse
  pInv = svd$v %*% t(dInv) %*% t(svd$u)
  
  ## Compute the model coeficients
  b = pInv %*% as.matrix(df$log.price)
  
  return(b)
}

b <- f_svd_coefficients(auto.data.ext, auto.svd)

b.elim <- f_svd_coefficients(auto.data.ext, auto.svd, remove.features = c(44, 45, 46))

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We Drop 3 features from the model
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Evaluate performance with plots and RMS error
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

f_svd_predict <- function(df, df.matrix, model) {
  df$pred <- as.matrix(df.matrix) %*% model
  df$residual <- df$pred - df$log.price
  df$sq.res <- df$residual ^ 2
  
  return(df)
}

b.prediction <- f_svd_predict(auto.data.ext, auto.model.matrix, b)
b.elim.prediction <- f_svd_predict(auto.data.ext, auto.model.matrix, b.elim)

# RMSE
f_rmse <- function(residuals) {
  rmse <- sqrt(mean(residuals))
  return(rmse)
}

rmse.b <- f_rmse(b.prediction$sq.res)
rmse.b
rmse.b.elim <- f_rmse(b.elim.prediction$sq.res)
rmse.b.elim

plot.diagnostic = function(df){
  ## Plot the histogram and Q-Q of the residuals
  par(mfrow = c(1,2))
  hist(df$residual,
       main = 'Histogram of residuals',
       xlab = 'Model residuals')
  qqnorm(df$residual)
  par(mfrow = c(1,1))
  
  ## Plot the residuals vs the predicted values
  require(ggplot2)
  ggplot(df, aes(pred, residual)) +
    geom_point(size = 2, alpha = 0.3) + 
    ggtitle('Residuals vs predicted value') + 
    xlab('Predicted values') + ylab('Residual')
}

plot.diagnostic(b.prediction)
plot.diagnostic(b.elim.prediction)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Insight: We see that the performance of the model with dropped
# variables improves over the initial model by 23%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Stepwise regression to select features ####
# Compare model performance with full model using summary statistics, plots and ANOVA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(MASS)
## Apply step wise regression to the new model
lm.auto <- lm(model.formula, data = auto.data.ext)
auto.stepwise = stepAIC(lm.auto, direction = 'both')

auto.stepwise$anova
summary(auto.stepwise)
plot(auto.stepwise)

