# –Apply ANOVA to the auto price data: 
#   >Compare the price (log price) of autos for several multi-valued categorical variables 
#     –number of doors, body style, drive wheels, number of cylinders, engine type 
#   >Graphically explore the differences 
#     –Hint, make sure you have enough data for each category. 
#   >Use standard ANOVA and Tukey ANOVA in R 
#   >Use the bootstrap distribution CIs of the (differences of) means 
#     – Hint write a function for to perform this calculation for any number of categories (levels) pairs



#### Load libraries ####
library(dplyr)
library(reshape2)
library(ggplot2)



#### Load automobile dataset ####
auto.data <- read.csv('Automobile price data _Raw_.csv', stringsAsFactors = FALSE)






#### Select proper columns and set format ####

## Set types on factor columns
factor.cols <- c("make", "fuel.type", "aspiration", "num.of.doors",
                 "body.style", "drive.wheels", "engine.location",
                 "engine.type", "num.of.cylinders", "fuel.system")
auto.data[, factor.cols] <- lapply(auto.data[factor.cols], as.factor)

## Set types on numeric columns
numeric.cols <- c("wheel.base", "length", "width", "height",
                  "curb.weight", "engine.size", "bore", "stroke",
                  "compression.ratio", "horsepower", "peak.rpm",
                  "city.mpg", "highway.mpg", "price")
auto.data[, numeric.cols] <- lapply(auto.data[numeric.cols], as.numeric)






#### Calculate log-price column ####
auto.data <- auto.data %>% mutate(log.price = log(price))






#### Graphically compare variables ####

## Graphical analysis function
f_graph_analysis <- function(data, dependent, independent) {
  
  # Select columns and remove rows with blanks
  plot.data <- data[, c(dependent, independent)]
  plot.data <- plot.data[complete.cases(plot.data), ]
  
  colnames(plot.data) <- c("dependent", "independent")
  
  # Re-order by median price
  median.plot.data <- aggregate(dependent ~ independent, plot.data, median)
  
  plot.data$independent <- factor(plot.data$independent,
                                  levels = median.plot.data[order(median.plot.data$dependent), "independent"])
  
  # Build the boxplot
  p.boxplot <- ggplot(plot.data, aes(independent, dependent)) +
    geom_boxplot() +
    labs(title = paste("Box plot of ", independent, " vs. ", dependent, sep =),
         x = independent, y = dependent)
  print(p.boxplot)
  
  # Plot number of observations of each level
  p.counts <- ggplot(plot.data, aes(independent)) +
    geom_bar() +
    labs(title = paste("Counts of ", independent, sep =""),
         x = independent, y = "count")
  print(p.counts)
}

## Make
f_graph_analysis(data = auto.data, dependent = "log.price", "make")

## Body Style
f_graph_analysis(data = auto.data, dependent = "log.price", "body.style")

## Drive Wheels
f_graph_analysis(data = auto.data, dependent = "log.price", "drive.wheels")

## Cylinder count
f_graph_analysis(data = auto.data, dependent = "log.price", "num.of.cylinders")

## Engine Type
f_graph_analysis(data = auto.data, dependent = "log.price", "engine.type")
qplot(auto.data$engine.type, auto.data$make)







#### ANOVA ####

# ANOVA Functions
f_anova_analyses <- function(data, formula) {
  # Calculate basic ANOVA
  anova.df <- aov(formula, data = data)
  # Print ANOVA table
  print(summary(anova.df))
  # Set a 2x2 plot layout and print diagnostic plots
  layout(matrix(c(1,2,3,4),2,2))
  plot(anova.df)
  
  # Calculate Tukey ANOVA
  tukey.df <- TukeyHSD(anova.df)
  # Print Tukey table
  print(tukey.df)
  # Reset plot layout to 1x1 and print Tukey plot
  layout(matrix(c(1),1,1))
  plot(tukey.df)
}


# Make
f_anova_analyses(auto.data, formula(log.price ~ make))

# Body style
f_anova_analyses(auto.data, formula(log.price ~ body.style))

# Number of cylinders
f_anova_analyses(auto.data, formula(log.price ~ num.of.cylinders))

# Engine type
f_anova_analyses(auto.data, formula(log.price ~ engine.type))


#### Bootstrap means ####

