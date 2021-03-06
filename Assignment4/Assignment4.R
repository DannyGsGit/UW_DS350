#### Load libraries ####
library(dplyr)
library(reshape2)
library(ggplot2)
library(lazyeval)
library(HistData)
library(resample)
library(simpleboot)
library(knitr)
library(RColorBrewer)




######################################################################
##  Import and set typing + basic calculated columns
######################################################################

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






######################################################################
##  Define Functions
######################################################################


#### Graphical analysis function ####

f_graph_analysis <- function(data, dependent, independent) {
  ## Description: This function takes a dataset with dependent and independent
  ##              variables, and generates a boxplot with independent variables
  ##              ordered by the median of the dependent. Also plots a bar chart
  ##              of counts of each independent variable level.
  ## Arguments:
  ##    data: A dataframe for plotting
  ##    dependent: character object describing dependent variable column name
  ##    independent: character object describing independent variable column name
  
  # Select columns of interest and remove rows with blanks
  plot.data <- data[, c(dependent, independent)]
  plot.data <- plot.data[complete.cases(plot.data), ]
  
  # Provide generic column names to standardize analysis pipelines
  colnames(plot.data) <- c("dependent", "independent")
  
  # Re-order independent variable factor levels by median price (dependent variable)
  median.plot.data <- aggregate(dependent ~ independent, plot.data, median)
  plot.data$independent <- factor(plot.data$independent,
                                  levels = median.plot.data[order(median.plot.data$dependent), "independent"])
  
  # Build a boxplot of independent-dependent values
  p.boxplot <- ggplot(plot.data, aes(independent, dependent)) +
    geom_boxplot() +
    labs(title = paste("Box plot of ", independent, " vs. ", dependent, sep =),
         x = independent, y = dependent)
  print(p.boxplot)
  
  # Plot number of observations of each level of independent variable
  p.counts <- ggplot(plot.data, aes(independent)) +
    geom_bar() +
    labs(title = paste("Counts of ", independent, sep =""),
         x = independent, y = "count")
  print(p.counts)
  
}








#### Filtering function ####

f_remove_levels <- function(data, target.column, remove.levels = NULL, na.columns = "log.price") {
  ## Description: This function removes any levels of an independent variable that
  ##              we want to eliminate before ANOVA or bootstrap analysis. Also removes
  ##              any rows where the dependent variable has a value of NA.
  ##
  ## Arguments:
  ##    data: input data frame
  ##    target.column: column to massage
  ##    remove.levels: any levels of the target column we want filtered out (e.g. if a level has too few samples to model)
  ##    na.columns: columns to remove NA values from.
  
  # If remove.levels has been defined, build an index of rows in data that contain those 
  # values and remove them
  if (!is.null(remove.levels)) {
    remove.index <- which(data[, target.column] %in% remove.levels)
    data <- data[-remove.index, ]
  }
  
  # Remove rows containing an NA in na.columns
  data <- data[!is.na(data[, na.columns]), ]
  
  # Drop unused factor levels made extinct in the above processing
  data <- droplevels(data)
  
  return(data)
}




## ANOVA function
f_anova_analyses <- function(data, formula, truncate = FALSE, suppress.diag.plot = TRUE) {
  ## Description: Takes dataset and formula and calculates ANOVA and Tukey HSD. 
  ##              Returns ANOVA and Tukey tables and plots.
  ## Arguments:
  ##    data: Dataframe for analysis
  ##    formula: A formula-type object describing the model. ex: formula(y ~ x)
  ##    truncate: Filter printed results to only show significant (p<0.05) results. Use when too many levels are presented to be meaningful.
  ##    suppress.diag.plot: Suppresses output of diagnostic plots from ANOVA. Suppress in reporting when no diagnostic issues are known.
  
  
  # Calculate basic ANOVA
  anova.df <- aov(formula, data = data)
  
  # Print ANOVA table
  print("ANOVA Results:")
  print(summary(anova.df))
  
  # Set a 2x2 plot layout and print diagnostic plots
  # Only executes if suppress.diag.plot == FALSE
  if (suppress.diag.plot == FALSE) {
    layout(matrix(c(1,2,3,4),2,2))
    plot(anova.df)
    layout(matrix(c(1),1,1))
  }
  
  
  # Calculate Tukey ANOVA from ANOVA results
  tukey.df <- TukeyHSD(anova.df)
  
  # Build data frame from Tukey results for manipulation
  tukey.df <- as.data.frame(tukey.df[1])
  colnames(tukey.df) <- c("diff", "lwr", "upr", "p.adj")  # Re-assert names lost in df translation
  tukey.df$comparison <- factor(rownames(tukey.df), levels = row.names(tukey.df)[order(tukey.df$diff)])  # Copy rownames into a column
  row.names(tukey.df) <- NULL  # Remove row names
  tukey.df$significant <- ifelse(tukey.df$p.adj <= 0.05, "yes", "no")  # Flag statistically significant rows
  
  
  # When truncate == TRUE, filter out Tukey results that are not significant
  if (truncate == TRUE) {
    tukey.df <- tukey.df %>% filter(p.adj <= 0.05)
  }
  
  
  # Print Tukey table. Use kable for nicer looking prints
  print("Tukey Results:")
  print(kable(tukey.df[, 1:5], digits = 3))
  
  
  ## Plot results
  
  # Create a custom color scale, mapping colors to statistical significance (tukey.df$significant)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(tukey.df$significant)
  colScale <- scale_colour_manual(name = "significant",values = myColors)
  
  # Build ggplot of tukey.df. Note the coord_flip to place names on Y axis, and h-line at zero.
  p.tukey <- ggplot(tukey.df, aes(x = comparison, y = diff, ymin = lwr, ymax = upr)) +
    geom_pointrange(aes(colour = significant)) +
    colScale +
    coord_flip() +
    geom_hline(yintercept = 0) +
    labs(title = "Tukey HSD Results", y = "Difference of log-price means")
  print(p.tukey)
  
}



## Bootstrapping functions
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




## Bootstrap Plotting functions
plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  ## Description: Histogram plotting function copied from DS350 course code. 
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}






## Plot multiple histograms for bootstrap results on multiple levels
f_multi_hist <- function(data, simplify = FALSE, nbins = 80, p = 0.05,
                         plot.title = "Histogram", y.label = "mean log-price", x.label = "Level"){
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







######################################################################
##  Analyses
######################################################################


### Make

# Graphical Analysis
f_graph_analysis(data = auto.data, dependent = "log.price", "make")

# Remove unusual levels
make.data <- f_remove_levels(auto.data, "make", remove.levels = c("renault", "isuzu", "mercury", "chevrolet", "alfa-romero", "porsche", "jaguar"))

# ANOVA
f_anova_analyses(make.data, formula(log.price ~ make), suppress.diag.plot = FALSE)

# Bootstrap means
make.one.boot <- f_multilevel_one.boot(make.data, "make", "log.price")
f_multi_hist(make.one.boot, simplify = TRUE, plot.title = "Bootstrap means of log-price by make")

# Bootstrap mean differences
make.two.boot <- f_multilevel_two.boot(make.data, "make", "log.price")
f_multi_hist(make.two.boot, simplify = TRUE, plot.title = "Difference of bootstrap mean log-price by make", y.label = "Mean log-price difference")







#### Body Style

# Graphical Comparison
f_graph_analysis(data = auto.data, dependent = "log.price", "body.style")

# Remove unusual levels
body.data <- f_remove_levels(auto.data, "body.style", remove.levels = c("convertible", "hardtop"))

# ANOVA
f_anova_analyses(body.data, formula(log.price ~ body.style), suppress.diag.plot = FALSE)

# Bootstrap means
body.one.boot <- f_multilevel_one.boot(body.data, "body.style", "log.price")
f_multi_hist(body.one.boot, plot.title = "Bootstrap means of log-price by body style")

# Bootstrap mean differences
body.two.boot <- f_multilevel_two.boot(body.data, "body.style", "log.price")
f_multi_hist(body.two.boot, plot.title = "Difference of bootstrap mean log-price by body style", y.label = "Mean log-price difference")






#### Drive Wheels

# Graphical
f_graph_analysis(data = auto.data, dependent = "log.price", "drive.wheels")

# Remove unusual levels
drive.data <- f_remove_levels(auto.data, "drive.wheels")

# ANOVA
f_anova_analyses(drive.data, formula(log.price ~ drive.wheels), suppress.diag.plot = FALSE)

# Bootstrap means
drive.one.boot <- f_multilevel_one.boot(drive.data, "drive.wheels", "log.price")
f_multi_hist(drive.one.boot, plot.title = "Bootstrap means of log-price by drive wheels")

# Bootstrap mean differences
drive.two.boot <- f_multilevel_two.boot(drive.data, "drive.wheels", "log.price")
f_multi_hist(drive.two.boot, plot.title = "Difference of bootstrap mean log-price by drive wheels", y.label = "Mean log-price difference")








#### Cylinder Count

# Compare variable levels graphically
f_graph_analysis(data = auto.data, dependent = "log.price", "num.of.cylinders")

# Remove unusual levels
cylinder.data <- f_remove_levels(auto.data, "num.of.cylinders", remove.levels = c("two", "three", "twelve"))

# ANOVA
f_anova_analyses(cylinder.data, formula(log.price ~ num.of.cylinders), suppress.diag.plot = FALSE)

# Bootstrap means
cylinder.one.boot <- f_multilevel_one.boot(cylinder.data, "num.of.cylinders", "log.price")
f_multi_hist(cylinder.one.boot, simplify = TRUE, plot.title = "Bootstrap means of log-price by cylinder count")

# Bootstrap mean differences
cylinder.two.boot <- f_multilevel_two.boot(cylinder.data, "num.of.cylinders", "log.price")
f_multi_hist(cylinder.two.boot, simplify = TRUE, plot.title = "Difference of bootstrap mean log-price by cylinder count", y.label = "Mean log-price difference")











#### Engine Type

# Compare variable levels graphically
f_graph_analysis(data = auto.data, dependent = "log.price", "engine.type")

# Remove unusual levels
engine.type.data <- f_remove_levels(auto.data, "engine.type", remove.levels = c("dohcv", "rotor"))

# ANOVA
f_anova_analyses(engine.type.data, formula(log.price ~ engine.type), suppress.diag.plot = FALSE)

# Bootstrap means
engine.type.one.boot <- f_multilevel_one.boot(engine.type.data, "engine.type", "log.price")
f_multi_hist(engine.type.one.boot, simplify = TRUE, plot.title = "Bootstrap means of log-price by engine type")

# Bootstrap mean differences
engine.type.two.boot <- f_multilevel_two.boot(engine.type.data, "engine.type", "log.price")
f_multi_hist(engine.type.two.boot, simplify = TRUE, plot.title = "Difference of bootstrap mean log-price by engine type", y.label = "Mean log-price difference")