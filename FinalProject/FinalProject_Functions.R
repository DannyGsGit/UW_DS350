## Sensor comparisons

f_build_data <- function(data, GT, PT08) {
  new.data <- data[ , c("timestamp", substitute(GT), substitute(PT08))]
  new.data <- na.exclude(new.data)
  row.names(new.data) <- NULL
  
  return(new.data)
}

f_normplots <- function(data) {
  
  label2.1 <- paste("Histogram of", colnames(data)[2])
  label2.2 <- paste("QQ Plot of", colnames(data)[2])
  label3.1 <- paste("Histogram of", colnames(data)[3])
  label3.2 <- paste("QQ Plot of", colnames(data)[3])
  
  label2.3 <- paste("Histogram of log", colnames(data)[2])
  label2.4 <- paste("QQ Plot of log", colnames(data)[2])
  label3.3 <- paste("Histogram of log", colnames(data)[3])
  label3.4 <- paste("QQ Plot of log", colnames(data)[3])
  
  par(mfrow = c(2,2))
  hist(data[,2], main = label2.1)
  qqPlot(data[,2], main = label2.2)
  hist(log(data[,2]), main = label2.3)
  qqPlot(log(data[,2]), main = label2.4)
  par(mfrow = c(1,1))
  
  par(mfrow = c(2,2))
  hist(data[,3], main = label3.1)
  qqPlot(data[,3], main = label3.2)
  hist(log(data[,3]), main = label3.3)
  qqPlot(log(data[,3]), main = label3.4)
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
  plot(data[, substitute(feature)], data[, substitute(target)],
       xlab = feature, ylab = target)
  lines(data[, substitute(feature)], data$fit, col = "blue", lwd = 2)
  lines(data[, substitute(feature)], data$lwr, col = "red", lwd = 2)
  lines(data[, substitute(feature)], data$upr, col = "red", lwd = 2)
}

f_time_labels <- function(data) {
  data$time <- hour(data$timestamp)
  data$wday <- wday(data$timestamp)
  data$day <- day(data$timestamp)
  data$week <- week(data$timestamp)
  data$month <- month(data$timestamp)
  data$year <- year(data$timestamp)
  
  return(data)
}

f_remove_outliers <- function(data, outliers) {
  data <- data[-outliers,]
  return(data)
}



## Bootstrap functions

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
  
  # # Print ANOVA table
  # print("ANOVA Results:")
  # print(summary(anova.df))
  
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
  # print("Tukey Results:")
  # print(kable(tukey.df[, 1:5], digits = 3))
  
  
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
    labs(title = "Tukey HSD Results", y = "Difference of means")
  print(p.tukey)
  
}