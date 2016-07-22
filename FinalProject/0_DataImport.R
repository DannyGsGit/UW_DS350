#### Analyses To Do: ####
# Check normality and transform
# By-order summary
# Region by day/week/month
# Customer segmentation & comparison of purchasing behavior between segments
# Customer lifetime value analysis
# Bootstrap interesting relationships for deeper investigation





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Load libraries ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Import and format dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Online retail dataset from UCI ML library
# Source: https://archive.ics.uci.edu/ml/datasets/Online+Retail
retail.data <- read.csv("data/OnlineRetail.csv", stringsAsFactors = FALSE)


## Set formats

# Review original types
str(retail.data)

# Set factors
factor.cols <- c("InvoiceNo", "StockCode", "CustomerID", "Country")
retail.data[, factor.cols] <- lapply(retail.data[factor.cols], as.factor)

# Set dates
date.cols <- c("InvoiceDate")
retail.data[, date.cols] <- lapply(retail.data[date.cols], mdy_hm)

# Confirm types
str(retail.data)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define Functions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Returns processing function.
f_process_returns <- function(order.data) {
  
  # Flog orders as returns
  return.columns <- c("InvoiceNo", "StockCode", "CustomerID", "Quantity")
  return.index <- grep("c", order.data$InvoiceNo, ignore.case = TRUE)
  order.data$return <- FALSE
  order.data$return[return.index] <- TRUE
  
  # Generate synthetic key of customer-SKU combination to simplify window functions
  order.data$syn.key <- paste(order.data$CustomerID, order.data$StockCode, sep = "")
  
  ####### Loop this:

  ## Nice test customer: zz <- order.data %>% filter(CustomerID == 12471)
  
  returns.left <- length(which(order.data$return == TRUE))
  order.data$adj.qty <- order.data$Quantity
  order.data <- order.data %>% arrange(syn.key, InvoiceDate)
  
  while (returns.left > 0 ) {
    # Subtract quantity from prior customer order
    order.data <- order.data %>% mutate(prior.qty = ifelse(syn.key == lag(syn.key), lag(Quantity), 0))
    order.data$prior.qty <- replace(order.data$prior.qty, is.na(order.data$prior.qty), 0)
    
    # When current is order and leading, matching synkey is a return, subtract from the current order
    order.data <- order.data %>% mutate(adj.qty = ifelse(lead(syn.key) == syn.key & return == FALSE & lead(return) == TRUE, adj.qty + lead(adj.qty), adj.qty))

    # When current synkey is a return and prior is an order, set adj.qty = 0 (flag for deletion) 
    order.data <- order.data %>% mutate(adj.qty = ifelse(lag(syn.key) == syn.key & return == TRUE & lag(return) == FALSE, 0, adj.qty))

    # Remove RETURNS with no leading ORDER
    order.data <- order.data %>% mutate(adj.qty = ifelse(return == TRUE & prior.qty == 0, 0, adj.qty))
    
    # Remove rows where adj.qty = 0
    order.data <- order.data %>% filter(adj.qty != 0)

    # Set RETURN on all negative quantities to TRUE
    order.data <- order.data %>% mutate(return = ifelse(adj.qty < 0, TRUE, FALSE))
    
    # Are there any returns left?
    returns.left <- length(which(order.data$return == TRUE))
    print(returns.left)
  }
  
  order.data <- order.data %>% select(-prior.qty, -syn.key, -return)
  
  return(order.data)
  
}

# Normality tests
f_norm_test <- function(data) {
  qqnorm(data)
  qqline(data)
}

# Summarization function for aggregate statistics
f_basic_summary <- function(grouped.data) {
  
  current.summary <- summarise(grouped.data,
                               orders = n_distinct(InvoiceNo),
                               customers = n_distinct(CustomerID),
                               volume = sum(adj.qty),
                               log.volume = log(sum(adj.qty)),
                               total.contribution = sum(adj.qty * UnitPrice),
                               log.total.contribution = log(sum(adj.qty * UnitPrice)),
                               mean.contribution = mean(UnitPrice),
                               log.mean.contribution = log(mean(UnitPrice)))
  
  return(current.summary)
}











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Cleansing ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove orders from customers not registered with the site
retail.data <- retail.data %>% filter(!is.na(CustomerID))

# Remove abnormal SKUs
bad.SKU.index <- which(retail.data$StockCode %in% c("BANK CHARGES", "POST", "M", "PADS", "DOT", "CRUK", "D", "C2"))
retail.data <- retail.data[-bad.SKU.index,]

# Clean up returns
retail.data <- f_process_returns(retail.data)

# Add extended cost column
retail.data <- retail.data %>% mutate(ext.cost = UnitPrice * adj.qty)
retail.data <- retail.data %>% filter(ext.cost != 0)

# Extract interesting date characteristics
retail.data$day.of.week <- wday(retail.data$InvoiceDate, label = TRUE)
retail.data$month <- month(retail.data$InvoiceDate, label = TRUE)
retail.data$year <- year(retail.data$InvoiceDate)
retail.data$day.of.month <- mday(retail.data$InvoiceDate)

# Drop extinct levels
retail.data <- droplevels(retail.data)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Transformations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Run normality tests:

# Unit Price
f_norm_test(log(retail.data$UnitPrice))

# Extended cost
f_norm_test(log(retail.data$ext.cost))

# adj.qty
f_norm_test(log(retail.data$adj.qty))

## All three numeric columns should be transformed for improved normality









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### General plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# quantity- mean.contribution








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summary stats and plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Generate alternate view where SKUs are collapsed into a single ORDER row
order.summary <- f_basic_summary(group_by(retail.data, InvoiceNo, CustomerID, InvoiceDate))




## SKU popularity
SKU.summary <- f_basic_summary(group_by(retail.data, StockCode))
pairs(SKU.summary)
# Insights:
# 1) There appears to be grouping of SKUs by item type, as seen by SKU-mean.contribution relationship
# 2) # of orders and # of customers by SKU is strongly co-linear, as expected
# 3) # of orders against volume by SKU shows a range. 
# 4) Cheaper items make the most revenue and volume in total as seen in mean.contribution - total.contribution chart

## Trends by day of week

daily.summary <- f_basic_summary(group_by(retail.data, day.of.week))
pairs(daily.summary)
# Insights:
# 1) The shop is closed on Saturday (1 = Sunday, 6 = Friday)
# 2) Order activity is lowest on Sunday, ramping up to a peak on Thursday and dropping off on Friday
# 3) # of orders, customers, volume, and total contribution are all co-linear on the same pattern
# 4) Sunday is the most frugal day. During the week, the mean cost of goods is inversely proportional to sales activity.
#    As order intake increases, mean prices decrease. We see maxima on Monday and Friday. Perhaps reflecting the dynamics
#    of sales to resellers during midweek and consumers on Monday/Friday?

## Trends by day of month

day.of.month.summary <- f_basic_summary(group_by(retail.data, day.of.month))
pairs(day.of.month.summary)
# Insights:
# 1) Order intake peaks at beginning of month and drops off over the course of the month

## Trends by month

monthly.summary <- f_basic_summary(group_by(retail.data, month))
monthly.SKU.summary <- f_basic_summary(group_by(retail.data, StockCode, month))  # Are some items summer items? Holiday items?
pairs(monthly.summary)
# Insights:
# 1) Order volume is lowest in Jan/Feb, then steadily ramps up to a peak in November, slight drop in December
# 2) Mean price peaks in the summer months, and reaches it's lowest point in the highest volume month of November. 

## Trends by customer
customer.summary <- f_basic_summary(group_by(retail.data, CustomerID))
customer.SKU.summary <- f_basic_summary(group_by(retail.data, CustomerID, StockCode))
pairs(customer.summary)
qplot(mean.contribution, total.contribution, data = customer.summary)
# Insights:
# 1) Broad spectrum of customer types, generally spreading out in 2 directions; low volume/high cost & high volume/low cost

## Regional trends
region.summary <- f_basic_summary(group_by(retail.data, Country))
region.SKU.summary <- f_basic_summary(group_by(retail.data, Country, StockCode))  # Are local affinities apparent?
pairs(region.summary)
# Insights:
# 1) The UK is, by far the largest customer
# 2) Singapore stands out as an outlier for high mean contribution. Interestingly, there is only a single customer that has
#    placed 10 orders for >5,000 items with a mean contribution of >$100 per item.








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Bootstrap analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## SKU relationships

## Time of week/month

library(resample)
library(simpleboot)
library(lazyeval)

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


weekday.volume.bootstrap <- f_multilevel_one.boot(retail.data, "day.of.week", "adj.qty")

f_multi_hist(weekday.volume.bootstrap, simplify = FALSE, plot.title = "Histogram of volume by weekday", y.label = "Volume", x.label = "Weekday")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Choropleth plotting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(leaflet)
library(RColorBrewer)
library(maps)
library(rgdal)

# Load country data to SPDF
countries <- readOGR("json/countries.geojson", "OGRGeoJSON")
# Add statistics to countries
region.summary$Country <- as.character(region.summary$Country)
region.summary$Country[region.summary$Country == "EIRE"] <- "Ireland"
region.summary$Country[region.summary$Country == "RSA"] <- "South Africa"
region.summary$Country[region.summary$Country == "USA"] <- "United States of America"

countries.data <- countries
countries.data@data <- countries.data@data %>% left_join(region.summary, by = c("ADMIN" = "Country"))
countries.data@data$log.customers <- log(countries.data@data$customers)
countries.data@data <- replace(countries.data@data, is.na(countries.data@data), 0)

options(viewer = NULL)

# Choropleth generating function
f_generate_choropleth <- function(country.data, domain) {
  ## Description: Generates a choropleth map
  ## Arguments:
  ## country.data: map data object (e.g. countries.data)
  ## domain: the column of map object containing coloring data (e.g. countries.data$customers)
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = domain
  )
  
  m <- leaflet(country.data) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~pal(domain))
  
  return(m)
}





