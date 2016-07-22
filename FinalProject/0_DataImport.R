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
#### Popularity and seasonality of SKUs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## SKU popularity
SKU.summary <- f_basic_summary(group_by(retail.data, StockCode))
pairs(SKU.summary)
# Insights:
# 1) There appears to be grouping of SKUs by item type, as seen by SKU-mean.contribution relationship
# 2) # of orders and # of customers by SKU is strongly co-linear, as expected
# 3) # of orders against volume by SKU shows a range. 
# 4) Cheaper items make the most revenue and volume in total as seen in mean.contribution - total.contribution chart

## Trends by day of week
retail.data$day.of.week <- wday(retail.data$InvoiceDate, label = TRUE)
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
retail.data$day.of.month <- mday(retail.data$InvoiceDate)
day.of.month.summary <- f_basic_summary(group_by(retail.data, day.of.month))
pairs(day.of.month.summary)
# Insights:
# 1) Order intake peaks at beginning of month and drops off over the course of the month

## Trends by month
retail.data$month <- month(retail.data$InvoiceDate, label = TRUE)
retail.data$year <- year(retail.data$InvoiceDate)
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





