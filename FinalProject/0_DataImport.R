#### Load libraries ####
library(dplyr)
library(lubridate)
library(reshape2)


#### Import data ####

# Online retail dataset from UCI ML library
# Source: https://archive.ics.uci.edu/ml/datasets/Online+Retail
retail.data <- read.csv("data/OnlineRetail.csv", stringsAsFactors = FALSE)





#### Set formats ####

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





#### Cleansing ####

## Remove orders from customers not registered with the site
retail.data <- retail.data %>% filter(!is.na(CustomerID))


## Returns append a "C" in front of the InvoiceNo. This makes it easy to match orders and their returns.
f_process_returns <- function(order.data) {
  
  # Flog orders as returns
  return.columns <- c("InvoiceNo", "StockCode", "CustomerID", "Quantity")
  return.index <- grep("c", order.data$InvoiceNo, ignore.case = TRUE)
  return.data <- order.data[return.index, return.columns]
  
  # Subtract quantity from prior customer order
  # Look at dplyr lead/lag functions
  
}







#### Popularity and seasonality of SKUs
# Seasons: monthly, weekly, daily

## Create additional date-based columns

# Day of week
retail.data$day.of.week <- wday(retail.data$InvoiceDate, label = TRUE)

# Day of month
retail.data$day.of.month <- mday(retail.data$InvoiceDate)

# Month of year
retail.data$month <- month(retail.data$InvoiceDate, label = TRUE)
retail.data$year <- year(retail.data$InvoiceDate)




## Group_bys
f_basic_summary <- function(grouped.data) {
  
  current.summary <- summarise(grouped.data,
                               orders = n_distinct(InvoiceNo),
                               customers = n_distinct(CustomerID),
                               volume = sum(Quantity),
                               contribution = sum(Quantity * UnitPrice))
  
  return(current.summary)
  
}

# SKU popularity
SKU.summary <- f_basic_summary(group_by(retail.data, StockCode))

# Trends by day of week
daily.summary <- f_basic_summary(group_by(retail.data, day.of.week))

# Trends by day of month
day.of.month.summary <- f_basic_summary(group_by(retail.data, day.of.month))

# Trends by month
monthly.summary <- f_basic_summary(group_by(retail.data, month))
monthly.SKU.summary <- f_basic_summary(group_by(retail.data, StockCode, month))  # Are some items summer items? Holiday items?

# Trends by customer
customer.summary <- f_basic_summary(group_by(retail.data, CustomerID))
customer.SKU.summary <- f_basic_summary(group_by(retail.data, CustomerID, StockCode))

# Regional trends
region.summary <- f_basic_summary(group_by(retail.data, Country))
region.SKU.summary <- f_basic_summary(group_by(retail.data, Country, StockCode))  # Are local affinities apparent?






#### Customer segmentation & comparison of purchasing behavior between segments

## Customer segmentation
# Generate list of StockCodes purchased by more than one customer and with total sales > 1
SKU.list <- SKU.summary %>% filter(customers > 1, volume > 1) %>% select(StockCode)

# Filter down the SKU-Customer summary to only include SKUs listed above
customer.orders <- customer.SKU.summary %>%
  inner_join(SKU.list, by = "StockCode") %>%
  filter(volume > 0) %>%
  select(CustomerID, StockCode) %>%
  mutate(Count = 1)

# Translate the list-style table into a sparse matrix with rows as customers and columns as SKUs
sparse.customer.orders <- dcast(customer.orders, CustomerID ~ StockCode)
rownames(sparse.customer.orders) <- sparse.customer.orders$CustomerID  # Set customer names to the rowname
sparse.customer.orders$CustomerID <- NULL  # Remove the customer name column
sparse.customer.orders[is.na(sparse.customer.orders)] <- 0  # Replace NA with 0

library(NMF)

fit<-nmf(sparse.customer.orders[1:100, ], 5, "lee", nrun=10)
basismap(fit)
coefmap(fit)

# code for sorting and printing
# the two factor matrices
h<-coef(fit)
library(psych)
fa.sort(t(round(h,3)))
w<-basis(fit)
wp<-w/apply(w,1,sum)
fa.sort(round(wp,3))

# hard clustering
type<-max.col(w)
table(type)
t(aggregate(Scotch, by=list(type), FUN=mean))






## Customer lifetime value analysis
