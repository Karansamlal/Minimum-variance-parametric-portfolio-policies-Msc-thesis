###########################################################################
#################### Load data and clean ###################################
### The data-set monthly_returns_crsp.csv is downloaded using the CRSP datebase and searching the entire database
### The data-set datashare.csv is obtained from the website of Shihao Gu, where the authors of
### the paper Gu (2020) posted data of the firm characteristics considered in Green (2017).

# Clear variables
rm(list = ls())

# load libraries
library(data.table)
library("data.table")
library("readr")

# read in the firm-characteristics data
return.data <- read.csv("monthly_returns_crsp.csv")

# clean data
return.data <- return.data[return.data$SHRCD %in% 10:11,] # keep common shares
return.data <- return.data[!is.na(return.data$PRC),] # remove NA prices
return.data <- return.data[!return.data$PRC %in% (-(4:9)*11),] # all stocks in NYSE,NASDAQ, other exchange
return.data$RET <- as.numeric(return.data$RET)
return.data <- return.data[!is.na(return.data$RET),] # remove NA returns
return.data$PRC <- abs(return.data$PRC) # 
return.data <- return.data[return.data$EXCHCD %in% 1:3,]

# convert the dates
return.data$date <- as.Date(paste(c(return.data$date)), format("%d/%m/%Y"))

# manipulate dates so that the dates always are end of the month
return.data <- return.data %>% group_by(PERMNO)%>% distinct(date, .keep_all = T)

# subset return data
return.data <- return.data[, c("PERMNO", "date", "RET", "SHROUT", "PRC")]

# read in factor data
csv.name <- "datashare.csv"
factor.data <- read_delim(csv.name, ",", col_names = TRUE)

# adjust columns in the factor data
names(factor.data)[names(factor.data) == 'permno'] <- 'PERMNO'
names(factor.data)[names(factor.data) == 'DATE'] <- 'date'

# adjust dates in factor data
factor.data$date <- as.Date(paste(c(factor.data$date)), format("%Y%m%d"))

# remove possible duplicates in the factor data-set
factor.data<- factor.data %>% group_by(PERMNO)%>% distinct(date, .keep_all = T)

# merge characteristics, and return data-set
df.merged <- merge(return.data, factor.data, by = c("date", "PERMNO"), all.x = T, all.y = F)



################## clean the merged data-set according to brandt ############################

# then remove firms that have no market-value
df.merged$me = log(df.merged$mvel1)
df.merged = df.merged[which(df.merged$me > 0),]

# compute market capitalization
df.merged$market_cap = df.merged$SHROUT * df.merged$PRC
df.merged <- df.merged %>% group_by(PERMNO)  %>% mutate(lagged_market_cap = lag(market_cap, na.rm = TRUE))

# fill NA features with 0
features = names(df.merged)[6:length(names(df.merged))]
df.merged[,features][is.na(df.merged[,features])] <- 0

# first remove firms with negative book-to-market ratio
df.merged = df.merged[which(df.merged$bm >= 0),] 

# then remove firms that are below the 20 percentile, so remove very small firms
# since these are very difficult to trade due to the lack of liquidity 
df.merged  = df.merged[which(df.merged $me > quantile(df.merged$me, 0.2)),]

# check how many firms there are in the sample
num_firms_time = df.merged %>%
  group_by(date) %>%
  summarise(count = n())
plot(num_firms_time$date,num_firms_time$count, type = "l")
num_firms_time[which(num_firms_time$count == max(num_firms_time$count)),]
num_firms_time[which(num_firms_time$count == min(num_firms_time$count)),]

