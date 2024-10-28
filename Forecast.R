library(tidyverse)
library(salesforcer)
library(lubridate)
library(forecast)
library(modeltime)
library(timetk)

sf_auth()

query1 <- "SElECT  Id, ACV_for_Quotas__c, StageName, Record_type_Name__c, shov_revenue_type__c, OwnerId, CloseDate FROM Opportunity"

opp_data <- sf_query(query1)

clean_df <- opp_data %>% 
  mutate(CloseDate = mdy(CloseDate))

clean_df <- clean_df %>% 
  filter(StageName == "6 - Closed Won")

library(dplyr)

# Step 1: Filter rows based on CloseDate, StageName, and ACV_for_Quotas__c
sales_data <- opp_data %>%
  filter(CloseDate >= as.Date("2022-06-01") &          
           StageName == '6 - Closed Won' &             
           ACV_for_Quotas__c > 0 &
           shov_revenue_type__c == "New Revenue") %>%                    
  filter(!is.na(ACV_for_Quotas__c) & !is.na(CloseDate)) 

# Step 2: Create a `YearMonth` column for monthly aggregation
sales_data <- sales_data %>%
  mutate(YearMonth = floor_date(CloseDate, "month"))

# Step 3: Aggregate sales by month
monthly_sales <- sales_data %>%
  group_by(YearMonth) %>%
  summarise(MonthlySales = sum(ACV_for_Quotas__c))

# Step 4: Check for missing dates in the sequence (optional)
# You can check if every month is represented, and if not, consider adding rows with `NA` or `0` to fill gaps.

# Step 5: Convert to a time series object
# Ensure there are no missing values in `MonthlySales` before conversion
monthly_sales_ts <- ts(monthly_sales$MonthlySales,
                       start = c(year(min(monthly_sales$YearMonth)), month(min(monthly_sales$YearMonth))),
                       frequency = 12)

# Step 6: Fit an ARIMA model
fit <- auto.arima(monthly_sales_ts)

# Step 7: Forecast for the next 12 months
sales_forecast <- forecast(fit, h = 12)

# Step 8: Plot the forecast
plot(sales_forecast)
