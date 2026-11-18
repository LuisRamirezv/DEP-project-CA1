
##===========================================================================##
## CA1 - Data Exploration and Preparation
## Luis Ramirez
## 2023169
##===========================================================================##
## Github = https://github.com/LuisRamirezv/DEP-project-CA1
## Video =
##===========================================================================##
# This project sits in the domain of retail and warehouse sales analysis.
# The motivation for this data preparation and visualisation is to understand
# the movement of products between warehouses and retail stores, identify
# sales patterns, and investigate differences between product categories
# and suppliers.
##===========================================================================##

# Installing packages
install.packages(c("tidyverse", "skimr"))

library(tidyverse)
library(skimr)

## Loading the dataset into df to process.

# Importing the Warehouse and Retail Sales dataset.
df <- read.csv("WRS.csv", header = TRUE)

dim(df) # Number of rows and columns
colnames(df) # Names of the columns

## Attributes, structure and types.

head(df) # The first 6 rows
glimpse(df) # Column names, types and sample values

# Checking the general summary of the dataset.
summary(df)

##===========================================================================##
## Missing values
##===========================================================================##

# Showing only the columns that contain missing values.
Filter(function(x) x > 0, colSums(is.na(df)))

# The output is "named numeric(0)", meaning none of the columns contains NA values

# Checking by columns
n <- nrow(df)

round(colSums(is.na(df)) / n * 100, 3)

# all nine columns contain 0% missing values
# Therefore, it is necessary to investigate blank values separately.

## Checking blank values
# Counting blanc values in each column

blank_counts <- sapply(df, function(x) {
  sum(trimws(as.character(x)) == "", na.rm = TRUE)
})

# Showing only the columns containing blank values

Filter(function(x) x > 0, blank_counts)

# output:
#     SUPPLIER    ITEM.TYPE RETAIL.SALES 
#          217            1            7 

# Calculating the percentage of blank values in each column.

round(blank_counts / n * 100, 3)
# Output
# 


##===========================================================================##
## Handling outliers
##===========================================================================##

# Based on the dataset structure, the variables that are relevant
# for outlier investigation are:
#
# rtl_sales     = Retail sales
# rtl_transfers = Transfers to retail
# whs_sales     = Warehouse sales
#
# The variables item_code, calendar_year and cal_month_num should not
# be investigated using the IQR method as they represent identifiers
# and time references rather than sales quantities.

## Checking the data summary of the sales variables.

df %>%
  select(RETAIL.SALES, RETAIL.TRANSFERS, WAREHOUSE.SALES) %>%
  summary()

# The summary provides the minimum, maximum, mean, median and quartiles
# of each numerical variable.
#
# These statistics help identify unusually high or low sales values.

## Identifying potential outliers using the IQR method.

# Calculating the IQR boundaries for each sales variable.

outlier_limits <- df_clean %>%
  summarise(
    Q1_Retail = quantile(rtl_sales, 0.25, na.rm = TRUE),
    Q3_Retail = quantile(rtl_sales, 0.75, na.rm = TRUE),
    IQR_Retail = IQR(rtl_sales, na.rm = TRUE),
    
    Lower_Retail = Q1_Retail - 1.5 * IQR_Retail,
    Upper_Retail = Q3_Retail + 1.5 * IQR_Retail,
    
    Q1_Transfers = quantile(rtl_transfers, 0.25, na.rm = TRUE),
    Q3_Transfers = quantile(rtl_transfers, 0.75, na.rm = TRUE),
    IQR_Transfers = IQR(rtl_transfers, na.rm = TRUE),
    
    Lower_Transfers = Q1_Transfers - 1.5 * IQR_Transfers,
    Upper_Transfers = Q3_Transfers + 1.5 * IQR_Transfers,
    
    Q1_Warehouse = quantile(whs_sales, 0.25, na.rm = TRUE),
    Q3_Warehouse = quantile(whs_sales, 0.75, na.rm = TRUE),
    IQR_Warehouse = IQR(whs_sales, na.rm = TRUE),
    
    Lower_Warehouse = Q1_Warehouse - 1.5 * IQR_Warehouse,
    Upper_Warehouse = Q3_Warehouse + 1.5 * IQR_Warehouse
  )

# Displaying the calculated boundaries.

outlier_limits

## Identifying observations outside the IQR boundaries.

# Retail sales outliers.

outliers_retail <- df_clean %>%
  filter(
    rtl_sales < outlier_limits$Lower_Retail |
      rtl_sales > outlier_limits$Upper_Retail
  )

# Retail transfers outliers.

outliers_transfers <- df_clean %>%
  filter(
    rtl_transfers < outlier_limits$Lower_Transfers |
      rtl_transfers > outlier_limits$Upper_Transfers
  )

# Warehouse sales outliers.

outliers_warehouse <- df_clean %>%
  filter(
    whs_sales < outlier_limits$Lower_Warehouse |
      whs_sales > outlier_limits$Upper_Warehouse
  )

## Number of potential outliers.

nrow(outliers_retail)
nrow(outliers_transfers)
nrow(outliers_warehouse)

## Percentage of potential outliers.

round(
  nrow(outliers_retail) / nrow(df_clean) * 100,
  3
)

round(
  nrow(outliers_transfers) / nrow(df_clean) * 100,
  3
)

round(
  nrow(outliers_warehouse) / nrow(df_clean) * 100,
  3
)

##===========================================================================##
## Outliers by product type
##===========================================================================##

# Investigating potential outliers within each item type.
# This allows comparisons between different product categories,
# since sales quantities can vary considerably depending on the product.

df_clean %>%
  group_by(item_type) %>%
  summarise(
    Q1_Retail = quantile(rtl_sales, 0.25, na.rm = TRUE),
    Q3_Retail = quantile(rtl_sales, 0.75, na.rm = TRUE),
    IQR_Retail = IQR(rtl_sales, na.rm = TRUE),
    Upper_Retail = Q3_Retail + 1.5 * IQR_Retail,
    
    Outliers_Retail = sum(
      rtl_sales > Upper_Retail,
      na.rm = TRUE
    ),
    
    Q1_Warehouse = quantile(whs_sales, 0.25, na.rm = TRUE),
    Q3_Warehouse = quantile(whs_sales, 0.75, na.rm = TRUE),
    IQR_Warehouse = IQR(whs_sales, na.rm = TRUE),
    Upper_Warehouse = Q3_Warehouse + 1.5 * IQR_Warehouse,
    
    Outliers_Warehouse = sum(
      whs_sales > Upper_Warehouse,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

##===========================================================================##
