
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
df <- read.csv("Warehouse_and_Retail_Sales.csv", header = TRUE)

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

## Handling missing values:

# Calculating the percentage of missing values in each column.

n <- nrow(df)

round(colSums(is.na(df)) / n * 100, 3)

# The percentage of missing data helps determine how much information
# is missing from each variable and whether it could affect the analysis.

# Creating a clean version of the original dataset.

df_clean <- df

# Checking the missing values in the dataset.
# The variables supplier, item_description and item_type are categorical.
# The variables rtl_sales, rtl_transfers and whs_sales represent
# sales and movement quantities.

# Replacing missing categorical values with "Unknown".
# This avoids deleting observations simply because a category is missing.

df_clean <- df_clean %>%
  mutate(
    supplier = replace_na(as.character(supplier), "Unknown"),
    item_description = replace_na(as.character(item_description), "Unknown"),
    item_type = replace_na(as.character(item_type), "Unknown")
  )

# Checking the numerical columns for missing values.

round(
  colSums(is.na(df_clean)) / nrow(df_clean) * 100,
  3
)

# Removing observations with missing information in the variables
# required for the analysis.

df_clean <- df_clean %>%
  filter(
    !is.na(calendar_year),
    !is.na(cal_month_num),
    !is.na(item_code),
    !is.na(rtl_sales),
    !is.na(rtl_transfers),
    !is.na(whs_sales)
  )

# Checking the missing values again.

round(
  colSums(is.na(df_clean)) / nrow(df_clean) * 100,
  3
)

# Checking the dimensions after handling missing values.

dim(df_clean)

##===========================================================================##
## Checking and correcting data types
##===========================================================================##

# The sales and transfer variables should be numeric.
# Calendar year and month should also be numeric.

df_clean <- df_clean %>%
  mutate(
    calendar_year = as.integer(calendar_year),
    cal_month_num = as.integer(cal_month_num),
    item_code = as.character(item_code),
    rtl_sales = as.numeric(rtl_sales),
    rtl_transfers = as.numeric(rtl_transfers),
    whs_sales = as.numeric(whs_sales)
  )

# Checking the data types again.

glimpse(df_clean)

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

df_clean %>%
  select(rtl_sales, rtl_transfers, whs_sales) %>%
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
