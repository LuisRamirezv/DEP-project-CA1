
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
## Checking Missing values
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
# Counting blank values in each column

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
#     SUPPLIER
#       0.062% 

# In the SUPPLIER category Only 0.062% of the values are blank, and
# removing these observations would mean losing otherwise valid sales 
# information.
# Replacing blanks with "Unknown" preserves the sales record without inventing
# supplier information


##===========================================================================##
## Handling Missing values
##===========================================================================##

# Creating a clean version of the original dataset.

df_clean <- df

# Replacing blank supplier with "Unknown"

df_clean <- df_clean %>%
  mutate(
    SUPPLIER = na_if(trimws(as.character(SUPPLIER)), ""),
    SUPPLIER = replace_na(SUPPLIER, "Unknown")
  )

# Checking blank values again.

blank_counts_clean <- sapply(df_clean, function(x) {
  sum(trimws(as.character(x)) == "", na.rm = TRUE)
})


Filter(function(x) x > 0, blank_counts_clean)

# Output
# ITEM.TYPE RETAIL.SALES 
#        1            7

#Removing observations with blank values in ITEM.TYPE and RETAIL.SALES.

df_clean <- df_clean %>%
  filter(
    trimws(ITEM.TYPE) != "",
    trimws(RETAIL.SALES) != ""
  )

# Checking blank values again.

blank_counts_clean <- sapply(df_clean, function(x) {
  sum(trimws(as.character(x)) == "", na.rm = TRUE)
})

Filter(function(x) x > 0, blank_counts_clean)

# Checking the dimensions after removing blank observations.

dim(df_clean)
# the dimension are the same that in the beginning

##===========================================================================##
## Handling outliers
##===========================================================================##































