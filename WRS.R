
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

library(ggplot2)
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
## Changing data types
##===========================================================================##
# Check the numerical summary before change it.

df_clean %>%
  select(RETAIL.SALES, RETAIL.TRANSFERS, WAREHOUSE.SALES) %>%
  summary()

# Converting sales variables into numeric format.

df_clean <- df_clean %>%
  mutate(
    across(
      c(RETAIL.SALES, RETAIL.TRANSFERS, WAREHOUSE.SALES),
      as.numeric
    )
  )

# This output confirms that the data types are now correct, and the three sales
# variables are numerical, so I can use it for the outlier analysis.

# Check the numerical summary
df_clean %>%
  select(RETAIL.SALES, RETAIL.TRANSFERS, WAREHOUSE.SALES) %>%
  summary()

# Output
#   RETAIL.SALES     RETAIL.TRANSFERS  WAREHOUSE.SALES
# Min.   :  0.000   Min.   :  0.000   Min.   :  0.0  
# 1st Qu.:  0.000   1st Qu.:  0.000   1st Qu.:  0.0  
# Median :  0.250   Median :  0.000   Median :  1.0  
# Mean   :  6.749   Mean   :  6.763   Mean   : 14.4  
# 3rd Qu.:  3.260   3rd Qu.:  3.000   3rd Qu.:  5.0  
# Max.   :992.000   Max.   :991.000   Max.   :999.8  
# NAs    :163       NAs    :1073      NAs    :2205

# RETAIL.SALES Output conclusions.
# At least, 25% of the observations have zero sales 
# The mean (6.749) is considerably higher than the median (0.25)
# The maximum is 992, suggesting some unusually high observations.
# there are 163 missing values

# RETAIL.TRANSFERS Output conclusions.
# at least 50% of the observations have zero transfers
# the mean is 6.763, while the median is zero
# there are 1073 missing values

# WAREHOUSE.SALES Output conclusions.
# at least 25% of observations have zero sales
# the mean is 14.4 compared with a median of 1
# there are 2205 missing values.

# The differences between the means and medians suggest that the distributions
# are strongly right-skewed

# An important fact here is the missing values appeared again.
# Although I previously handled blank values, this summaries now identifies:
# 163 missing values in RETAIL.SALES (0.046)
# 1039 missing values in RETAIL.TRANSFERS (0.304%)
# 2188 missing values in WAREHOUSE.SALES (0.626%)


# Checking values that cannot be converted into numeric format.

sales_columns <- c(
  "RETAIL.SALES",
  "RETAIL.TRANSFERS",
  "WAREHOUSE.SALES"
)

# Sales variables that need numeric conversion.

for (variable in sales_columns) {
  
  original_values <- trimws(as.character(df[[variable]]))
  
  invalid_values <- unique(
    original_values[
      !is.na(original_values) &
        original_values != "" &
        is.na(suppressWarnings(as.numeric(original_values)))
    ]
  )
  
  cat("\nVariable:", variable, "\n") # Printing each variable with their values
  print(invalid_values)
}


## CHALLENGE.
# The oputput shows the actual problem with the dataset. The missing values
# were not simply blank cells. The numeric conversion that I did before
# interpreted some valid sales values as missing.
# For example, thousands separators can not be converted directly
# Negative values in brackets "(0.01)" represents -0.01 in accounting format
# but R does not automatically recognize this as a negative number.
# This explain the new NA values.


##===========================================================================##
## Correcting data types
##===========================================================================##

# Sales variables that need numeric conversion.

sales_columns <- c(
  "RETAIL.SALES",
  "RETAIL.TRANSFERS",
  "WAREHOUSE.SALES"
)

# Function to clean accounting-formatted numbers.

clean_numeric <- function(x) {
  
  # Convert values to character and remove spaces.
  x <- trimws(as.character(x))
  
  # Convert empty strings into NA.
  x[x == ""] <- NA
  
  # Identify negative values represented by brackets.
  negative <- !is.na(x) & grepl("^\\(.*\\)$", x)
  
  # Remove brackets and thousands separators.
  x <- gsub("[(),]", "", x)
  
  # Convert values into numeric format.
  x <- suppressWarnings(as.numeric(x))
  
  # Restore negative signs.
  x[negative] <- -abs(x[negative])
  
  return(x)
}

# Apply the function to the original dataset.
# This avoids using values already incorrectly converted.

df_clean <- df %>%
  mutate(
    SUPPLIER = na_if(trimws(as.character(SUPPLIER)), ""),
    SUPPLIER = replace_na(SUPPLIER, "Unknown"),
    across(all_of(sales_columns), clean_numeric)
  ) %>%
  filter(
    !is.na(ITEM.TYPE),
    trimws(ITEM.TYPE) != ""
  )

##===========================================================================##
## VERIFYING DATA TYPES
##===========================================================================##

# Check numerical summaries.

df_clean %>%
  select(all_of(sales_columns)) %>%
  summary()

# Output summary.
#   RETAIL.SALES      RETAIL.TRANSFERS   WAREHOUSE.SALES   
# Min.   :  -6.490   Min.   : -38.490   Min.   :-7800.00  
# 1st Qu.:   0.000   1st Qu.:   0.000   1st Qu.:    0.00  
# Median :   0.250   Median :   0.000   Median :    1.00  
# Mean   :   6.913   Mean   :   6.867   Mean   :   25.58  
# 3rd Qu.:   3.260   3rd Qu.:   3.000   3rd Qu.:    5.00  
# Max.   :2923.000   Max.   :1990.830   Max.   :18317.00  
# NAs    :7  

# Comparison of numerical summaries
# After correcting the numeric conversion, the results changed
# significantly compared with the original summary

# The original conversion using as.numeric() incorrectly interpreted
# values containing thousands separatos and accounting- format
# negative numbers as missing values

# The new clean_numeric() function successfully corrected there
# formatting issues.

# As a result, the number of missing values decreased considerably:
# RETAIL.SALES: 163 to 7
# RETAIL.TRANSFERS: 1073 to 0
# WAREHOUSE.SALES: 2205 to 0

# The corrected summary also identified larger maximum values
# and negative observations that were not correctly recognized before

# The remaining seven missing values belong to RETAIL.SALES, and I will keep
# as NA to avoid introducing incorrect assumptions into the dataset.


##===========================================================================##
## Handling outliers
##===========================================================================##










