
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
#
# DATASET.
#
# Title: Warehouse and Retail Sales.
# URL: https://catalog.data.gov/dataset/warehouse-and-retail-sales
#
# The dataset records monthly sales and product movement in Montgomery County, 
# Maryland, including supplier names, item descriptions, product types, retail 
# sales, retail transfers, and warehouse sales.
#
# PROBLEM DOMAIN.
#
# In Montgomery County, Maryland, products from different suppliers are sent
# from warehouse to retail stores (transfers) and sold in the stores (retail sales)
# or directly from the warehouse (warehouse sales).
#
# MOTIVATION.
#
# Understanding how products move between warehouse and stores helps to plan
# stock, avoid sending product that does not sell, and identify the categories
# and suppliers that matter most.
#
#
##===========================================================================##
#
#
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
## OUTLIER DETECTION
##===========================================================================##

# The variables to analyse for outliers will be "RETAIL.SALES", "RETAIL.TRANSFERS",
# and "WAREHOUSE.SALES".

# Using 1.5 as the standard IQR multiplier
k <- 1.5

# Sales variables to analyse.

sales_columns <- c(
  "RETAIL.SALES",
  "RETAIL.TRANSFERS",
  "WAREHOUSE.SALES"
)

# Function to detect outliers.

detect_outliers <- function(x) {
  
  x <- x[!is.na(x) & x != 0]
  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  
  lower <- Q1 - k * IQR_value
  upper <- Q3 + k * IQR_value
  
  outliers <- x[x < lower | x > upper]
  
  data.frame(
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR_value,
    Lower_Limit = lower,
    Upper_Limit = upper,
    Number_Outliers = length(outliers),
    Percentage_Outliers = round(length(outliers) / length(x) * 100, 3)
  )
}

# Apply the function to all sales variables.

outlier_results <- bind_rows(
  lapply(sales_columns, function(variable) {
    result <- detect_outliers(df_clean[[variable]])
    result$Variable <- variable
    result
  })
) %>%
  select(Variable, everything())

print(outlier_results)

# I used the IQR method to identify potential outliers
# in the three sales variables, and a multiplier of 1.5 was used because
# is the standard value in tht IQR method.

# Since the dataset contains many zeros values, the calculation
# were performed using non-zero values only, preventing from affect
# the outlier limits and allows unusual values to be identified.

##===========================================================================##
## HANDLING OUTLIERS
##===========================================================================##

# Removing outliers from the sales variables using the calculated IQR limits and
# missing values and zero values are kept to avoid losing valid records.
# The cleaned dataset is stored in df_no_outliers

df_no_outliers <- df_clean

for (variable in sales_columns) {
  
  limits <- outlier_results %>%
    filter(Variable == variable)
  
  df_no_outliers <- df_no_outliers %>%
    filter(
      is.na(.data[[variable]]) |
        .data[[variable]] == 0 |
        (.data[[variable]] >= limits$Lower_Limit &
           .data[[variable]] <= limits$Upper_Limit)
    )
}

# Comparing the number of rows before and after removing outliers.

cat("Rows before:", nrow(df_clean), "\n")
cat("Rows after:", nrow(df_no_outliers), "\n")
cat("Rows removed:", nrow(df_clean) - nrow(df_no_outliers), "\n")

# Comparing numerical summaries.

summary(df_clean[sales_columns])
summary(df_no_outliers[sales_columns])

##===========================================================================##
## OUTLIER VISUALISATION
##===========================================================================##

# Preparing data for the boxplots.

sales_before <- df_clean %>%
  select(all_of(sales_columns)) %>%
  mutate(Dataset = "Before")

sales_after <- df_no_outliers %>%
  select(all_of(sales_columns)) %>%
  mutate(Dataset = "After")

sales_long <- bind_rows(sales_before, sales_after) %>%
  pivot_longer(
    cols = all_of(sales_columns),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value), Value != 0)

# Creating boxplots to compare before and after.

ggplot(sales_long, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_boxplot(coef = k, outlier.colour = "red", outlier.size = 1) +
  facet_wrap(~ Variable) +
  coord_trans(y = "pseudo_log") +
  scale_x_discrete(limits = c("Before", "After")) +
  scale_y_continuous(breaks = c(-1000, -10, 0, 10, 100, 1000, 10000)) +
  scale_fill_manual(values = c("Before" = "tomato", "After" = "steelblue")) +
  labs(
    title = "Sales Outliers Before and After Cleaning",
    subtitle = paste0("IQR method (", k, " x IQR) on non-zero values, pseudo-log axis"),
    x = NULL,
    y = "Sales Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# The bloxplot compare the sales distributions before
# and after oultier removal

# The red and blue boxes represent the datasets before
# and after cleaning, respectively, according to the
# default ggplot colour palette

# I created a separately displayed variable to make it easy for comparison

##===========================================================================##
## EXPLORATORY DATA ANALYSIS (EDA)
##===========================================================================##

# df_clean is used for totals and counts (outliers are real sales)
# df_no_outliers is used for distributions and relationships.

## Variations with one variable.

####### Q1. Which type of alcohol is sold the most?
alcohol <- df_clean %>%
  filter(ITEM.TYPE %in% c("BEER", "WINE", "LIQUOR", "KEGS")) %>%
  group_by(ITEM.TYPE) %>%
  summarise(Total_Sales = sum(RETAIL.SALES, na.rm = TRUE))

ggplot(alcohol, aes(x = reorder(ITEM.TYPE, -Total_Sales), y = Total_Sales)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total retail sales by alcohol type",
       x = "Alcohol type", y = "Total retail sales") +
  theme_minimal()
## Liquor refers to Spirit drinks

alcohol <- arrange(alcohol, desc(Total_Sales))
alcohol   # prints the quantity of each type

####### Q2 How many records are there in each year?
ggplot(df_clean, aes(x = factor(YEAR))) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = format(after_stat(count), big.mark = ",")),
            vjust = -0.5) +
  labs(title = "Number of records per year", x = "Year", y = "Records") +
  theme_minimal() 

## Covariations between two variables

####### Q3. when more stock is sent to the stores (transfer), do the stores sell more?

data1 <- subset(df_no_outliers, RETAIL.SALES > 0 & RETAIL.TRANSFERS > 0)
set.seed(123)
data1 <- slice_sample(data1, n = 20000)

ggplot(data1, aes(RETAIL.TRANSFERS, RETAIL.SALES)) +
  geom_point(alpha = 0.15, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "tomato", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Retail sales vs retail transfers (log 10 scale)",
       x = "Retail transfers", y = "Retail sales") +
  theme_minimal()

cor(data1$RETAIL.SALES, data1$RETAIL.TRANSFERS, method = "spearman")

# Observation: 
# There is a positive relationship between te high stock send to the stores and
# the sell. As the transfer increase, the retail sales increase.
# The red line is close to 45 angles degrees on the log scale, so sales grow 
# roughly in step with transfers.But transfers alone do not predict sales precisely
# Since many records sold much more or much less than the line suggests.

####### Q4. Do the products that sell well in the stores also sell well from the warehouse?

data2 <- subset(df_no_outliers, RETAIL.SALES > 0 & WAREHOUSE.SALES > 0)
set.seed(123)
data2 <- slice_sample(data2, n = 20000)

ggplot(data2, aes(RETAIL.SALES, WAREHOUSE.SALES)) +
  geom_point(alpha = 0.15, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "tomato", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Warehouse sales vs retail sales (log10)",
       x = "Retail sales", y = "Warehouse sales") +
  theme_minimal()

# Observation: 
# The plot shows a weak positive relationship. Products that sell more in stores
# sell only slightly more from the warehouse, and the dots are spread across
# the whole plot.
# 

####### Unusual patterns

####### UP1 - Which item types have negative values?
neg <- subset(df_clean, RETAIL.SALES < 0 | RETAIL.TRANSFERS < 0 | WAREHOUSE.SALES < 0)

ggplot(neg, aes(x = fct_infreq(ITEM.TYPE))) +
  geom_bar(fill = "tomato") +
  labs(title = "Records with negative values by item type",
       x = "Item type", y = "Records") +
  theme_minimal()

nrow(neg)   # total records with a negative value

# Observation: 
# the plot shows that negative values appear in evey item, but they are
# concentrated in whine, which has more than twice as many as any other 
# type. Those negative sales and transfer may be possible returns or 
# corrections.


####### UP2 -  Do sales change over time (per month)?
monthly <- df_clean %>%
  group_by(YEAR, MONTH) %>%
  summarise(Total = sum(RETAIL.SALES, na.rm = TRUE), .groups = "drop") %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-")))

ggplot(monthly, aes(DATE, Total)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue") +
  labs(title = "Monthly total retail sales", x = "Month", y = 
         "Total retail sales") +
  theme_minimal()

# Observation: 
# Retail sales have a high peak at the end of 2017, which is probably the
# holiday season. January is low in each year (2018, 2019 and 2020), so the
# drop at the start of 2020 looks seasonal and happened before the COVID-19
# restrictions of March 2020. Sales were high again in February-March 2020 and
# fell afterwards, which may be related to the pandemic. 
# There is also a gap of several years in the data, so the long straight line 
# in the plot only joins two separate periods.

##===========================================================================##
## ONE-HOT ENCODING
##===========================================================================##

# One-hot encoding is done by transforming the variable ITEM.TYPE, because
# it has fewer categories, and no order between them.

# The 7 records with missing RETAIL.SALES values were excluded before encoding 
# to prepare the data for aonther analysis.

df_enc <- df_no_outliers %>%
  filter(!is.na(RETAIL.SALES))

onehot <- model.matrix(~ ITEM.TYPE - 1, data = df_enc) %>%
  as.data.frame()

colnames(onehot) <- gsub("ITEM.TYPE", "TYPE_", colnames(onehot))

head(onehot)
colSums(onehot)

##===========================================================================##
## PRINCIPAL COMPONENT ANALYSIS (PCA)
##===========================================================================##

# Selecting the numeric variables and the one-hot encoded columns.
# This will allows me to include both sales information and product
# categories in the PCA analysis

pca_data <- cbind(df_enc[, c("YEAR", "MONTH", sales_columns)], onehot)

# Removing variables with zero variance because the have the same value
# in every row, and they will not provide any useful information.
pca_data <- pca_data[, sapply(pca_data, var) > 0]

# Variables are scaled because they have different units.
pca <- prcomp(pca_data, scale. = TRUE)

#Checking how much of the total variance is explained by each component
summary(pca)

# Variance explained and number of components for 80%.
var_exp <- pca$sdev^2 / sum(pca$sdev^2)
cum_var <- cumsum(var_exp)
n_pc <- which(cum_var >= 0.8)[1]
n_pc

###### V1 - PCA Screeplot ######

# Creating a dataframe to prepare the screeplot.
scree <- data.frame(PC = 1:length(var_exp), Variance = var_exp, Cumulative = cum_var)

# Visualizing the screeplot
ggplot(scree, aes(PC)) +
  geom_col(aes(y = Variance), fill = "steelblue") +
  geom_line(aes(y = Cumulative), colour = "tomato") +
  geom_point(aes(y = Cumulative), colour = "tomato") +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_x_continuous(breaks = scree$PC) +
  labs(title = "Scree plot", x = "Principal component",
       y = "Proportion of variance") +
  theme_minimal()

# Cheking the Loadings of the first four components.
round(pca$rotation[, 1:4], 2)

loadings <- as.data.frame(pca$rotation[, 1:4])
loadings$Feature <- rownames(loadings)
loadings <- pivot_longer(loadings, -Feature, names_to = "PC", values_to = "Loading")

###### V2 - PCA Heatmap ######

# Creating a heatmap t compare the contribution of each variable
# Blue represents positive loading, while red represents negative loading.

ggplot(loadings, aes(PC, Feature, fill = Loading)) +
  geom_tile() +
  geom_text(aes(label = round(Loading, 2)), size = 3) +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "steelblue") +
  labs(title = "PCA loadings", x = NULL, y = NULL) +
  theme_minimal()

# Records on PC1 and PC2 (sample of 5000).
scores <- data.frame(pca$x[, 1:2], ITEM.TYPE = df_enc$ITEM.TYPE)
set.seed(123)
scores <- slice_sample(scores, n = 5000)

###### V3 - PCA Scatter Plot ######

# Creating a scatter plot using the first two principal components.
# Different colors represent different items categories.

ggplot(scores, aes(PC1, PC2, colour = ITEM.TYPE)) +
  geom_point(alpha = 0.5) +
  labs(title = "Records on the first two principal components") +
  theme_minimal()

# Observation:
# Wine and liquor from two separate bands, and beer and kegs from small
# clusters close to each other, so the first two components separate the main
# item types.

# Reduced dataset.
pca_reduced <- pca$x[, 1:n_pc]

# Comparing dimensions before and after PCA
# This will help me to show how many variables were reduced while
# retaining the selected amount of information.
dim(pca_data)     # before PCA
dim(pca_reduced)  # after PCA