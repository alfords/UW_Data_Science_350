## ---------------------------------------------------
## Aleksey Kramer
## Data Science 350
## Week 5 Homework (Lecture 4)
## ---------------------------------------------------

# Set working directory
setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week5/homework')

# Import libraries
require(logging)

# Open data file for reading
df <- read.csv("ChicagoDiabetesData.csv")

# Sum all values in all the columns excluding the first row (hadings)
data_sums = apply(df[-1],2,sum)

# Extract x and y values from the list of column names extracted with names() function
x_vals = data_sums[grepl('^Crude.Rate.[0-9]+$', names(data_sums), perl = TRUE)]
y_vals = data_sums[grepl('^Hospitalizations.[0-9]+$', names(data_sums), perl = TRUE)]

# Calculate consecutive differences in the values of X and Y to reduce the scale
x_diffs = diff(x_vals)
y_diffs = diff(y_vals)
