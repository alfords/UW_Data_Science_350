## ---------------------------------------------------
## Aleksey Kramer
## Data Science 350
## Week 5 Homework (Lecture 4)
## ---------------------------------------------------

simple_lm <- function(x, y, title) {
    plot(x, y, pch = 16, main = title)
    best_fit_line <- lm(y ~ x)
    abline(best_fit_line)
    return(best_fit_line)
}

if(interactive()) {
    
    # Set working directory
    # setwd('C:/Users/Aleksey/Documents/School/UW_Data_Science/UW_Data_Science_350/Week5/homework')
    setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week5/homework')
    
    # Import libraries
    require(logging)

    # Setup Test Logger
    basicConfig()
    addHandler(writeToFile, file="testing.log", level='DEBUG')

    # Open data file for reading
    loginfo("Begin reading file")
    df <- read.csv("ChicagoDiabetesData.csv")
    loginfo("Completed reading file")

    # Sum all values in all the columns excluding the first row (hadings)
    loginfo("Sum up values by column")
    data_sums = apply(df[-1],2,sum)

    # Extract x and y values from the list of column names extracted with names() function
    loginfo("Extracting X-values - Crude Rate and Y-values - Hospitalizations")
    x_vals = data_sums[grepl('^Crude.Rate.[0-9]+$', names(data_sums), perl = TRUE)]
    y_vals = data_sums[grepl('^Hospitalizations.[0-9]+$', names(data_sums), perl = TRUE)]

    # Delta's for X and Y - derived using diff() function
    loginfo("Running diff() produce deltas of X and Y variables")
    x_diffs = diff(x_vals)
    y_diffs = diff(y_vals)
    
    # Produce simple graph (visual exploration of Curde Admission vs. Hospitalization)
    fit1 <- simple_lm(x_vals, y_vals, "Crude Admission vs. Hospitalization")
    summary(fit1)
    
    # Produce simple graph (visual exploration of Deltas of Cruda Admissin vs. Hospitalization)
    fit2 <- simple_lm(x_diffs, y_diffs, "Deltas: Crude Admission vs Hospitalization")
    summary(fit2)
}

#  Comment and/or print your findings at the end. (1-2 full paragraphs with full sentences).
#
#  Be sure to interpret the slopes for each model (Some good examples here: 
#           http://www.austincc.edu/mparker/1342/lessons/less5-8/interpret_slope.pdf)
#
#
#  Comment on the differences in R^2 between the two- what does this mean?
#
#
#
#
#
#
#
#
#
#
#

