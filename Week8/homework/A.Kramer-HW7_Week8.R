# Homework 7
# Hint

setwd('C:\\Users\\db345c\\Desktop\\UW_TRAIN\\Week8\\homework')


##-----Load Libraries-----
library(dplyr)
library(data.table)

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----
rm(weather)
rm(numeric_cols)

# Creating time factors
headcount$day_count = as.numeric(headcount$DateFormat - min(headcount$DateFormat))
headcount$week_count = floor(headcount$day_count/7.0)
headcount$month_count = floor(headcount$day_count/30.5)

headcount$month = as.numeric(format(headcount$DateFormat, format = "%m"))
headcount$season = floor((headcount$month - 1) / 3)
headcount$weekday = as.numeric(format(headcount$DateFormat, format = "%w"))
headcount$week = as.numeric(format(headcount$DateFormat, format = "%W"))

#---------------------------------------------

# Create temperature model
temp_model <- lm(temp ~ . - DateFormat, data = headcount)
summary(temp_model)

# Date vs. Temperature plot (85%)
plot(headcount$DateFormat, headcount$temp, type="l", lwd=2, main="Temperature", xlab="Date", ylab="Temp")
lines(headcount$DateFormat, temp_model$fitted.values, lwd=2, lty=8, col="green")

# Report Residual Standard Error
print("-----------------------------------------------------------")
print(paste("Residual Standard error for temp_model is", summary.lm(temp_model)$sigma))

#---------------------------------------------

# Create dew point model (75%)
dew_model <- lm(dew_pt ~ . - DateFormat, data = headcount)
summary(dew_model)

# Date vs. Due point plot
plot(headcount$DateFormat, headcount$dew_pt, type="l", lwd=2, main="Dew Point", xlab="Date", ylab="Dew_pt")
lines(headcount$DateFormat, dew_model$fitted.values, lwd=2, lty=8, col="red")

# Report Residual Standard Error
print("---------------------------------------------------------")
print(paste("Residual Standard error for dew_model is", summary.lm(dew_model)$sigma))

##------------ Writeup ------------------------
# Residual standard error for temp_mode is 7.26
# Residual standard errof for dew_model is 6.73
# Basically, residual standard error (s) represents standard deviation of the residuals 
# and hep us estimate how close the fit is to the actual points.  0 is considered to be 
# a perfect fit (normally due to over-fitting). Both models above are pretty good, but 
# based on the residual standard error - there is still room for improvement.

