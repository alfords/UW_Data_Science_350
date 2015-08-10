##-------------------------------------------------
## Aleksey Kramer
## Data Science 350
## Final Project
## Analysis of Seattle Restaurant Inspection Data
##-------------------------------------------------

require(logging)

# Set working directory
setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_350\\FinalTWITProject')

# Filename to save
f_name <- "restaurant.csv"

if(!file.exists(f_name)) {
  # URL to download data from
  url <- "https://data.kingcounty.gov/api/views/pph9-v8tz/rows.csv?accessType=DOWNLOAD"
  
  # Download and save data
  download.file(url,destfile = f_name)
}

# Read data file
r_data <- read.csv(f_name, stringsAsFactors = FALSE, na.strings = "")

# Look at the names
names(r_data)

# Removing Phone data because it is irrelevant and incomplete
r_data$Phone <- NULL

# Isolate Seattle only
r_data <- r_data[r_data$City == "Seattle",]

# Remove City filed (not needed any more)
r_data$City <- NULL

# Check the dimension of the data
dim(r_data)

# Replace all N/A valuses in with "none" to save the records
r_data$Violation.Type[is.na(r_data$Violation.Type)] <- "none"
r_data$Violation.Description[is.na(r_data$Violation.Description)] <- "none"
r_data$Violation_Record_ID[is.na(r_data$Violation_Record_ID)] <- "none"

# Convert date field to Date Type
r_data$Inspection.Date <- as.Date(r_data$Inspection.Date, format = "%m/%d/%Y")

# Isolate year 2015 only
r_data <- r_data[r_data$Inspection.Date > '2014-12-31',]

# Try histograms on violation points (not good)
hist(r_data$Violation.Points)

# Try log of violation points (much better)
hist(log(r_data$Violation.Points))

