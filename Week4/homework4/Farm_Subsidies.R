##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 3
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)

##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#     Note- you can find the farms per state in census data.
#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----

setwd("/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week4/homework4")

getFarmPayments <- function() {
  if(!file.exists("CAS.WDC11019.PMT11.FINAL.DT11186.TXT")) {
    zip_file_url <- "http://www.fsa.usda.gov/Internet/FSA_File/2011_payment_data.zip"
    zip_file_name <- "downloaded_farm_payments.zip"
    download.file(zip_file_url, zip_file_name)
    unzip(zip_file_name, "CAS.WDC11019.PMT11.FINAL.DT11186.TXT")
    data <- read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";", header=FALSE, stringsAsFactors=FALSE)
    unlink(zip_file_name)
    return(data)
  } else {
    data <- read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";", header=FALSE, stringsAsFactors=FALSE)
    return(data)
  }
}

getStateCodes <- function() {
  if(!file.exists("foia_state_county_codes-1.xls")) {
    xls_file_url <- "http://www.fsa.usda.gov/Internet/FSA_File/foia_state_county_codes.xls"
    xls_file_name <- "foia_state_county_codes-1.xls"
    download.file(xls_file_url, xls_file_name, mode="wb")
    
    if("gdata" %in% rownames(installed.packages()) == FALSE) {
      install.packages("gdata")
    }
    
    library(gdata)
    state_data <- read.xls (xls_file_name, sheet = 1, header = TRUE)
    return(state_data)
  } else {
    if("gdata" %in% rownames(installed.packages()) == FALSE) {
      install.packages("gdata")
    }
    
    library(gdata)
    state_data <- read.xls ("foia_state_county_codes-1.xls", sheet = 1, header = TRUE)
    return(state_data)
  }
}


##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="testing.log", level='DEBUG')
  
  # Get Farm Payments file
  loginfo("Getting Farm Payments Data")
  data <- getFarmPayments()
  
  # Assign column names to Farm Payments data
  loginfo("Assigning column names to Farm Payments data")
  names(data) <- c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##----Trim Whitespaces-----
  loginfo("Creating data from from Payments DAta")
  data <- as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  ##------Read State/County File-----
  loginfo("Constructing fields to merge payments and state data")
  county_state_codes = getStateCodes()
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  loginfo("Getting state codes")
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  
  ##----Write data to sqllite database-----
  loginfo("Creating temprary database (removing if exists)")
  db_name = 'data.db'
  if (file.exists(db_name)){
      unlink(db_name)
  }
  
  # Connect to sqlite database
  con = dbConnect(dbDriver("SQLite"), db_name)
  
  # Skip writing to database if already written
  loginfo("Write data frame to the database")
  dbWriteTable(con, "data", data)
  
  # Create query to aggregate by state: sum of ammounts paid and number of farms
  query = 'SELECT ST state, sum(amount) amount, count(farm_num) num_of_tax_ids FROM DATA group by ST'
  loginfo(paste("Executing query:", query))
  rs = dbSendQuery(con, query)
  
  # Create data fram from the result set and close db connection
  loginfo("Creating aggredated.df for acutal data analyis")
  aggregated.df <- fetch(rs)
  
  # closing dabase suprssing warnings
  loginfo("Removing sqlite database file")
  suppressWarnings(dbDisconnect(con))
  unlink("data.db")
  
  ##-----Probably do some data exploration----
  
  length(aggregated.df)
  length(aggregated.df[,1])
  summary(aggregated.df$amount)
  
  # There appers to be a linear relationship between the number of farms
  # and the amount of subsidies received (in millions) with two two outliers
  # who received more than 50 millions in subsidies. Need to run further tests to confirm
  loginfo("Running visual analysis")
  plot(aggregated.df$amount/1000000~aggregated.df$num_of_tax_ids,
    xlab="Number of Customers",
    ylab="Ammount (in Millions)")
  abline(lm(aggregated.df$amount/1000000 ~ aggregated.df$num_of_tax_ids))
  
  ##----Forcing to use decimal point
  options("scipen"=100, "digits"=4)
  
  ##----Perform a test for equal representation-----
  loginfo("Executiing test for equal representation")
  weighted_probs = 1/50
  chi.result_equal <- chisq.test(aggregated.df$amount, p = rep(weighted_probs, 50)) 
  print("========== Testing for equal representation ==========")
  print(chi.result_equal)

  ##----Derive the weights for each state----
  state_weighted_probs <- aggregated.df$num_of_tax_ids / sum(aggregated.df$num_of_tax_ids)
  
  ##----Perform a test for equal repreentation by farms/state-----
  loginfo("Executing tests for weighted representation")
  chi.result_weighted <- chisq.test(aggregated.df$amount, p = state_weighted_probs)
  print("========== Testing for weighted farms/state representation ==========")
  print(chi.result_weighted)
}




