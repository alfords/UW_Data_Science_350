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

setwd("F:/PCE_Data_Science/3_Outliers_MissingData_Hypothesis")

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
      install.packages("EXConnect")
    }
    
    library(gdata)
    state_data <- read.xls (xls_file_name, sheet = 1, header = TRUE)
    return(state_data)
  } else {
    if("gdata" %in% rownames(installed.packages()) == FALSE) {
      install.packages("EXConnect")
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
  data <- getFarmPayments()
  
  # Assign column names to Farm Payments data
  names(data) <- c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##----Trim Whitespaces-----
  data <- as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  ##------Read State/County File-----
  county_state_codes = getStateCodes()
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  
  ##----Write data to sqllite database (and test correctness)-----
  db_name = 'data.db'
  con = dbConnect(dbDriver("SQLite"), db_name)
  
  # Skip writing to database if already written
  if(!file.exists("data.db")) {
    dbWriteTable(con, "data", data)
  }
  
  # Create query to aggregate by state: sum of ammounts paid and number of farms
  query = 'SELECT ST state, sum(amount) amount, count(farm_num) num_of_tax_ids FROM DATA group by ST'
  rs = dbSendQuery(con, query)
  
  # Create data fram from the result set and close db connection
  aggregated.df <- fetch(rs)
  dbDisconnect(con)
  
  ##-----Probably do some data exploration----
  
  length(aggregated.df)
  length(aggregated.df[,1])
  summary(aggregated.df$amount)
  
  # There appers to be a linear relationship between the number of farms
  # and the amount of subsidies received (in millions) with two two outliers
  # who received more than 50 millions in subsidies
  plot(aggregated.df$amount/1000000~aggregated.df$num_of_tax_ids,
    xlab="Number of Customers",
    ylab="Ammount (in Millions)")
  
  ##----Perform a test for equal representation-----
  
  # Here we are going to test if all 50 states are represented equally in the data.
  #  If they are, the null hypothesis is that each state should have (1/50th) of occurence.
  
  weighted_prob = 1/50
  
  chi.result <- chisq.test(aggregated.df$amount, p = rep(weighted_prob,50))
  
  # We interpret the p-value as this:
  #   "There is a 'p' probability of observing our sample assuming they are all equally represented by $"
  #
  #   - since this p-value is VERY small, we know that they are different!  We reject the null.
  #
  #  For this test, you tested the hypotheses:
  #     Null:        All 50 states are equally represented in our sample by $subsidies given.
  #     Alternative: There exists at least one difference in how the states are represented.
  #
  #  This should not surprise us (Rhode Island subsidies << California subsidies)
  
  
  ##----Access the farms/state data-----
  
  # You've already done this above
  
  ##----Derive the weights for each state----
  
  # Now you want to perform the SAME test with different weights.  We want to weight the farms by, say, # of farms/state
  #  New weights:
  #     State A weight = (Number of Farms in State A) / (Total Number of Farms in US)
  
  
  ##----Perform a test for equal repreentation by farms/state-----
  
  
  # Perform the same chi-squared test with different weights.  What about acreage of farms? I think that's in our data somewhere.
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
}




