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

setwd("C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_350\\Week4\\homework4")

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
  
  ##-----Probably do some data exploration----
  
  ##----Perform a test for equal representation-----
  
  ##----Access the farms/state data-----
  
  ##----Derive the weights for each state----
  
  ##----Perform a test for equal repreentation by farms/state-----
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
}




