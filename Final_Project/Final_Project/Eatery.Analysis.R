##-------------------------------------------------
## Aleksey Kramer
## Data Science 350
## Final Project
## Analysis of Seattle Restaurant Inspection Data
##-------------------------------------------------

require(logging)
require(nortest)

# Set working directory (Working on 3 different computers)
# setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_350\\FinalTWITProject')
setwd('C:\\Users\\db345c\\Desktop\\UW_TRAIN\\Data Science 350\\Final_Project')
# setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/FinalTWITProject')

if(interactive()) {
    
    # Setup Test Logger
    basicConfig()
    addHandler(writeToFile, file="FINAL_PROJECT-LOG.txt", level='DEBUG')
    
    # Filename to save
    f_name <- "restaurant.csv"

    if(!file.exists(f_name)) {
      # URL to download data from
      url <- "https://data.kingcounty.gov/api/views/pph9-v8tz/rows.csv?accessType=DOWNLOAD"
  
      # Download and save the data
      
      # For Mac
      # loginfo("Downloading data file")
      # download.file(url,destfile = f_name, method = "curl")
      # loginfi("Data file downloaded")
  
      # For Windows
      loginfo("Downloading data file")
      download.file(url,destfile = f_name)
      loginfi("Data file downloaded")
      
      # Remove url variable
      loginfo("Removing unused url variable")
      rm(url)
    }

    # Read data file
    r_data <- read.csv(f_name, stringsAsFactors = FALSE, na.strings = "")
    loginfo("Read data file in memory")
    
    # Remove f_name variable
    rm(f_name)
    loginfo("Removed unused f_name filename variable")

    # Look at the names
    names(r_data)
    loginfo("Looked at the column names")

    # Removing Phone data because it is irrelevant and incomplete
    r_data$Phone <- NULL
    loginfo("Removed sparcely populated Phone field")

    # Isolate Seattle only
    r_data <- r_data[r_data$City == "Seattle",]
    loginfo("Isolated Seattle's data only")

    # Remove City filed (not needed any more)
    r_data$City <- NULL
    loginfo("Removed City field - all data pertains to Seattle at this point")

    # Check the dimension of the data
    dim(r_data)

    # Replace all N/A valuses in with "none" to save the records
    loginfo("Beting subsetting data with 'none'")
    r_data$Violation.Type[is.na(r_data$Violation.Type)] <- "none"
    r_data$Violation.Description[is.na(r_data$Violation.Description)] <- "none"
    r_data$Violation_Record_ID[is.na(r_data$Violation_Record_ID)] <- "none"
    loginfo("Done subsetting data with 'none")

    # Convert date field to Date Type (just in case)
    loginfo("Confert Date field to be a Date type to look good")
    r_data$Inspection.Date <- as.Date(r_data$Inspection.Date, format = "%m/%d/%Y")

    # Histograms of Longtitude and Lattitude
    loginfo("Visually examining Longitude and Latitude fields")
    hist(r_data$Latitude)
    hist(r_data$Longitude)

    # Check Longtitude and Lattitude variables for Normality
    loginfo("Beting running normality tests")
    var_longt <- ad.test(r_data$Longitude)
    var_latt <- ad.test(r_data$Latitude)
    loginfo("Done running normality tests")

    # Output p-value for Longtitute and Lattitude variables (both of them
    # are normally distributed).  Paremetric tests should be used.
    loginfo("Printing p-values of normality tests on the screen")
    print(paste('p-value for normality test of Longtitude variable is:', var_longt$p.value))
    print(paste('p-value for normality test of Lattitude variable is:', var_latt$p.value))

    # Cleanup workspace for the variables that will not be used any more
    loginfo("Cleanup of normality tests objects")
    rm(var_latt)
    rm(var_longt)
    
    # Boxplots of the data to be analyzed
    loginfo("Analyzing boxplots")
    boxplot(r_data$Longitude ~ r_data$Inspection.Typ)
    boxplot(r_data$Latitude ~ r_data$Inspection.Typ)

    # Longtitude vs. Inspection Type (Affect of Inspection type on Longtitude selection)
    loginfo("Run and display ANOVA test of effects of Inspection Type on Longitude selection ")
    longitude.aov1 <- aov(r_data$Longitude ~ r_data$Inspection.Type)
    summary(longitude.aov1)
    print(paste("longitude.aov1 p-value is", summary(longitude.aov1)[[1]][["Pr(>F)"]][1]))
    TukeyHSD(longitude.aov1)
    # The mean comparison with other means shows that Routine Inspection/Field Review-Return Inspection relationship
    # is not significant, while other mean comparisons are
    
    # Lattitude vs. Inspection Type (Affect of Inspection type on Lattitude selection)
    loginfo("Run and display ANOVA test of effects of Inspection Type on Latitude selection")
    lattitude.aov1 <- aov(r_data$Latitude ~ r_data$Inspection.Type)
    summary(lattitude.aov1)
    print(paste("lattitude.aov1 p-value is", summary(lattitude.aov1)[[1]][["Pr(>F)"]][1]))
    TukeyHSD(lattitude.aov1)
    # The mean comparison with other means shows that Routine Inspection/Field Review-Return Inspection relationship
    # is not significant, while other mean comparisons are
}
