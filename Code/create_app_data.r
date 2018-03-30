#This script creates the R data object that will be used by the interactive application
#It is a subset of the full dataset derived from the orginal data file

library(dplyr)
library(data.table)

########################################################################################
var_names <- c("Id", "DateStart", "TimeStart", "DateEnd", "TimeEnd", "DateReport", "ClassCode", "OffenseDesc", 
               "IntClassCode", "IntOffenseDesc", "AtptCptdStatus", "Level", "Jurisdiction", "Boro", "Pct", "LocOfOccr", "PremDesc", 
               "ParkName", "HousingDevName", "XCoord", "YCoord", "Lat", "Long", "Lat_Long")

crime_df <- fread("NYPD_Complaint_Data_Historic.csv",na.strings="", col.names = var_names, stringsAsFactors = TRUE)

#Convert dates and times to correct format
crime_df$DateStart <- as.Date(crime_df$DateStart, format='%m/%d/%Y')

NycAppData <- crime_df %>% select("DateStart", "Level", "Boro", "Pct", "OffenseDesc", "Lat", "Long")
NycAppData$Pct <- as.factor(NycAppData$Pct)

save(NycAppData, file = "NycAppData.RData")

#This .RData file can now be loaded int the application with the command:
# load("NycAppData.RData")