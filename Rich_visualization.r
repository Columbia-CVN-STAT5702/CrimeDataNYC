#Note the "NYPD_Complaint_Data_Historic.csv" should be in the working directory when running this file.

library(tidyverse)

crime_data <- read.csv("NYPD_Complaint_Data_Historic.csv", header=TRUE)
#Convert dates and times to correct format
crime_data$CMPLNT_FR_DT <- as.Date(crime_data$CMPLNT_FR_DT, format='%m/%d/%Y')
crime_data$CMPLNT_TO_DT <- as.Date(crime_data$CMPLNT_TO_DT, format='%m/%d/%Y')
crime_data$RPT_DT <- as.Date(crime_data$RPT_DT, format='%m/%d/%Y')

#Plot histogram of incidents by police precinct
data0 <- crime_data %>% select(ADDR_PCT_CD) %>% drop_na()
ggplot(data0, aes(ADDR_PCT_CD)) + geom_histogram(binwidth = 1) + xlab("Precinct Number") + ggtitle("Incidents by Precinct")

#Plot mosaic of the Category of crime with bororgh
library(vcdExtra)
data1 <- crime_data %>% select(LAW_CAT_CD, BORO_NM) %>% filter(BORO_NM != "")
counts <- data1 %>% group_by(BORO_NM, LAW_CAT_CD) %>% summarize(Freq=n())
vcd::mosaic(LAW_CAT_CD ~ BORO_NM, counts, direction = c("v", "h"), rot_labels=c(90,90,0,0))

#Plot X,Y location of data
start_date <- as.Date("2012-10-01")
stop_date <- as.Date("2012-10-31")
data2 <- crime_data %>% filter(RPT_DT >= start_date, RPT_DT <= stop_date) %>% select(LAW_CAT_CD, BORO_NM, Latitude, Longitude) %>% drop_na() 
ggplot(data2, aes(x = Longitude, y = Latitude, color = LAW_CAT_CD)) + geom_point(alpha = 0.3) + geom_density_2d(bins = 7)

#Plot number of Felony, Misdemeanor, Violation over time
start_date <- as.Date("2012-01-01")
stop_date <- as.Date("2012-10-31")
data3 <- crime_data %>% filter(RPT_DT >= start_date, RPT_DT <= stop_date) %>% select(RPT_DT, LAW_CAT_CD) %>% drop_na() %>% mutate(month_year = format(RPT_DT, "%m/%Y")) %>% group_by(month_year, LAW_CAT_CD) %>% summarise(total = n())
ggplot(data3, aes(x = month_year, y = total)) + geom_col() + facet_grid(~LAW_CAT_CD) + coord_flip()
