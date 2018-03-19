#Note the "NYPD_Complaint_Data_Historic.csv" should be in the working directory when running this file.

library(dplyr)
library(tidyr)
library(ggplot2)
library(vcdExtra)
library(data.table)

########################################################################################
var_names <- c("Id", "DateStart", "TimeStart", "DateEnd", "TimeEnd", "DateReport", "ClassCode", "OffenseDesc", 
               "IntClassCode", "IntOffenseDesc", "AtptCptdStatus", "Level", "Jurisdiction", "Boro", "Pct", "LocOfOccr", "PremDesc", 
               "ParkName", "HousingDevName", "XCoord", "YCoord", "Lat", "Long", "Lat_Long")

crime_df <- fread("NYPD_Complaint_Data_Historic.csv",na.strings="", col.names = var_names, stringsAsFactors = TRUE)

#Convert dates and times to correct format
crime_df$DateStart <- as.Date(crime_df$DateStart, format='%m/%d/%Y')
crime_df$DateEnd <- as.Date(crime_df$DateEnd, format='%m/%d/%Y')
crime_df$DateReport <- as.Date(crime_df$DateReport, format='%m/%d/%Y')

########################################################################################
#Read in weather data from file
weather_select = c("DATE", "AWND", "PRCP", "SNOW", "TMAX")
weather_data <- fread("nyc_weather_data.csv", na.strings="", select = weather_select, stringsAsFactors = FALSE)
weather_data$DATE <- as.Date(weather_data$DATE)
weather_data$AWND <- as.numeric(weather_data$AWND)
weather_data$PRCP <- as.numeric(weather_data$PRCP)
weather_data$SNOW <- as.numeric(weather_data$SNOW)
weather_data$TMAX <- as.numeric(weather_data$TMAX)

#Merge the data together
crime_df <- crime_df[weather_data, on=.(DateStart = DATE)]

rainy_day_summary <- crime_df %>% filter(PRCP > 0.00) %>% group_by(Pct) %>% summarize(count_rain = n(), n_days_rain = n_distinct(DateStart), Rain = count_rain/n_days_rain) %>% drop_na() %>% select(Pct, Rain)
no_rain_summary <- crime_df %>% filter(PRCP == 0.00) %>% group_by(Pct) %>% summarize(count_no_rain = n(), n_days_no_rain = n_distinct(DateStart), NoRain = count_no_rain/n_days_no_rain) %>% drop_na() %>% select(Pct, NoRain)
#Merge the two summary tables together
weather_summary <- no_rain_summary  %>% left_join(rainy_day_summary, by = c("Pct" = "Pct"))
weather_summary$Pct <- as.factor(weather_summary$Pct)
#Tidy the summary table
tidy_weather_summary <- weather_summary %>% gather(key = "Weather", value = "AvgDailyReports", -Pct)
#Plot precincts on rainy days
ggplot(tidy_weather_summary, aes(x = Pct, y = AvgDailyReports, color = Weather)) + geom_point() + coord_flip() + ggtitle("Affect of Precipitation on Crime Reports")

#Plot histogram of incidents by police precinct
data_pct <- crime_df %>% group_by(Pct) %>% summarize(count = n()) %>% drop_na()
ggplot(data_pct, aes(reorder(Pct, count), count)) + geom_bar(stat = "identity") + xlab("Precint Number") + ggtitle("Incidents by Precinct") + coord_flip()

#Plot mosaic of the Category of crime with bororgh
data1 <- crime_df %>% select(Level, Boro) %>% filter(Boro != "")
counts <- data1 %>% group_by(Boro, Level) %>% summarize(Freq=n())
vcd::mosaic(Level ~ Boro, counts, direction = c("v", "h"), rot_labels=c(90,90,0,0))

#Plot X,Y location of data
start_date <- as.Date("2012-10-01")
stop_date <- as.Date("2012-10-31")
data2 <- crime_df %>% filter(DateReport >= start_date, DateReport <= stop_date) %>% select(Level, Boro, Lat, Long) %>% drop_na() 
ggplot(data2, aes(x = Long, y = Lat, color = Level)) + geom_point(alpha = 0.3) + geom_density_2d(bins = 7)

#Plot number of Felony, Misdemeanor, Violation over time
start_date <- as.Date("2012-01-01")
stop_date <- as.Date("2012-10-31")
data3 <- crime_df %>% filter(DateReport >= start_date, DateReport <= stop_date) %>% select(DateReport, Level) %>% drop_na() %>% mutate(month_year = format(DateReport, "%m/%Y")) %>% group_by(month_year, Level) %>% summarize(total = n())
ggplot(data3, aes(x = month_year, y = total)) + geom_col() + facet_grid(~Level) + coord_flip()
