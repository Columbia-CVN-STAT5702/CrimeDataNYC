#Note the "NYPD_Complaint_Data_Historic.csv" should be in the working directory when running this file.
#Note the "nyc_weather_data.csv" file should be in the working directory when running this file.
#Note the "nyc_moon_data.csv" file should be in the working directory when running this file.

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

###############################################################################
#Read in moon phase data
moon_data <- fread("nyc_moon_data.csv", na.strings="", select = c("date", "phase"), stringsAsFactors = FALSE)
moon_data$date <- as.Date(moon_data$date, format='%m/%d/%Y')
moon_data$phase <- as.factor(moon_data$phase)
full_moon_data <- moon_data %>% filter(phase == "Full Moon")

#Merge the moon phase data into the main data frame
crime_df <- crime_df %>% left_join(moon_data, by = c("DateStart" = "date"))

#Generate plot to compare average number of crimes on rainy days as compared to non-rainy days
rainy_day_summary <- crime_df %>% filter(PRCP > 0.50) %>% group_by(Pct) %>% summarize(count_rain = n(), n_days_rain = n_distinct(DateStart), Rain = count_rain/n_days_rain) %>% drop_na() %>% select(Pct, Rain)
no_rain_summary <- crime_df %>% filter(PRCP == 0.00) %>% group_by(Pct) %>% summarize(count_no_rain = n(), n_days_no_rain = n_distinct(DateStart), NoRain = count_no_rain/n_days_no_rain) %>% drop_na() %>% select(Pct, NoRain)
#Merge the two summary tables together
weather_summary <- no_rain_summary  %>% left_join(rainy_day_summary, by = c("Pct" = "Pct"))
weather_summary$Pct <- as.factor(weather_summary$Pct)
#Tidy the summary table
tidy_weather_summary <- weather_summary %>% gather(key = "Weather", value = "AvgDailyReports", -Pct)
#Use a cleveland dot plot to show difference in averge daily crime on rainy days vs. non-rainy days
ggplot(tidy_weather_summary, aes(x = Pct, y = AvgDailyReports, color = Weather)) + geom_point() + coord_flip() + ggtitle("Affect of Precipitation on Crime Reports")
#Plot the difference in average crimes per day on a non-rainy day and average crimes per day on a rainy day for each precinct.
weather_summary_diff <- weather_summary %>% mutate(Delta = Rain - NoRain)
ggplot(weather_summary_diff, aes(x = Pct, y = Delta)) + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Difference in Average Number of Crimes per Day when Raining")

#Generate plot to compare averge number of crimes on full moon days as compared to non-full moon days
crime_summary_reg <- crime_df %>% filter(phase != "Full Moon") %>% group_by(Pct) %>% summarize(count_reg = n(), n_days_reg = n_distinct(DateStart), Avg_Reg = count_reg/n_days_reg) %>% drop_na() %>% select(Pct, Avg_Reg)
crime_summary_fm <- crime_df %>% filter(phase == "Full Moon") %>% group_by(Pct) %>% summarize(count_fm = n(), n_days_fm = n_distinct(DateStart), Avg_Fm = count_fm/n_days_fm) %>% drop_na() %>% select(Pct, Avg_Fm)
#Merge the summary tables together
fm_summary <- crime_summary_reg %>% left_join(crime_summary_fm, by = c("Pct" = "Pct"))
fm_summary$Pct <- as.factor(fm_summary$Pct)
#Tidy the summary table
tidy_fm_summary <- fm_summary %>% gather(key = "Moon Phase", value = "AvgDailyReports", -Pct)
#Cleveland dot plot to compare crime rate on full moon days
ggplot(tidy_fm_summary, aes(x = Pct, y = AvgDailyReports, color = `Moon Phase`)) + geom_point() + coord_flip() + ggtitle("Affect of Full Moon on Crime Reports")
#Plot the difference in average crimes per day on a Full Moon day and average crimes per day on a non-Full Moon for each precinct.
fm_summary_diff <- fm_summary %>% mutate(Delta = Avg_Fm - Avg_Reg)
ggplot(fm_summary_diff, aes(x = Pct, y = Delta)) + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Difference in Average Number of Crimes per Day when There is a Full Moon")

#Generate plot to compare average number of crimes on cold days as compared to hot days
hot_day_summary <- crime_df %>% filter(TMAX > 90) %>% group_by(Pct) %>% summarize(count_hot = n(), n_days_hot = n_distinct(DateStart), Hot = count_hot/n_days_hot) %>% drop_na() %>% select(Pct, Hot)
cold_day_summary <- crime_df %>% filter(TMAX < 20) %>% group_by(Pct) %>% summarize(count_cold = n(), n_days_cold = n_distinct(DateStart), Cold = count_cold/n_days_cold) %>% drop_na() %>% select(Pct, Cold)
#Merge the two summary tables together
temp_summary <- hot_day_summary  %>% left_join(cold_day_summary, by = c("Pct" = "Pct"))
temp_summary$Pct <- as.factor(temp_summary$Pct)
#Tidy the summary table
tidy_temp_summary <- temp_summary %>% gather(key = "Temp", value = "AvgDailyReports", -Pct)
#Use a cleveland dot plot to show difference in averge daily crime on rainy days vs. non-rainy days
ggplot(tidy_temp_summary, aes(x = Pct, y = AvgDailyReports, color = Temp)) + geom_point() + scale_color_manual(values=c("Blue", "Red")) + coord_flip() + ggtitle("Affect of Temperature on Crime Reports")
#Plot the difference in average crimes per day on a cold days and average crimes per day on a hot days for each precinct.
temp_summary_diff <- temp_summary %>% mutate(Delta = Cold - Hot)
ggplot(temp_summary_diff, aes(x = Pct, y = Delta)) + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Difference in Average Number of Crimes per Day on Cold Days when compared to Hot Days")

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


##################################
#Stop and Frisk Data
#Read in data on the total annual stop and frist incidents in NYC
sqf <- fread("StopAndFrisk.csv")
sqf$Year <- as.factor(sqf$Year)
sqf$Stops <- as.numeric(sqf$Stops)

#Create a grouped summary dataframe for total number of each offence for every year
data4 <- crime_df %>% mutate(Year = year(DateReport)) %>% group_by(Year, OffenseDesc) %>% summarize(count = n()) %>% drop_na()
data5 <- crime_df %>% mutate(Year = year(DateReport)) %>% group_by(Year, Level) %>% summarize(count = n()) %>% drop_na()

#Loop through each Offense and determine the correlation with sqf data
iterations = 70
variables = 2

output <- matrix(ncol=variables, nrow=iterations)
for (i in 1:iterations) {
  output[i,1] <- levels(data4$OffenseDesc)[i]
  data_offense <- data4 %>% filter(OffenseDesc == levels(data4$OffenseDesc)[i])
  if (nrow(data_offense) == 11){
    correlation = cor(data_offense$count, sqf$Stops)
  }
  else {
    correlation = ""
  }
  output[i,2] <- correlation
}

offense_corr_df <- data.frame("OffenseDesc" = output[,1], "Correlation" = output[,2])
offense_corr_df <- offense_corr_df %>% drop_na() %>% arrange(desc(Correlation))

iterations = 3
variables = 2

output2 <- matrix(ncol=variables, nrow=iterations)
for (i in 1:iterations) {
  output2[i,1] <- levels(data5$Level)[i]
  data_offense2 <- data5 %>% filter(Level == levels(data5$Level)[i])
  if (nrow(data_offense2) == 11){
    correlation = cor(data_offense2$count, sqf$Stops)
  }
  else {
    correlation = ""
  }
  output2[i,2] <- correlation
  
  #Generate linear model for each Level
  if (i == 1) {
    lm_m <- lm(data_offense2$count ~sqf$Stops)
    plot(sqf$Stops, data_offense2$count, main = "Misdemeneor")
    abline(lm_m)
  }
  
  if (i == 2) {
    lm_f <- lm(data_offense2$count ~sqf$Stops)
    plot(sqf$Stops, data_offense2$count, main = "Felony")
    abline(lm_f)
  }

  if (i == 3) {
    lm_v <- lm(data_offense2$count ~sqf$Stops)
    plot(sqf$Stops, data_offense2$count, main = "Violation")
    abline(lm_v)
  }
  
}

level_corr_df <- data.frame("Level" = output2[,1], "Correlation" = output2[,2])
level_corr_df <- level_corr_df %>% drop_na() %>% arrange(desc(Correlation))
