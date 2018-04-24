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

#Generate violent crime dataframe
#filter for violent crime
violent_crime_df <- crime_df %>% filter(OffenseDesc == "ASSAULT 3 & RELATED OFFENSES" | 
                                          OffenseDesc == "FELONY ASSAULT" | 
                                          OffenseDesc == "MURDER & NON-NEGL. MANSLAUGHTER" |  
                                          OffenseDesc == "RAPE" |
                                          OffenseDesc == "ROBBERY" |
                                          IntOffenseDesc == "AGGRAVATED SEXUAL ASBUSE" |
                                          IntOffenseDesc == "ASSAULT 2,1,UNCLASSIFIED" |
                                          IntOffenseDesc == "ASSAULT 3" |
                                          IntOffenseDesc == "RAPE 1" |
                                          IntOffenseDesc == "ROBBERY,OPEN AREA UNCLASSIFIED" |
                                          IntOffenseDesc == "SEXUAL ABUSE" |
                                          IntOffenseDesc == "SEXUAL ABUSE 3,2")


#Generate plot to compare average number of crimes on rainy days as compared to non-rainy days
rainy_day_summary <- crime_df %>% filter(PRCP > 0.50) %>% group_by(Pct) %>% summarize(count_rain = n(), n_days_rain = n_distinct(DateStart), Rain = count_rain/n_days_rain) %>% drop_na() %>% select(Pct, Rain)
no_rain_summary <- crime_df %>% filter(PRCP == 0.00) %>% group_by(Pct) %>% summarize(count_no_rain = n(), n_days_no_rain = n_distinct(DateStart), NoRain = count_no_rain/n_days_no_rain) %>% drop_na() %>% select(Pct, NoRain)
#Merge the two summary tables together
weather_summary <- no_rain_summary  %>% left_join(rainy_day_summary, by = c("Pct" = "Pct"))
weather_summary$Pct <- as.factor(weather_summary$Pct)
#Tidy the summary table
tidy_weather_summary <- weather_summary %>% gather(key = "Weather", value = "AvgDailyReports", -Pct)
weather_summary_diff <- weather_summary %>% mutate(Delta = Rain - NoRain)

weather_summary_arr <- weather_summary_diff %>% arrange(NoRain)
weather_summary_arr <- weather_summary_arr %>% mutate(Pct = factor(Pct, Pct))
ggplot(weather_summary_arr, aes(group=1)) + geom_step(aes(x=Pct, y=Rain, color="Rain"), size=1) + 
  geom_step(aes(x=Pct, y=NoRain, color="No Rain"), size=1) + 
  scale_color_discrete(name = "Precipitation") + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x= "Precinct", y = "Average Number of Crimes per Day")

#Generate plot to compare averge number of crimes on full moon days as compared to non-full moon days
crime_summary_reg <- crime_df %>% filter(phase != "Full Moon") %>% group_by(Pct) %>% summarize(count_reg = n(), n_days_reg = n_distinct(DateStart), Avg_Reg = count_reg/n_days_reg) %>% drop_na() %>% select(Pct, Avg_Reg)
crime_summary_fm <- crime_df %>% filter(phase == "Full Moon") %>% group_by(Pct) %>% summarize(count_fm = n(), n_days_fm = n_distinct(DateStart), Avg_Fm = count_fm/n_days_fm) %>% drop_na() %>% select(Pct, Avg_Fm)
#Merge the summary tables together
fm_summary <- crime_summary_reg %>% left_join(crime_summary_fm, by = c("Pct" = "Pct"))
fm_summary$Pct <- as.factor(fm_summary$Pct)
#Tidy the summary table
tidy_fm_summary <- fm_summary %>% gather(key = "Moon Phase", value = "AvgDailyReports", -Pct)
fm_summary_diff <- fm_summary %>% mutate(Delta = Avg_Fm - Avg_Reg)

fm_summary_arr <- fm_summary_diff %>% arrange(Avg_Reg)
fm_summary_arr <- fm_summary_arr %>% mutate(Pct = factor(Pct, Pct))
ggplot(fm_summary_arr, aes(group=1)) + geom_step(aes(x=Pct, y=Avg_Reg, color="No Full Moon"), size=1) + 
  geom_step(aes(x=Pct, y=Avg_Fm, color="Full Moon"), size=1) + 
  scale_color_discrete(name = "Moon Phase") + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x= "Precinct", y = "Average Number of Crimes per Day")


#Generate plot to compare average number of crimes on cold days as compared to hot days
hot_day_summary <- crime_df %>% filter(TMAX > 90) %>% group_by(Pct) %>% summarize(count_hot = n(), n_days_hot = n_distinct(DateStart), Hot = count_hot/n_days_hot) %>% drop_na() %>% select(Pct, Hot)
cold_day_summary <- crime_df %>% filter(TMAX < 20) %>% group_by(Pct) %>% summarize(count_cold = n(), n_days_cold = n_distinct(DateStart), Cold = count_cold/n_days_cold) %>% drop_na() %>% select(Pct, Cold)
#Merge the two summary tables together
temp_summary <- hot_day_summary  %>% left_join(cold_day_summary, by = c("Pct" = "Pct"))
temp_summary$Pct <- as.factor(temp_summary$Pct)
#Tidy the summary table
tidy_temp_summary <- temp_summary %>% gather(key = "Temp", value = "AvgDailyReports", -Pct)
temp_summary_diff <- temp_summary %>% mutate(Delta = Cold - Hot)

temp_summary_arr <- temp_summary_diff %>% arrange(Hot)
temp_summary_arr <- temp_summary_arr %>% mutate(Pct = factor(Pct, Pct))
cols <- c("Hot Days" = "red", "Cold Days" = "blue")
ggplot(temp_summary_arr, aes(group=1)) + geom_step(aes(x=Pct, y=Hot, color="Hot Days"), size=1) + 
  geom_step(aes(x=Pct, y=Cold, color="Cold Days"), size=1) + 
  scale_color_manual(name = "Temperature", values = cols) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x= "Precinct", y = "Average Number of Crimes per Day")


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

#Leaflet Interactive Work
library(leaflet)
library(geojsonio)
library(htmltools)

#Read in geojson data
nyc_precincts <- geojsonio::geojson_read("nyc_precinct.geojson", what = "sp")

#Plot data
leaflet(nyc_precincts) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.3, fillColor = "blue", label = ~paste0("Pct: ", formatC(nyc_precincts@data[["Precinct"]])))





#New edits
#Generate scatterplot of crime vs precipitiation
rain_summary_per_day <- crime_df %>% group_by(DateStart, Level) %>% summarize(Count = n()) %>% drop_na()
#append weather data
rain_summary_per_day <- rain_summary_per_day %>% left_join(weather_data, by = c("DateStart" = "DATE")) %>% select(DateStart, Level, Count, PRCP)
#Scatter plot of daily crimes vs. precipitation level

#Filter on Level of crime and generate linear model for each
# linear model: Felonies
f_df_rain <- rain_summary_per_day %>% filter(Level=="FELONY")
flm_rain <- lm(Count~PRCP, f_df_rain)
# linear model: Misdemeanors
m_df_rain <- rain_summary_per_day %>% filter(Level=="MISDEMEANOR")
mlm_rain <- lm(Count~PRCP, m_df_rain)
# linear model: Violation
v_df_rain <- rain_summary_per_day %>% filter(Level=="VIOLATION")
vlm_rain <- lm(Count~PRCP, v_df_rain)

#Plot data for all three crime levels vs precipitation with linear model results
ggplot(rain_summary_per_day, aes(x=PRCP, y=Count, color=Level)) + 
  geom_point(alpha=0.3) +
  geom_abline(slope=flm_rain[["coefficients"]][["PRCP"]],intercept=flm_rain[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 3, y=420, label=paste0("y=",round(flm_rain[["coefficients"]][["PRCP"]],2),"x+",round(flm_rain[["coefficients"]][["(Intercept)"]],0))) +
  geom_abline(slope=mlm_rain[["coefficients"]][["PRCP"]],intercept=mlm_rain[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 3, y=750, label=paste0("y=",round(mlm_rain[["coefficients"]][["PRCP"]],2),"x+",round(mlm_rain[["coefficients"]][["(Intercept)"]],0))) +
  geom_abline(slope=vlm_rain[["coefficients"]][["PRCP"]],intercept=vlm_rain[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 3, y=180, label=paste0("y=",round(vlm_rain[["coefficients"]][["PRCP"]],2),"x+",round(vlm_rain[["coefficients"]][["(Intercept)"]],0))) +
  labs(x = "Precipitation [inches]", y = "Daily Crime Incident Count", title = "Daily Crime Counts vs. Precipitation by Level of Crime with Linear Models")

#Plot data only for violent crimes vs precipitation with linear model result
#Generate scatterplot of crime vs precipitiation
vc_df_rain <- violent_crime_df %>% group_by(DateStart) %>% summarize(Count = n()) %>% drop_na()
#append weather data
vc_df_rain <- vc_df_rain %>% left_join(weather_data, by = c("DateStart" = "DATE")) %>% select(DateStart, Count, PRCP)
#Scatter plot of daily crimes vs. precipitation level

#Generate linear model for Violent Crime vs. Precipitation
vclm_rain <- lm(Count~PRCP, vc_df_rain)

ggplot(vc_df_rain, aes(x=PRCP, y=Count)) + 
  geom_point(alpha=0.3) +
  geom_abline(slope=vclm_rain[["coefficients"]][["PRCP"]],intercept=vclm_rain[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 3.5, y=240, label=paste0("y=",round(vclm_rain[["coefficients"]][["PRCP"]],2),"x+",round(vclm_rain[["coefficients"]][["(Intercept)"]],0))) +
  labs(x = "Precipitation [inches]", y = "Daily Violent Crime Incident Count", title = "Daily Violet Crime Counts vs. Precipitation with Linear Models")



#New Plots for full moon analysis
moon_summary <- crime_df %>% 
  filter(phase == "Full Moon" | phase == "New Moon" | phase == "First Quarter" | phase == "Last Quarter") %>% 
  group_by(DateStart, phase) %>% summarize(Count = n()) %>% drop_na()
moon_avg_crime <- moon_summary %>% group_by(phase) %>% summarize(Avg_Count = weighted.mean(Count))
moon_total_crime <- moon_summary %>% group_by(phase) %>% summarize(Total_Count = sum(Count))
moon_phase_total_count <- sum(moon_total_crime$Total_Count)
moon_total_crime <- moon_total_crime %>% mutate(Pct = Total_Count/moon_phase_total_count)

#Create a pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

ggplot(data = moon_total_crime, aes(x="", y = Total_Count, fill = phase)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start=0) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = scales::percent(Pct)), position = position_stack(vjust = 0.5)) + 
  ggtitle("Moon Phase vs. Crime Count Analysis")

#Generate the same analysis based on Violent Crimes
vc_moon_summary <- violent_crime_df %>% 
  filter(phase == "Full Moon" | phase == "New Moon" | phase == "First Quarter" | phase == "Last Quarter") %>% 
  group_by(DateStart, phase) %>% summarize(Count = n()) %>% drop_na()
vc_moon_avg_crime <- vc_moon_summary %>% group_by(phase) %>% summarize(Avg_Count = weighted.mean(Count))
vc_moon_total_crime <- vc_moon_summary %>% group_by(phase) %>% summarize(Total_Count = sum(Count))
vc_moon_phase_total_count <- sum(vc_moon_total_crime$Total_Count)
vc_moon_total_crime <- vc_moon_total_crime %>% mutate(Pct = Total_Count/vc_moon_phase_total_count)

ggplot(data = vc_moon_total_crime, aes(x="", y = Total_Count, fill = phase)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start=0) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = scales::percent(Pct)), position = position_stack(vjust = 0.5)) + 
  ggtitle("Moon Phase vs. Violent Crime Count Analysis")

