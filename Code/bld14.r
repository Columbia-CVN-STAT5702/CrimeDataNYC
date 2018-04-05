setwd("/Volumes/FactoryUsers/Users/bdaniel/Dropbox/Columbia Video Network/2018 a Data Visualization/CrimeDataNYC/CrimeData")

library(data.table)
crime_df <- fread("NYPD_Complaint_Data_Historic.csv",na.strings="")

crime_df%>%mutate(CMPLNT_FR_DT=as.Date(CMPLNT_FR_DT,format='%m/%d/%Y'),
                  CMPLNT_TO_DT=as.Date(CMPLNT_TO_DT,format='%m/%d/%Y'),
                  RPT_DT=as.Date(RPT_DT,format='%m/%d/%Y'))->df
crime_df<-df

var_names <- c("Id", "DateStart", "TimeStart", "DateEnd", "TimeEnd", "DateReport", "ClassCode", "OffenseDesc", 
               "IntClassCode", "IntOffenseDesc", "AtptCptdStatus", "Level", "Jurisdiction", "Boro", "Pct", "LocOfOccr", "PremDesc", 
               "ParkName", "HousingDevName", "XCoord", "YCoord", "Lat", "Long", "Lat_Long")

colnames(crime_df)<-var_names

crime_df%>%mutate_if(is.character,funs(factor(.)))->crime_df

setwd("/Volumes/FactoryUsers/Users/bdaniel/Dropbox/Columbia Video Network/2018 a Data Visualization/CrimeDataNYC")
#Anita's code has this:
#df <-crime_df %>% drop_na()

# bring in Borough Population and massage it
bdf <- fread("Data_Files/BoroughPop.csv")
bdf <- bdf[1:6,]
bdf$Boro <- c("TOTAL","BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")


library(dplyr)
# summarize for mosaic, per capita plots
df_bsum <-crime_df %>% 
  filter(!is.na(Boro)) %>%
  group_by(Boro,Level) %>% 
  summarize(Freq = n())

# merge in the borough population
df_bsum <- merge(df_bsum, bdf, by="Boro")

# per capita calculation
df_bsum$PerCap <-df_bsum$Freq/df_bsum$`2016 Estimate`

library(grid)
library(vcd)

# Mosaic By Count
mosaic(Level~Boro,df_bsum, direction=c("v","h"), main="Crime by Borough by Level")

# By Per Capita -- you have to have "Freq" be the column for the thing the Mosaic will use for frequency, so 
# for Per Capita, you need to swap the Freq column names
colnames(df_bsum)[colnames(df_bsum)=="Freq"] <- "Count"
colnames(df_bsum)[colnames(df_bsum)=="PerCap"] <- "Freq"
mosaic(Level~Boro,df_bsum, direction=c("v","h"), main="Crime per Capita by Borough by Level")


# simplfy column-names
var_names <- c("Id", "DateStart", "TimeStart", "DateEnd", "TimeEnd", "DateReport", "ClassCode", "OffenseDesc", 
               "IntClassCode", "IntOffenseDesc", "AtptCptdStatus", "Level", "Jurisdiction", "Boro", "Pct", "LocOfOccr", "PremDesc", 
               "ParkName", "HousingDevName", "XCoord", "YCoord", "Lat", "Long", "Lat_Long")
colnames(df) <- var_names


#Convert dates and times to correct format -- per Rich
df$DateStart <- as.Date(df$DateStart, format='%m/%d/%Y')
df$DateEnd <- as.Date(df$DateEnd, format='%m/%d/%Y')
df$DateReport <- as.Date(df$DateReport, format='%m/%d/%Y')

#need to rename the bdf Boro in order to make the merge work
colnames(bdf)[colnames(bdf)=="Boro"] <- "Boro"

# limit to specific years of the population data and test
# start with 2010
# summarize for mosaic, per capita plots
df_bsum2010 <-crime_df %>% 
  filter(!is.na(Boro)) %>%
  filter(DateStart > "2009-12-31" & DateStart < "2011-01-01") %>%
  group_by(Boro,Level) %>% 
  summarize(Freq = n())

# merge in the borough population
df_bsum2010 <- merge(df_bsum2010, bdf, by="Boro")


# per capita calculation 
df_bsum2010$PerCap <-df_bsum2010$Freq/df_bsum2010$`2010 Population`

#2010 mosaic
colnames(df_bsum2010)[colnames(df_bsum2010)=="Freq"] <- "Count"
colnames(df_bsum2010)[colnames(df_bsum2010)=="PerCap"] <- "Freq"
mosaic(Level~Boro,df_bsum2010, direction=c("v","h"), main="2010 Crime per Capita by Borough by Level")


# now 2016 Estimate
# summarize for mosaic, per capita plots
df_bsum2016 <-crime_df %>% 
  filter(!is.na(Boro)) %>%
  filter(DateStart > "2015-12-31" & DateStart < "2017-01-01") %>%
  group_by(Boro,Level) %>% 
  summarize(Freq = n())

# merge in the borough population
df_bsum2016 <- merge(df_bsum2016, bdf, by="Boro")

# per capita calculation
df_bsum2016$PerCap <-df_bsum2016$Freq/df_bsum2016$`2016 Estimate`


# By Per Capita -- you have to have "Freq" be the column for the thing the Mosaic will use for frequency, so 
# for Per Capita, you need to swap the Freq column names


#2016
colnames(df_bsum2016)[colnames(df_bsum2016)=="Freq"] <- "Count"
colnames(df_bsum2016)[colnames(df_bsum2016)=="PerCap"] <- "Freq"
mosaic(Level~Boro,df_bsum2016, direction=c("v","h"), main="2016 Crime per Capita by Borough by Level")


#Plot 2010 year over 2016 year by Borough
colnames(df_bsum2010)[colnames(df_bsum2010)=="Freq"] <- "PerCap10"
colnames(df_bsum2016)[colnames(df_bsum2016)=="Freq"] <- "PerCap16"
df_bsum.pcap <- merge(df_bsum2010,df_bsum2016, by=c("Boro","Level"))
df_bsum.pcap$Count.y <- NULL
df_bsum.pcap$Borough.y <- NULL
df_bsum.pcap$"2010 Population.y" <- NULL
df_bsum.pcap$"2016 Estimate.y" <- NULL

tidy_bsum <- tidyr::gather(df_bsum.pcap, key="Year", value="PerCap", -"Boro", -"Level.x", -"Count.x", -"2010 Population.x", -"2016 Estimate.x", -"Borough.x")

ggplot(tidy_bsum, aes(x=Year, y=PerCap, fill=Level))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_discrete(name="Year",
  #breaks=c(1, 2),
  labels=c("Felony", "Misdemeanor","Violation")) +
  xlab("Year")+ylab("Per Capita Crime") +
  facet_wrap(~Boro) +
  ggtitle("Per Capita Crime Rates by Level by Borough by Time")  


## leverage Rich's code for CRIME VS TEMPERATURE


#Read in weather data from file -- per Rich
weather_select = c("DATE", "AWND", "PRCP", "SNOW", "TMAX")
weather_data <- fread("Data_Files/nyc_weather_data.csv", na.strings="", select = weather_select, stringsAsFactors = FALSE)
weather_data$DATE <- as.Date(weather_data$DATE)
weather_data$AWND <- as.numeric(weather_data$AWND)
weather_data$PRCP <- as.numeric(weather_data$PRCP)
weather_data$SNOW <- as.numeric(weather_data$SNOW)
weather_data$TMAX <- as.numeric(weather_data$TMAX)

weather_data$DateStart <- as.Date(weather_data$DATE)

#Merge the data together -- per Rich
crime_df <- merge(crime_df,weather_data,by="DateStart")

### Relationship between max temp and crime volume
# set up the data by day and Level
daily_df <-crime_df %>% group_by(DateStart,Level) %>% summarize(CrimeCount=n(),MaxTemp=mean(TMAX))
# plot it -- well, plot it later after the linear models are run so we can see the linear slopes
library(ggplot2)
# daily_df %>% ggplot(aes(x=MaxTemp, y=CrimeCount, color=Level)) + geom_point()
# linear model: Felonies
f_df <- daily_df %>% filter(Level=="FELONY")
flm <- lm(CrimeCount~MaxTemp, f_df)
# linear model: Misdemeanors
m_df <- daily_df %>% filter(Level=="MISDEMEANOR")
mlm <- lm(CrimeCount~MaxTemp, m_df)
# linear model: Violation
v_df <- daily_df %>% filter(Level=="VIOLATION")
vlm <- lm(CrimeCount~MaxTemp, v_df)

# see shape of the daily counts... normal?
daily_df %>% ggplot(aes(CrimeCount)) +
  geom_histogram() +
  facet_wrap(~Level) +
  ggtitle("Histograms of Daily Crime Count by Level of Crime")

ggplot(daily_df, aes(x=CrimeCount)) +
  geom_density(aes(group=Level, color=Level, fill=Level), alpha=0.3) +
  ggtitle("Density Curves of Daily Crime Count by Level of Crime")

qqnorm(f_df$CrimeCount, main="Normal Q-Q Plot, Daily Felony Count", col="#F8766D") 
qqnorm(m_df$CrimeCount, main="Normal Q-Q Plot, Daily Misdemeanor Count", col="#00BA38") 
qqnorm(v_df$CrimeCount, main="Normal Q-Q Plot, Daily Violation Count", col="#619CFF") 

#replot the scatterplot with linear results
ggplot(daily_df, aes(x=MaxTemp, y=CrimeCount, color=Level)) + 
  geom_point(alpha=0.5) +
  geom_abline(slope=flm[["coefficients"]][["MaxTemp"]],intercept=flm[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 25, y=450, label=paste0("y=",round(flm[["coefficients"]][["MaxTemp"]],2),"x+",round(flm[["coefficients"]][["(Intercept)"]]),0)) +
  geom_abline(slope=mlm[["coefficients"]][["MaxTemp"]],intercept=mlm[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 25, y=770, label=paste0("y=",round(mlm[["coefficients"]][["MaxTemp"]],2),"x+",round(mlm[["coefficients"]][["(Intercept)"]]),0)) +
  geom_abline(slope=vlm[["coefficients"]][["MaxTemp"]],intercept=vlm[["coefficients"]][["(Intercept)"]]) +
  annotate("text", x= 25, y=200, label=paste0("y=",round(vlm[["coefficients"]][["MaxTemp"]],2),"x+",round(vlm[["coefficients"]][["(Intercept)"]]),0)) +
  ggtitle("Daily Crime Counts vs. Temperature by Level of Crime with Linear Models")

# print summaries
summary(flm)
summary(mlm)
summary(vlm)

# Residuals plots
plot(f_df$MaxTemp, resid(flm), 
     main="Residual Plot, Felonies vs. Max Temp", 
     xlab="Maximum Temperature", 
     ylab="Residuals of Crime Count", 
     col="#F8766D",
     abline(0,0))
plot(m_df$MaxTemp, resid(mlm), 
     main="Residual Plot, Misdemeanors vs. Max Temp", 
     xlab="Maximum Temperature", 
     ylab="Residuals of Crime Count", 
     col="#00BA38",
     abline(0,0))
plot(v_df$MaxTemp, resid(vlm), 
     main="Residual Plot, Violations vs. Max Temp", 
     xlab="Maximum Temperature", 
     ylab="Residuals of Crime Count", 
     col="#619CFF",
     abline(0,0))


## Explore time series for top Felony Categories

# start by extending Anita's Time Series analyses
library(lubridate)

# compare to Anita's Monthly graph
crime_time <-crime_df %>% filter(year(DateStart)>2005) %>%
  group_by(Date=floor_date(DateStart, "month"),Level) %>% summarize(count=n())
ggplot(crime_time, aes(Date,count, color=Level))+ geom_line() +
  ggtitle("Trend/Rate of Crimes in Each Category Across year - sampled month-wise")

#Check trend of months
crime_time$month <- month(crime_time$Date)
crime_time$monthabb <- as.factor(month.abb[crime_time$month])
levels(crime_time$monthabb) <-c(month.abb)

ggplot(crime_time, aes(Date,count, color=Level))+ geom_line() + facet_wrap(~monthabb) +
  ggtitle("Trend/Rate of Crimes in Each Category Across year - sampled month-wise")



