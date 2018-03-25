setwd("/Volumes/FactoryUsers/Users/bdaniel/Dropbox/Columbia Video Network/2018 a Data Visualization/CrimeDataNYC")
library(data.table)

fread("CrimeData/NYPD_Complaint_Data_Historic.csv",na.strings="",colClasses = c(PARKS_NM="c",HADEVELOPT="c"))->df

# bring in Borough Population and massage it
bdf <- fread("Data_Files/BoroughPop.csv")
bdf <- bdf[1:6,]
bdf$BORO_NM <- c("TOTAL","BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")


library(dplyr)
# summarize for mosaic, per capita plots
df_bsum <- df %>% 
  filter(!is.na(BORO_NM)) %>%
  group_by(BORO_NM,LAW_CAT_CD) %>% 
  summarize(Freq = n())

# merge in the borough population
df_bsum <- merge(df_bsum, bdf, by="BORO_NM")

# per capita calculation
df_bsum$PerCap <- df_bsum$Freq/df_bsum$`2016 Estimate`

library(grid)
library(vcd)

# Mosaic By Count
mosaic(LAW_CAT_CD~BORO_NM, df_bsum, direction=c("v","h"))


# By Per Capita -- you have to have "Freq" be the column for the thing the Mosaic will use for frequency, so 
# for Per Capita, you need to swap the Freq column names
colnames(df_bsum)[colnames(df_bsum)=="Freq"] <- "Count"
colnames(df_bsum)[colnames(df_bsum)=="PerCap"] <- "Freq"
mosaic(LAW_CAT_CD~BORO_NM, df_bsum, direction=c("v","h"))


## leverage Rich's code for CRIME VS TEMPERATURE
# simplfy column-names
var_names <- c("Id", "DateStart", "TimeStart", "DateEnd", "TimeEnd", "DateReport", "ClassCode", "OffenseDesc", 
               "IntClassCode", "IntOffenseDesc", "AtptCptdStatus", "Level", "Jurisdiction", "Boro", "Pct", "LocOfOccr", "PremDesc", 
               "ParkName", "HousingDevName", "XCoord", "YCoord", "Lat", "Long", "Lat_Long")
colnames(df) <- var_names


#Convert dates and times to correct format -- per Rich
df$DateStart <- as.Date(df$DateStart, format='%m/%d/%Y')
df$DateEnd <- as.Date(df$DateEnd, format='%m/%d/%Y')
df$DateReport <- as.Date(df$DateReport, format='%m/%d/%Y')

#Read in weather data from file -- per Rich
weather_select = c("DATE", "AWND", "PRCP", "SNOW", "TMAX")
weather_data <- fread("Data_Files/nyc_weather_data.csv", na.strings="", select = weather_select, stringsAsFactors = FALSE)
weather_data$DATE <- as.Date(weather_data$DATE)
weather_data$AWND <- as.numeric(weather_data$AWND)
weather_data$PRCP <- as.numeric(weather_data$PRCP)
weather_data$SNOW <- as.numeric(weather_data$SNOW)
weather_data$TMAX <- as.numeric(weather_data$TMAX)

#Merge the data together -- per Rich
df <- df[weather_data, on=.(DateStart = DATE)]

### Plot relationship between max temp and crime volume
# set up the data by day and Level
daily_df <- df %>% group_by(DateStart,Level) %>% summarize(CrimeCount=n(),MaxTemp=mean(TMAX))
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
#replot the scatterplot with linear results
ggplot(daily_df, aes(x=MaxTemp, y=CrimeCount, color=Level)) + 
  geom_point() +
  geom_abline(slope=flm[["coefficients"]][["MaxTemp"]],intercept=flm[["coefficients"]][["(Intercept)"]]) +
  geom_abline(slope=mlm[["coefficients"]][["MaxTemp"]],intercept=mlm[["coefficients"]][["(Intercept)"]]) +
  geom_abline(slope=vlm[["coefficients"]][["MaxTemp"]],intercept=vlm[["coefficients"]][["(Intercept)"]]) +
  ggtitle("Daily Crime Counts vs. Temperature by Level of Crime with Linear Models")

# see shape of the daily counts... normal?
daily_df %>% ggplot(aes(CrimeCount)) +
  geom_histogram() +
  facet_wrap(~Level)



qqnorm(f_df$CrimeCount, main="Normal Q-Q Plot, Daily Felony Count", col="#F8766D") 
qqnorm(m_df$CrimeCount, main="Normal Q-Q Plot, Daily Misdemeanor Count", col="#00BA38") 
qqnorm(v_df$CrimeCount, main="Normal Q-Q Plot, Daily Violation Count", col="#F8766D") 



