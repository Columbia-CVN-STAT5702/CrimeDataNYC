#Test script 
library(RSocrata)
library(tidyverse)

df <- read.socrata(
  "https://data.cityofnewyork.us/resource/9s4h-37hy.json?$select=addr_pct_cd",
  app_token = "77lEiK4wck7zpW1TqNwKxVMHr",
  email     = "rjl2168@columbia.edu",
  password  = "Stat5702"
)

#Convert charcter variable to numeric
df$pd_cd <- as.numeric(df$addr_pd_cd)
#Filter out missing data
df_clean <- df %>% filter((!is.na(addr_pd_cd)))
ggplot(df_clean, aes(addr_pd_cd)) + geom_histogram(binwidth = 1)