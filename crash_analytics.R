rm(list = ls())

### Import Libraries
##===================
setwd("/Users/brianho/Desktop/Axiata/")

if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(lubridate)) {install.packages("lubridate"); require(lubridate)}
if (!require(tm)) {install.packages("tm"); require(tm)}
if (!require(caret)) {install.packages("caret"); require(caret)}
if (!require(odbc)) {install.packages("odbc"); require(odbc)}
if (!require(wordcloud)) {install.packages("wordcloud"); require(wordcloud)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(DT)) {install.packages("DT"); require(DT)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(SnowballC)) {install.packages("SnowballC"); require(SnowballC)}


source("getTrend.R"); source("getOperator.R");source("getLocationCrash.R"); source("getFatalities.R")
source("getCrashtype.R"); source("getCrashreason.R")

crashdata <- read.csv(file="Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",")

# 1. Date : Date of accident
# 2. Location : City, country
# 3. Operator: Aircraft Operator 
# 4. Type: Aircraft Type
# 5. Aboard: Count of aboard passengers
# 6. Fatalities: Count of deaths occured
# 7. Summary: Reasons for the crash

# Omitting Na values 
crashdata.1 <- crashdata[complete.cases(crashdata),]

# Parse date into year and month and weekend/weekday
crashdata.2 <- crashdata.1 %>% mutate( month = month(as.Date(Date, "%m/%d/%Y")))
crashdata.2 <- crashdata.2 %>% mutate( year = year(as.Date(Date, "%m/%d/%Y")))
crashdata.2 <- crashdata.2 %>% mutate( dayofweek = weekdays(as.Date(Date, "%m/%d/%Y")))

# Parse location into country and city
crashdata.2$Location <- sapply(crashdata.2$Location, as.character)
crashdata.2$place <- (colsplit(string=crashdata.2$Location, pattern=",", names=c("city", "country"))[1])
crashdata.2$country_city <- (colsplit(string=crashdata.2$Location, pattern=",", names=c("city", "country"))[2])
crashdata.2$country_city <- sapply(lapply(crashdata.2$country, trimws)[[1]], as.factor)
crashdata.2$place <- sapply(lapply(crashdata.2$place, trimws)[[1]], as.factor)
crashdata.2$Date <- NULL
crashdata.2$Location <- NULL
crashdata.2$cn.In <- NULL
crashdata.2$cn.In <- NULL

# Parse Route into Origin and Destination
crashdata.2$Location <- sapply(crashdata.2$Route, as.character)
crashdata.2$origin <- (colsplit(string=crashdata.2$Route, pattern=" - ", names=c("origin", "destination"))[1])
crashdata.2$destination <- (colsplit(string=crashdata.2$Location, pattern=" - ", names=c("origin", "destination"))[2])
crashdata.2$destination <- sapply(lapply(crashdata.2$destination, trimws)[[1]], as.factor)
crashdata.2$origin <- sapply(lapply(crashdata.2$origin, trimws)[[1]], as.factor)

# Parse time into hourly segments: 6am-12pm, 12pm-6pm, 6pm-12am, 12am-6am
crashdata.2 <- crashdata.2 %>% mutate( hour = hour(strptime(crashdata.2$Time,"%H:%M")))
crashdata.2 <- crashdata.2 %>% mutate( hoursegment = case_when((hour>=6 & hour<12) ~ "6am-12pm",
                                                               (hour>=12 & hour<18) ~"12pm-6pm",
                                                               (hour>=18 & hour<24) ~"6pm-12am",
                                                               (hour>=0 & hour<6) ~"12am-6am",))
crashdata.2$Time <- NULL
crashdata.2$Registration <- NULL
crashdata.2$Location <- NULL
crashdata.2$hour <- NULL
crashdata.2$Ground <- NULL


cdf <- crashdata.2

#Clean the summary field

#Trend
getTrend(crashdata)
getOperator(crashdata)
getLocationCrash(crashdata)
getFatalities(crashdata)
getCrashtype(crashdata)
getCrashreason(crashdata)

rsconnect::setAccountInfo(name='brianho',
                          token='C206C0D5B084B661266C854CCD9FF406',
                          secret='bLNg13jnEe6FaQNml6Jwr1vv3D9M2EsNzuQLcJqj')
