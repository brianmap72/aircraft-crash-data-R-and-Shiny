rm(list = ls())

### Import Libraries
##===================
setwd("/Users/brianho/Desktop/crash/")

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
if (!require(randomForest)) {install.packages("randomForest"); require(randomForest)}
if (!require(caret)) {install.packages("caret"); require(caret)}
if (!require(e1071)) {install.packages("e1071"); require(e1071)}
if (!require(ranger)) {install.packages("ranger"); require(ranger)}

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
                                                               (hour>=0 & hour<6) ~"12am-6am"))
crashdata.2$Time <- NULL
crashdata.2$Registration <- NULL
crashdata.2$Location <- NULL
crashdata.2$hour <- NULL
crashdata.2$Ground <- NULL

cdf <- crashdata.2

Fatalities <- cdf %>% group_by(year) %>% 
  summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))

incomedata <- read.csv(file="countryincome.csv", header=TRUE, sep=",")
passenger <- read.csv(file="countrypassenger.csv", header=TRUE, sep=",")
aviationtech <- read.csv(file="AviationTechEvents.csv", header=TRUE, sep=",")
worldevent <- read.csv(file="WorldEvent.csv", header=TRUE, sep=",")

Fatalities.1 <- merge(x = Fatalities, y = aviationtech, by.x = "year", by.y = c("Year"), all.x = TRUE)
Fatalities.2 <- merge(x = Fatalities.1, y = worldevent, by.x = "year", by.y = c("Year"), all.x = TRUE)


library('stringdist')
library('dplyr')
crashdataCityCountry <-  unique(cdf$country_city)
incomedatacCityCountry <- unique(incomedata$TableName)
passengerCityCountry <- unique(passenger$Country.Name)

kpm <- stringdistmatrix(as.character(crashdataCityCountry),as.character(incomedatacCityCountry),method="lv")
kpm <- data.frame(as.matrix(kpm))

incomematchcitycountry <- {}
for (i in 1: length(crashdataCityCountry)){
  minindex <- which.min(kpm[i,])
  incomematchcitycountry <- c(incomematchcitycountry, as.character(incomedatacCityCountry[minindex]))}
df <- as.data.frame(incomematchcitycountry)
colnames(df)[which(names(df) == "incomematchcitycountry")] <- "TableName"
df$country_city <- crashdataCityCountry

cdf.1 <- merge(x = cdf, y = df, by = "country_city", all.x = TRUE)
cdf.2 <- merge(x = cdf.1, y = incomedata[c(3,4)], by = "TableName", all.x = TRUE)

kpm <- stringdistmatrix(as.character(crashdataCityCountry),as.character(passengerCityCountry),method="lv")
kpm <- data.frame(as.matrix(kpm))

passengermatchcitycountry <- {}
for (i in 1: length(crashdataCityCountry)){
  minindex <- which.min(kpm[i,])
  passengermatchcitycountry <- c(passengermatchcitycountry, as.character(passengerCityCountry[minindex]))}
df <- as.data.frame(passengermatchcitycountry)
colnames(df)[which(names(df) == "passengermatchcitycountry")] <- "Country.Name"
df$country_city <- crashdataCityCountry

cdf.3 <- merge(x = cdf, y = df, by = "country_city", all.x = TRUE)
cdf.4 <- merge(x = cdf.3, y = passenger, by = "Country.Name", all.x = TRUE)
cdf.5 <- cbind(cdf.2,cdf.4[c(18:57)])


  library(randomForest)
  library(caret)
  library(e1071)
library(ranger)       # a faster implementation of randomForest

  # Train the model
  trainingdata <- cdf.5
  trainingdata$month <- as.factor(trainingdata$month)
  trainingdata$year <- as.factor(trainingdata$year)
  trainingdata$dayofweek <- as.factor(trainingdata$dayofweek)
  trainingdata$hoursegment <- as.factor(trainingdata$hoursegment)
  
  trainingdata$country_city <- as.character(trainingdata$country_city) #######
  abc <-sort(table(trainingdata$country_city),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$country_city %in% names(abc)))
  trainingdata$country_city[def] <- "None"
  trainingdata$country_city[is.na(trainingdata$country_city)]<- "None"
  trainingdata$country_city <- as.factor(trainingdata$country_city)  
  
  trainingdata$place <- as.character(trainingdata$place) #######
  abc <-sort(table(trainingdata$place),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$place %in% names(abc)))
  trainingdata$place[def] <- "None"
  trainingdata$place[is.na(trainingdata$place)]<- "None"
  trainingdata$place <- as.factor(trainingdata$place) 

  trainingdata$origin <- as.character(trainingdata$origin) #######
  abc <-sort(table(trainingdata$origin),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$origin %in% names(abc)))
  trainingdata$origin[def] <- "None"
  trainingdata$origin[is.na(trainingdata$origin)]<- "None"
  trainingdata$origin <- as.factor(trainingdata$origin) 
  
  trainingdata$destination <- as.character(trainingdata$destination) #######
  abc <-sort(table(trainingdata$destination),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$destination %in% names(abc)))
  trainingdata$destination[def] <- "None"
  trainingdata$destination[is.na(trainingdata$destination)]<- "None"
  trainingdata$destination <- as.factor(trainingdata$destination) 
  
  trainingdata$Operator <- as.character(trainingdata$Operator) #######
  abc <-sort(table(trainingdata$Operator),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$Operator %in% names(abc)))
  trainingdata$Operator[def] <- "None"
  trainingdata$Operator[is.na(trainingdata$Operator)]<- "None"
  trainingdata$Operator <- as.factor(trainingdata$Operator) 
  
  trainingdata$Type <- as.character(trainingdata$Type) #######
  abc <-sort(table(trainingdata$Type),decreasing = TRUE)[1:50]
  def<- which(!(trainingdata$Type %in% names(abc)))
  trainingdata$Type[def] <- "None"
  trainingdata$Type[is.na(trainingdata$Type)]<- "None"
  trainingdata$Type <- as.factor(trainingdata$Type) 
  
  trainingdata$hoursegment <- as.character(trainingdata$hoursegment)
  trainingdata$hoursegment[is.na(trainingdata$hoursegment)]<- "None"
  trainingdata$hoursegment <- as.factor(trainingdata$hoursegment)
  
  trainingdata$IncomeGroup <- as.character(trainingdata$IncomeGroup)
  trainingdata$IncomeGroup[is.na(trainingdata$IncomeGroup)]<- "None"
  trainingdata$IncomeGroup <- as.factor(trainingdata$IncomeGroup)
  
  trainingdata$dayofweek <- as.character(trainingdata$dayofweek)
  trainingdata$dayofweek[is.na(trainingdata$dayofweek)]<- "None"
  trainingdata$dayofweek <- as.factor(trainingdata$dayofweek)
  
  for (i in 17:57){
    trainingdata[[i]] <- sapply(trainingdata[[i]],function(x) replace(x,is.na(x),0))
  }

  
  set.seed(51)
  trainingdata.1 <- trainingdata[c(2,3,6,8,10,12:57)]
  
  model_rf_reg <- randomForest(Fatalities~.,
                                       data = trainingdata.1,
                                       ntree = 200,
                                       mtry = 14,
                                       nodesize =9,
                                       sampsize = round(0.55*nrow(trainingdata.1)),
                                       do.trace	= TRUE)
  
  myVar = varImp(model_rf_reg, scale=FALSE)

  p <- plot_ly(crashdata.2, x = ~month, y = ~hoursegment, z = ~dayofweek, color = ~Fatalities, size = ~Fatalities, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Month'),
                      yaxis = list(title = 'Hour'),
                      zaxis = list(title = 'Day of Week')))
  
  
  OOB_RMSE <- vector(mode = "numeric", length = 100)
  
  for(i in seq_along(OOB_RMSE)) {
    
    print(i)
    
    optimal_ranger <- ranger(
      formula         = Fatalities ~ ., 
      data            = trainingdata.1, 
      num.trees       = 200,
      mtry            = 24,
      min.node.size   = 5,
      sample.fraction = .8,
      importance      = 'impurity'
    )
    
    OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
  }
  
  optimal_ranger$variable.importance %>% 
    dplyr::arrange(desc(x)) %>%
    dplyr::top_n(25) %>%
    ggplot(aes(reorder(names, x), x)) +
    geom_col() +
    coord_flip() +
    ggtitle("Top 25 important variables")
  
  hist(OOB_RMSE, breaks = 20)


  hyper_grid <- expand.grid(
    mtry       = seq(10, 30, by = 2),
    node_size  = seq(3, 9, by = 2),
    sampe_size = c(.55, .632, .70, .80),
    OOB_RMSE   = 0
  )
  for(i in 1:nrow(hyper_grid)) {
    print(i)
    # train model
    model <- ranger(
      formula         = Fatalities ~ ., 
      data            = trainingdata.1, 
      num.trees       = 200,
      mtry            = hyper_grid$mtry[i],
      min.node.size   = hyper_grid$node_size[i],
      sample.fraction = hyper_grid$sampe_size[i],
      seed            = 123
    )
    
    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
  }
  36
  hyper_grid[36,]
  mtry node_size sampe_size OOB_RMSE
  14         9       0.55 30.70517
  
  # total number of combinations
  nrow(hyper_grid)
# #Clean the summary field
# 
# #Trend
# getTrend(crashdata)
# getOperator(crashdata)
# getLocationCrash(crashdata)
# getFatalities(crashdata)
# getCrashtype(crashdata)
# getCrashreason(crashdata)
# 
# rsconnect::setAccountInfo(name='brianho',
#                           token='C206C0D5B084B661266C854CCD9FF406',
#                           secret='bLNg13jnEe6FaQNml6Jwr1vv3D9M2EsNzuQLcJqj')



  getTree(model_rf_reg, 
          k = 185, 
          labelVar = TRUE)

