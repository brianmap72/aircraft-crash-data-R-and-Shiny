if (!require(shinydashboard)) {install.packages("shinydashboard"); require(shinydashboard)}
if (!require(shiny)) {install.packages("shiny"); require(shiny)}
if (!require(leaflet)) {install.packages("leaflet"); require(leaflet)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(googleway)) {install.packages("googleway"); require(googleway)}
if (!require(rgeos)) {install.packages("rgeos"); require(rgeos)}
if (!require(ggmap)) {install.packages("ggmap"); require(ggmap)}
if (!require(crayon)) {install.packages("crayon"); require(crayon)}
if (!require(randomForest)) {install.packages("randomForest"); require(randomForest)}
if (!require(rhandsontable)) {install.packages("rhandsontable"); require(rhandsontable)}
if (!require(ROCR)) {install.packages("ROCR"); require(ROCR)}
if (!require(DT)) {install.packages("DT"); require(DT)}
if (!require(feather)) {install.packages("feather"); require(feather)}
if (!require(caret)) {install.packages("caret"); require(caret)}
if (!require(rpart)) {install.packages("rpart"); require(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); require(rpart.plot)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(treemapify)) {install.packages("treemapify"); require(treemapify)}
if (!require(feather)) {install.packages("feather"); require(feather)}
if (!require(colorspace)) {install.packages("colorspace"); require(colorspace)}
if (!require(plotly)) {install.packages("plotly"); require(plotly)}
if (!require(shinyalert)) {install.packages("shinyalert"); require(shinyalert)}
if (!require(lubridate)) {install.packages("lubridate"); require(lubridate)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(tm)) {install.packages("tm"); require(tm)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}
if (!require(SnowballC)) {install.packages("SnowballC"); require(SnowballC)}
if (!require(plotly)) {install.packages("plotly"); require(plotly)}
if (!require(plotly)) {install.packages("plotly"); require(plotly)}
if (!require(wordcloud)) {install.packages("wordcloud"); require(wordcloud)}
if (!require(wordcloud2)) {install.packages("wordcloud2"); require(wordcloud2)}
if (!require(e1071)) {install.packages("e1071"); require(e1071)}
if (!require(ranger)) {install.packages("ranger"); require(ranger)}
if (!require(stringdist)) {install.packages("stringdist"); require(stringdist)}
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(topicmodels)) {install.packages("topicmodels"); require(topicmodels)}

ui <- dashboardPage(
  dashboardHeader(
    tags$li(a(href = 'https://www.axiata.com',
              img(src = 'axiata.png',
                  title = "Airplane Crash Analytics - 1908", height = 30, height = 60),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown"),
    title = "Airplane Crash Analytics - 1908",
    titleWidth = 620
  ),
  skin = "black",
  dashboardSidebar(
    width = 620,
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("home", lib = "font-awesome")),
      menuItem("Descriptive Analytics", tabName = "AA", icon = icon("check-circle", lib = "font-awesome"),
               menuSubItem("Various Historical Data Visualizations", tabName = "descp", icon = icon("bell"))),
      menuItem("Predictive Analytics", tabName = "AA", icon = icon("check-circle", lib = "font-awesome"),
               menuSubItem("External Events Analytics", tabName = "ext", icon = icon("bell")),
               menuSubItem("Random Forest Regressor", tabName = "pred", icon = icon("bell"))
               )
    )),
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }'))),
    
    #TAB MENU ITEMS   
    #=========
    tabItems(
      # TAB 1: Introduction
      # ==================================
      tabItem(tabName = "Introduction",
                br(),
                br(),
                br(),
                fluidRow(
                box(
                  title = tags$p("Descriptive Analytics:", style = "font-size: 100%;"), width = 6, height = 250, solidHeader = TRUE, status = "primary",
                  collapsible = TRUE,
                  tags$p("Descriptive analytics is a preliminary stage of data processing that creates a summary of historical data to yield useful
                  information and possibly prepare the data for further analysis.", style = "font-size: 160%;")
                ),
                box(
                  title = tags$p("Predictive Analytics:", style = "font-size: 100%;"), width = 6, height = 250, solidHeader = TRUE,status = "danger",
                  collapsible = TRUE,
                  tags$p("Predictive analytics is a form of advanced analytics that uses both new and historical data to forecast activity, behavior and trends. 
                         It involves applying statistical analysis techniques, analytical queries and automated machine learning algorithms 
                         to data sets to create predictive models", style = "font-size: 160%;")
                )
                #HTML('<center><img src="axiata.png" width="500"></center>')
                )
      ),
      
      # TAB 1: Descriptive analytics
      # ==================================
      tabItem(tabName = "descp",
              tags$head(tags$style(HTML('.main-header .logo {
                                        font-family: "Georgia", Times, "Times New Roman", serif;
                                        font-weight: bold;
                                        font-size: 24px;
                                        }
                                        '))),
              titlePanel("Descriptive Analytics: "),
              
              fluidRow(
                column(width = 4,
                       wellPanel(
                         actionButton("getkeydriver", "Explore Descriptive Analytics",width = 350)
                       ))
              ),
              
              mainPanel(
                useShinyalert(),  # Set up shinyalert
                width = 1500,
                height = 3200,
                tabsetPanel(
                  tabPanel("Overall Interactive Data Table",DT::dataTableOutput("crashdatatable")),
                  tabPanel("Trend Analysis",plotOutput("trend1",height = "550px"),plotOutput("trend2",height = "550px"),plotOutput("trend3",height = "550px")),
                  #tabPanel("Fatality Analysis",plotOutput("fatality",height = "1150px")),
                  tabPanel("Crash Site Analysis",plotOutput("crashsite",height = "1150px")),
                  tabPanel("Crash Reason Analysis",
                           fluidRow(
                             column(width = 6,
                                    wellPanel(
                                    tags$p("Wordcloud of Key Crash Reasons", style = "font-size: 120%;"),
                                    wordcloud2Output("crashreason", width="100%",height="600px"))  
                             ),
                             column(width = 6,
                                    wellPanel(
                                      selectInput("wordassoc", "Select Top Key Crash Reason for Words Association:",
                                                  choices=list("","pilot","approach","engine","runway","failure","crew"),selected = FALSE, multiple = FALSE),
                                      h5("\n"),
                                      plotOutput("crashreason1",height = "650px"))
                             )
                           )),
                  tabPanel("Crash Reason Topic Modeling",dataTableOutput("topic")),
                  tabPanel("Aircarft Operator Analysis",plotOutput("operator",height = "1150px")),
                  tabPanel("Origi and Destination Fatality Heatmap",plotlyOutput("oridest_heatmap",height = "1150px")),
                  tabPanel("3D Crash Date and Time Analysis",plotlyOutput("ThreeDtime",height = "850px"))
                ))),
      # TAB 2: Predictive analytics
      # ==================================
      tabItem(tabName = "ext",
              tags$head(tags$style(HTML('.main-header .logo {
                                        font-family: "Georgia", Times, "Times New Roman", serif;
                                        font-weight: bold;
                                        font-size: 24px;
                                        }
                                        '))),
              titlePanel("Correlation with External Data or Events: "),
              
              fluidRow(
                useShinyalert(),  # Set up shinyalert
                column(width = 3,
                       wellPanel(
                         actionButton("getkeydriver1", "Explore External Data",width = 250)
                       ))
              ),

              mainPanel(
                useShinyalert(),  # Set up shinyalert
                width = 1500,
                height = 3200,
                tabsetPanel(
                  tabPanel("Fatality Count vs Hist Aviation Tech Events",plotlyOutput("aviationtechevent1",height = "750px")),
                  tabPanel("Fatality Rate vs Hist Aviation Tech Events",plotlyOutput("aviationtechevent2",height = "750px")),
                  tabPanel("Number of Crash Case vs Hist Aviation Tech Events",plotlyOutput("aviationtechevent3",height = "750px")),
                  tabPanel("Fatality Count vs Hist World Events",plotlyOutput("worldevent1",height = "750px")),
                  tabPanel("Fatality Rate vs Hist World Events",dataTableOutput("worldevent2",height = "750px")),
                  tabPanel("Number of Crash Case vs Hist World Events",plotlyOutput("worldevent3",height = "750px"))
                ))),
      # TAB 3: Predictive analytics
      # ==================================
      tabItem(tabName = "pred",
              tags$head(tags$style(HTML('.main-header .logo {
                                        font-family: "Georgia", Times, "Times New Roman", serif;
                                        font-weight: bold;
                                        font-size: 24px;
                                        }
                                        '))),
              titlePanel("Predictive Analytics: "),
              
              fluidRow(
                useShinyalert(),  # Set up shinyalert
                column(width = 3,
                       wellPanel(
                         actionButton("getkeydriver2", "Explore Predictive Analytics",width = 250)
                       ))
              ),
              
              mainPanel(
                useShinyalert(),  # Set up shinyalert
                width = 1500,
                height = 3200,
                tabsetPanel(
                  tabPanel("Mean Squared Error Plot vs Number of Tree",plotOutput("mseplot",height = "750px")),
                  tabPanel("Tree which Gives the Minimum Mean Squared Error",dataTableOutput("mseplot1",height = "750px")),
                  tabPanel("Variable Importance",plotOutput("mseplot2",height = "750px"))
                )))
    )))

# ==================================
# ================================== 


#server   
#=========
server <- function(input, output, session) {

  crashdatafile <- eventReactive(input$getkeydriver,{
    
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

    return(crashdata.2)
  })
  
  fatalityatafile <- eventReactive(input$getkeydriver1,{
    
    crashdata <- read.csv(file="Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",")
    aviationtech <- read.csv(file="AviationTechEvents.csv", header=TRUE, sep=",")
    worldevent <- read.csv(file="WorldEvent.csv", header=TRUE, sep=",")

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
    
    Fatalities <- crashdata.2 %>% group_by(year) %>% 
      summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))
    
    Fatalities.1 <- merge(x = Fatalities, y = aviationtech, by.x = "year", by.y = c("Year"), all.x = TRUE)
    Fatalities.2 <- merge(x = Fatalities.1, y = worldevent, by.x = "year", by.y = c("Year"), all.x = TRUE)
    
    return(Fatalities.2)
  })
  
  crashcasedatafile <- eventReactive(input$getkeydriver1,{
    
    crashdata <- read.csv(file="Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",")
    aviationtech <- read.csv(file="AviationTechEvents.csv", header=TRUE, sep=",")
    worldevent <- read.csv(file="WorldEvent.csv", header=TRUE, sep=",")
    
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
    
    crashcase <- crashdata.2 %>% group_by(year) %>% 
      summarise(total_crashcase = n())
    
    crashcase.1 <- merge(x = crashcase, y = aviationtech, by.x = "year", by.y = c("Year"), all.x = TRUE)
    crashcase.2 <- merge(x = crashcase.1, y = worldevent, by.x = "year", by.y = c("Year"), all.x = TRUE)
    
    return(crashcase.2)
  })
  
  modelingdatafile <- eventReactive(input$getkeydriver2,{
    
    crashdata <- read.csv(file="Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",")
    incomedata <- read.csv(file="countryincome.csv", header=TRUE, sep=",")
    passenger <- read.csv(file="countrypassenger.csv", header=TRUE, sep=",")
    aviationtech <- read.csv(file="AviationTechEvents.csv", header=TRUE, sep=",")
    worldevent <- read.csv(file="WorldEvent.csv", header=TRUE, sep=",")
    
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
    
    return(trainingdata)
  })
  
  
  output$crashdatatable <- DT::renderDataTable({
    withProgress(message = 'Building data table', {
      Sys.sleep(0.25)
      cdf <- crashdatafile()            
      
      Sys.sleep(0.75)
    })
    DT::datatable(cdf)
  })
  
  output$trend1 <- renderPlot({
    withProgress(message = 'Computing for trend and plotting charts', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      Crashcase <- cdf %>% group_by(year) %>% 
        summarise(Freq=n())
      
      p <- ggplot(Crashcase, aes(y = Freq, x = year, group = 1))  + 
        geom_line(size = 1, linetype = 1, color = "Red") + 
        geom_point(size = 3, shape = 20)+ 
        geom_smooth() +
        xlab("Years") + ylab("Number of Crashes") + 
        scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 2)) + 
        ggtitle("Total Number of Crashes per Year") +
        theme(axis.text.x.bottom= element_text(angle = 45, hjust = 0))
      
      Sys.sleep(0.75)
    })
    print(p)
  })
  output$trend2 <- renderPlot({
    withProgress(message = 'Computing for trend and plotting charts', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            

      Fatalities <- cdf %>% group_by(year) %>% 
        summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))
      Fatalities <- as.data.frame(Fatalities)
      p <- ggplot(Fatalities, aes(y = total_fatalities, x = year, group = 1))  + 
        geom_line(size = 1, linetype = 1, color = "Navy") + 
        geom_point(size = 3, shape = 20)+ 
        geom_smooth() +
        xlab("Years") + ylab("Fatality Count") + 
        scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 2)) + 
        ggtitle("Fatality Count per Year") +
        theme(axis.text.x.bottom= element_text(angle = 45, hjust = 0))
      
      Sys.sleep(0.75)
    })
    print(p)
  })
  output$trend3 <- renderPlot({
    withProgress(message = 'Building Model and derive key drivers', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      Fatalities <- cdf %>% group_by(year) %>% 
        summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))
      Fatalities <- as.data.frame(Fatalities)
      p <- ggplot(Fatalities, aes(y = round((total_fatalities*100/total_passengers),2), x = year, group = 1))  + 
        geom_line(size = 1, linetype = 1, color = "Green") + 
        geom_point(size = 3, shape = 20)+ 
        geom_smooth() +
        xlab("Years") + ylab("Fatality Rate") + 
        scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 2)) + 
        ggtitle("Fatality Rate per Year") +
        theme(axis.text.x.bottom= element_text(angle = 45, hjust = 0))
      
      Sys.sleep(0.75)
    })
    print(p)
  })
  
  output$fatality <- renderPlot({
    withProgress(message = 'Building Model and derive key drivers', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      Fatalities <- cdf %>% group_by(year) %>% 
        summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))
      
      p <- ggplot(Fatalities, aes(y = (total_fatalities/total_passengers)*100, x = year, group = 10))  + 
        geom_line(size = 1, linetype = 1, color = "Red") + 
        geom_point(size = 3, shape = 20) + 
        geom_smooth() +
        xlab("Years") + ylab("Fatality Rate") + 
        scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 10)) +
        ggtitle("Fatality Rate per Year")
      
      Sys.sleep(0.75)
    })
    
    print(p)
    
  })
  
  output$crashsite <- renderPlot({
    withProgress(message = 'Computing for crashsite stats and plotting chart', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      Location_Crash <-   cdf %>% group_by(place) %>% 
        summarise(total_fatalities = sum(Fatalities)) %>% arrange(desc(total_fatalities))
      
      Location_Crashdf <- as.data.frame(Location_Crash)
      Location_Crashdf.1 <- Location_Crashdf[1:20,]
      Location_Crashdf.1$place <- as.factor(Location_Crashdf.1$place)
      
      p <- ggplot(Location_Crash[1:20,], aes(x = reorder(place, -total_fatalities), y = total_fatalities, alpha = total_fatalities)) +
        geom_bar(stat = "identity", fill = "red", width = 0.5) +
        xlab("Countries") + ylab("Number of fatalities") +
        ggtitle("Top 20 Countries with highest Fatalities")+
        theme(axis.text.x.bottom= element_text(angle = 45, hjust = 0))

      Sys.sleep(0.75)
    })
    
    print(p)
    
  })
  
  output$operator <- renderPlot({
    withProgress(message = 'Deriving the Top 20 operator causing aircrash', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      crash_operator <-   cdf %>% group_by(Operator) %>% 
        summarise(Freq = n()) %>% arrange(desc(Freq))

      crash_operator <- as.data.frame(crash_operator)
      crash_operator.1 <- crash_operator[1:20,]
      
      p <- ggplot(crash_operator.1, aes(x = reorder(factor(Operator), Freq), y = Freq, alpha = Freq)) + 
        geom_bar(stat = "identity", fill = "red", width = 0.5) + geom_point(stat = "identity") + 
        xlab("Aircraft Operators") + ylab("Crashes") + 
        ggtitle("Top 20 Aircraft Operator causing Aircrash") +
        theme(axis.text.x.bottom= element_text(angle = 90, hjust = 0))
      
      Sys.sleep(0.75)
    })
    
    print(p)
    
  })
  
  output$crashreason <- renderWordcloud2({
    withProgress(message = 'Deriving the crash reasons, this will take a couple of minutes....', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      docs <- VCorpus(VectorSource(cdf$Summary))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      # Remove /
      docs <- tm_map(docs, toSpace, "/")
      # Remove @
      docs <- tm_map(docs, toSpace, "@")
      # Remove |
      docs <- tm_map(docs, toSpace, "\\|")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      #head(d, 10)
      
      d <- d[-c(1:4),]
      p <- wordcloud2(d)
      
      Sys.sleep(0.75)
    })
    
    print(p)
    
  })
  
  selectedwordassoc <- reactive({
    was <- input$wordassoc
  })
  output$crashreason1 <- renderPlot({
    withProgress(message = 'Deriving the crash reasons, this will take a couple of minutes....', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      docs <- VCorpus(VectorSource(cdf$Summary))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      # Remove /
      docs <- tm_map(docs, toSpace, "/")
      # Remove @
      docs <- tm_map(docs, toSpace, "@")
      # Remove |
      docs <- tm_map(docs, toSpace, "\\|")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)

      d <- d[-c(1:4),]
      
      print(selectedwordassoc())
      
      adf <- findAssocs(dtm, terms = as.character(selectedwordassoc()), corlimit = 0.15)
      assocdf <- as.data.frame(adf)
      colnames(assocdf) <- "var"
      assocdf$words <- rownames(assocdf)
      
      p <- ggplot(assocdf, aes(reorder(words,var), var, fill = var)) + 
        geom_bar(position="dodge", stat="identity") + coord_flip() + 
        theme(axis.text.y=element_text(angle=0, hjust=1,size = 15)) + labs(x="associated words", y="correlation coefficient") +
        theme(axis.title=element_text(size=18,face="bold")) + 
        theme(legend.text=element_text(size=14))
      
      Sys.sleep(0.75)
    })
    
    print(p)
    
  })
  
  output$oridest_heatmap <- renderPlotly({
    withProgress(message = 'Plotting the 2D heatmap, this will take a couple of minutes....', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()            
      
      cdf$origin <- as.character(cdf$origin)
      cdf$origin[cdf$origin==""] <- "None"
      truncateindex_origin <-sort(table(cdf$origin),decreasing = TRUE)[1:100]
      notincindex_origin<- which(!(cdf$origin %in% names(truncateindex_origin)))
      cdf$origin[notincindex_origin] <- "None"
      cdf$origin <- factor(cdf$origin)

      cdf$destination <- as.character(cdf$destination)
      cdf$destination[which(cdf$destination=="")] <- "None"
      truncateindex_destination <-sort(table(cdf$destination),decreasing = TRUE)[1:100]
      notincindex_destination<- which(!(cdf$destination %in% names(truncateindex_destination)))
      cdf$destination[notincindex_destination] <- "None"
      cdf$destination <- factor(cdf$destination)
      
      Var1 <-{};Var2 <-{};value <-{}
      nx <- length(levels(cdf$origin))
      vecx <- levels(cdf$origin)
      ny <- length(levels(cdf$destination))
      vecy <- levels(cdf$destination)

      vecx_ind <- grep("None",vecx)
      vecy_ind <- grep("None",vecy)
      
      df <- data.frame(matrix(ncol = 0, nrow = nx * ny))
      for (i in 1:nx){
        for (j in 1:ny){
          ind_subset <- subset(cdf, (cdf$origin==as.character(vecx[i]) & cdf$destination==as.character(vecy[j])))

          Fatalities_subset <- ind_subset$Fatalities
          aa <- vecx[i]
          bb <- vecy[j]
          if(is.factor(aa)) aa <-as.character(aa) 
          if(is.factor(bb)) aa <-as.character(bb) 
          Var1 <- c(Var1, aa)
          Var2 <- c(Var2, bb)

          if(i == vecx_ind | j == vecy_ind) {
            value <-c(value,0)}
          else{     value <-c(value,sum(Fatalities_subset))}
          
        }}
      df$Var1 <-Var1
      df$Var2 <-Var2
      df$value <-value

      p <-  plot_ly(x = df$Var1, y = df$Var2, z = df$value, 
                     key = df$value, type = "heatmap", source = "heatplot") %>%
        layout(xaxis = list(title = "Origin"), 
               yaxis = list(title = "Destination")) 
 
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$aviationtechevent2 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatality Rate vs Historical Aviation Tech Events', {
      Sys.sleep(0.25)
      
      cdf <- fatalityatafile()            
      cdf.1 <- subset(cdf, cdf$AviationTechEvents!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~round((total_fatalities*100/total_passengers),2) ,type = 'scatter', mode = 'lines')%>%
        layout(title = "Fatality Rate vs Year with Historical Aviation Tech Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Fatality Rate"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = round((cdf.1$total_fatalities*100/cdf.1$total_passengers),2),
          text = cdf.1$AviationTechEvents,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$worldevent2 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatality Rate vs Historical World Events', {
      Sys.sleep(0.25)
      
      cdf <- fatalityatafile()  
      cdf.1 <- subset(cdf, cdf$WorldEvent!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~round((total_fatalities*100/total_passengers),2),type = 'scatter', mode = 'lines')%>%
        layout(title = "Fatality Rate vs Year with Historical World Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Fatality Rate"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = round((cdf.1$total_fatalities*100/cdf.1$total_passengers),2),
          text = cdf.1$WorldEvent,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
    
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$aviationtechevent1 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical Aviation Tech Events', {
      Sys.sleep(0.25)
      
      cdf <- fatalityatafile()            
      cdf.1 <- subset(cdf, cdf$AviationTechEvents!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~total_fatalities,type = 'scatter', mode = 'lines')%>%
        layout(title = "Fatalities vs Year with Historical Aviation Tech Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Fatality Count"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = cdf.1$total_fatalities,
          text = cdf.1$AviationTechEvents,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$worldevent1 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical World Events', {
      Sys.sleep(0.25)
      
      cdf <- fatalityatafile()  
      cdf.1 <- subset(cdf, cdf$WorldEvent!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~total_fatalities,type = 'scatter', mode = 'lines')%>%
        layout(title = "Fatalities vs Year with Historical World Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Fatality Count"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = cdf.1$total_fatalities,
          text = cdf.1$WorldEvent,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$worldevent2 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical World Events', {
      Sys.sleep(0.25)
      
      cdf <- fatalityatafile()  
      cdf.1 <- subset(cdf, cdf$WorldEvent!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~total_fatalities,type = 'scatter', mode = 'lines')%>%
        layout(title = "Fatalities vs Year with Historical World Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Fatality Count"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = cdf.1$total_fatalities,
          text = cdf.1$WorldEvent,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  output$aviationtechevent3 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical Aviation Tech Events', {
      Sys.sleep(0.25)
      
      cdf <- crashcasedatafile()            
      cdf.1 <- subset(cdf, cdf$AviationTechEvents!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~total_crashcase,type = 'scatter', mode = 'lines')%>%
        layout(title = "Number of Crash Case vs Year with Historical Aviation Tech Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Crash Case"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = cdf.1$total_crashcase,
          text = cdf.1$AviationTechEvents,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$worldevent3 <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical World Events', {
      Sys.sleep(0.25)
      
      cdf <- crashcasedatafile()  
      cdf.1 <- subset(cdf, cdf$WorldEvent!="")
      
      p <- plot_ly(cdf, x = ~year, y = ~total_crashcase,type = 'scatter', mode = 'lines')%>%
        layout(title = "Number of Crash Case vs Year with Historical World Events",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Crash Case"), barmode = 'stack') %>%
        add_annotations(
          x = cdf.1$year,
          y = cdf.1$total_crashcase,
          text = cdf.1$WorldEvent,
          showarrow = TRUE,
          color = 'rgb(255,69,0)',
          yanchor = 'left',
          xanchor = 'left',
          font=list(size=10,face="bold")
        )
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$mseplot <- renderPlot({
    withProgress(message = 'Building a random forest regressor model, this process may takes up to 3-4 minutes', {
      Sys.sleep(0.25)
      
      trainingdata <- modelingdatafile()  
 
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
      p <- plot(model_rf_reg$mse)
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$mseplot1 <- renderDataTable({
    withProgress(message = 'Building a random forest regressor model, this process may takes up to 3-4 minutes', {
      Sys.sleep(0.25)
      
      trainingdata <- modelingdatafile()  
      
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
      besttreeind <- which.min(model_rf_reg$mse)
      besttree <- getTree(model_rf_reg, 
              k = besttreeind, 
              labelVar = TRUE)  
      
      Sys.sleep(0.75)
    })
    DT::datatable(besttree)
  })

  output$mseplot2 <- renderPlot({
    withProgress(message = 'Building a random forest regressor model, this process may takes up to 3-4 minutes', {
      Sys.sleep(0.25)
      
      trainingdata <- modelingdatafile()  
      
      for (i in 17:57){
        trainingdata[[i]] <- sapply(trainingdata[[i]],function(x) replace(x,is.na(x),0))
      }
      
      set.seed(52)
      trainingdata.1 <- trainingdata[c(2,3,6,8,10,12:57)]
      
      OOB_RMSE <- vector(mode = "numeric", length = 50)
      
      for(i in seq_along(OOB_RMSE)) {
        
        print(i)
        
        optimal_ranger <- ranger(
          formula         = Fatalities ~ ., 
          data            = trainingdata.1, 
          num.trees       = 100,
          mtry            = 14,
          min.node.size   = 9,
          sample.fraction = .55,
          importance      = 'impurity'
        )
        
        OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
      }
      
      df <-as.data.frame(optimal_ranger$variable.importance)
      colnames(df) <- "Importance"
      df$Driver <- rownames(df)
      
      p <- ggplot(df, aes(reorder(Driver,Importance), Importance, fill = Importance)) + 
        geom_bar(position="dodge", stat="identity") + coord_flip() + 
        theme(axis.text.y=element_text(angle=0, hjust=1,size = 15)) + labs(x="Predictor", y="Relative Importance") +
        theme(axis.title=element_text(size=18,face="bold")) + 
        theme(legend.text=element_text(size=14))
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
  
  output$topic <- DT::renderDataTable({
    withProgress(message = 'Building data table.. This will take a couple of minutes', {
      Sys.sleep(0.25)
      cdf <- crashdatafile()            
      
      summdoc <- iconv(cdf$Summary, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
      summdoc <- tolower(summdoc)  # Make everything consistently lower case
      summdoc <- gsub("rt", " ", summdoc)  # Remove the "RT" (retweet) so duplicates are duplicates
      summdoc <- gsub("@\\w+", " ", summdoc)  # Remove user names (all proper names if you're wise!)
      summdoc <- gsub("http.+ |http.+$", " ", summdoc)  # Remove links
      summdoc <- gsub("[[:punct:]]", " ", summdoc)  # Remove punctuation
      summdoc <- gsub("[ |\t]{2,}", " ", summdoc)  # Remove tabs
      summdoc <- gsub("amp", " ", summdoc)  # "&" is "&amp" in HTML, so after punctuation removed ...
      summdoc <- gsub("^ ", "", summdoc)  # Leading blanks
      summdoc <- gsub(" $", "", summdoc)  # Lagging blanks
      summdoc <- gsub(" +", " ", summdoc) # General spaces (should just do all whitespaces no?)
      summdoc <- unique(summdoc)  # Now get rid of duplicates!
      
      corpus <- VCorpus(VectorSource(summdoc))  # Create corpus object
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      corpus <- tm_map(corpus, toSpace, "/")
      corpus <- tm_map(corpus, toSpace, "@")
      corpus <- tm_map(corpus, toSpace, "\\|")
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      
      doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
      dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
    
      SEED = 12345  
      k = 4  
      
      model <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,thin = 100,    iter = 1000))
      tpm <- terms(model, 10)
      
      Sys.sleep(0.75)
    })
    DT::datatable(as.data.frame(tpm))
  })
  
  output$ThreeDtime <- renderPlotly({
    withProgress(message = 'Deriving Yearly Fatalities vs Historical World Events', {
      Sys.sleep(0.25)
      
      cdf <- crashdatafile()  
      
      p <- plot_ly(cdf, x = ~month, y = ~hoursegment, z = ~dayofweek, color = ~Fatalities, size = ~Fatalities, marker = list(symbol = 'circle', sizemode = 'diameter'),sizes = c(1, 30), colors = c('#BF382A', '#0C4B8E')) %>%
        layout(scene = list(xaxis = list(title = 'Month'),
                            yaxis = list(title = 'Hour'),
                            zaxis = list(title = 'Day of Week')))
      
      Sys.sleep(0.75)
    })
    print(p) 
  })
}

shinyApp(ui, server)