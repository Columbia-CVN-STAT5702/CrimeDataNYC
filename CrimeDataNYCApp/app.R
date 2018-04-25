#
# This is a web application submitted in partial fulfillment of the requirements for
# the STAT W5701 Final Project

library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(lubridate)
library(ggplot2)
library(leaflet.extras)
library(ggpubr)
library("sp")
library("rgdal")
library("KernSmooth")

library(tm)
library(wordcloud)
library(RWeka)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
#ui <- fluidPage(theme = shinytheme("slate"),
                
   # Application title
   titlePanel("NYC Crime Data Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout( 
      sidebarPanel(
       # HTML('<p><img src="Columbia.png"/></p>'),
        
        
        conditionalPanel(condition="input.tabselected==1", width="400px",
                         #uiOutput("yearRange")
                         #,
                         uiOutput("dateSelect"),
                         uiOutput("boroSelect"),
                         uiOutput("MapTypeSelect"),
                         
                         plotOutput("wrdcld")
                         #div(style="display: inline-block;vertical-align:top; width: 150px; height=20px; font-weight : 10px;",uiOutput("CrimeTypeSelect"))
        ),
        
        
        
        conditionalPanel(condition="input.tabselected==2",
                         uiOutput("dateRangeSelect"),
                         uiOutput("y_var"),
                         uiOutput("color_var"),
                         dataTableOutput("SummaryTable"))
       
        
      ),
     
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Map", value = 1,  leafletOutput("Nycgeomap",height = 625)),
                    tabPanel("Summary", value = 2, plotlyOutput("DataPlot", height=800)),
                    #tabPanel("Precinct Map", value = 3, leafletOutput("NycPrecinctMap")),
                    id = "tabselected")
        
      )
   )
)

#Load the data files for the application
load("NycAppData.RData")
load("Precincts.RData")

#Server Function
server <- function(input, output) {

  #*************************************  
  #Render input UI when on the Map tab
  output$dateSelect <- renderUI({
    dateInput(inputId = "startDate", 
              label = "Date",
              value = "06-01-2012",
              min = "01-01-2006",
              max = "12-31-2016")
  })
  
  
  output$yearRange <- renderUI({
    sliderInput("date_range", 
                "Choose Date Range:", 
                min = as.Date("2006-01-01"), max = as.Date("2016-12-31"), 
                value = c(as.Date("2012-02-25"), Sys.Date())
    )
  })
  
  
  output$boroSelect <- renderUI({
    selectInput(inputId = "boro",
                label = "Borough",
                choices = c("ALL", "MANHATTAN", "BROOKLYN", "QUEENS", "BRONX", "STATEN ISLAND"),
                selected = "ALL")
  })
  
  output$MapTypeSelect <- renderUI({
    selectInput(inputId = "maptype",
                label = "Select Map Type",
                choices = c("Point "="point","Density"="density","Precincts (NYC ALL Boroughs)" ="precincts"),
                selected = "Point")})
  
  
  #*************************************
  
  #*************************************  
  #Render input UI when on the Plot tab
  # output$dateRangeSelect <- renderUI({
  #   dateRangeInput(inputId = "dateRange",
  #                  label = "Select Date Range",
  #                  start = "06-01-2012",
  #                  end = "06-30-2012",
  #                  min = "01-01-2006",
  #                  max = "12-31-2016")
  # })

  output$dateRangeSelect <- renderUI({
    sliderInput(inputId = "dateRange", 
                label = "Choose Date Range:",
                min = as.Date("2006-01-01"), 
                max = as.Date("2016-12-31"),
                value = c(as.Date("2010-01-01"), as.Date("2012-12-31")))
  })

  output$y_var <- renderUI({
    selectInput(inputId = "y",
                label = "Primary Variable",
                choices = c("Level", "Boro", "Pct"),
                selected = "Pct")
  })
  
  output$color_var <- renderUI({
    selectInput(inputId = "clr",
                label = "Secondary Variable",
                choices = c("Level", "Boro", "Pct"),
                selected = "Level")
  })
  #*************************************  

  #*************************************
  #Render Crime Description Cloud in Sidebar Panel (Placeholder)
  wordcloud_rep <- repeatable(wordcloud)
  
  getTermMatrix <- (function(text) {
    
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"),"the", "and", "but"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
  })
  
  output$wrdcld <- renderPlot({
    
    req(points())
    DF<- points()
    
    text <- DF[!DF$OffenseDesc == '',] 
    #View(text)
    myCorpus = Corpus(VectorSource(DF$OffenseDesc))
    #print(myCorpus)
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"),"the", "and", "but"))
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    v<-sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v),freq=v)
    #head(d, 10)
    pal <- brewer.pal(4,"YlGnBu")
    #pal <- pal[-(1:4)]
    #print(brewer.pal(4,"YlGnBu"))
    wordcloud(myCorpus, max.words = 500, random.order = FALSE,colors = brewer.pal(4, "Dark2"))
    
  })
  
  #*************************************

  #****************************************************************************
  #****************************************************************************
  #MAPS TAB
  #****************************************************************************
  #****************************************************************************
  
  #*************************************  
  #Reactive expressions for Map Tab
  points <- reactive({
    req(input$boro, input$startDate)
    if (input$boro == "ALL"){
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% drop_na()
    }
    else {
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% filter(Boro == input$boro) %>% drop_na()
    }
    filtered_data
   })

  #*************************************  
  #Leaflet Map based on user select
  
  output$Nycgeomap =renderLeaflet({
    
    req(points())
    req(input$maptype)
    
    #print("started")
    #View(points())
    
    crime_map<- points()
    if(input$maptype =="heatmap")
    {
      
      #points_Lat_Long <- cbind(crime_map$Long, crime_map$Lat)
      #map = leaflet(crime_map) %>% addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")%>% 
       # addWebGLHeatmap(lng=~Long, lat=~Lat,size=20,units='px') 
    }
    else 
    {
      if(input$maptype =="density")
      {
        #print("density")
        
        points_Lat_Long <- cbind(crime_map$Long, crime_map$Lat)
        #View(points_Lat_Long)
        #crimeMap <- crimeMap[!is.na(Long)]
        #dat[ , date := as.IDate(date, "%m/%d/%Y")]
        
        ## MAKE CONTOUR LINES
        ## Note, bandwidth choice is based on MASS::bandwidth.nrd()
        kde <- bkde2D(points_Lat_Long,
                      bandwidth=c(.0045, .0068), gridsize = c(100,100))
        CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
        
        ## EXTRACT CONTOUR LINE LEVELS
        LEVS <- as.factor(sapply(CL, `[[`, "level"))
        NLEV <- length(levels(LEVS))
        
        ## CONVERT CONTOUR LINES TO POLYGONS
        pgons <- lapply(1:length(CL), function(i)
          Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
        spgons = SpatialPolygons(pgons)
        
        ## Leaflet map with polygons
        leaflet(spgons) %>% addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>% 
          addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
        
      }
      else
      {
        if(input$maptype =="precincts")
        {
       
            leaflet(nyc_precincts) %>%
              addTiles() %>%
              addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.3, fillColor = "blue", label = ~paste0("Pct: ", formatC(nyc_precincts@data[["Precinct"]])))
        
          
        }else
        {
        labs <- lapply(seq(nrow(points())), function(i) {
          paste0( '<p>', points()[i, "Level"], '<p></p>', 
                  points()[i, "Pct"], ', ', 
                  points()[i, "OffenseDesc"],'</p><p>' ) 
                })
        
        
        
        leaflet(data = points()) %>%
          addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
          addMarkers(
            lng = ~Long, 
            lat = ~Lat, 
            label = lapply(labs, HTML),
            #label = ~as.character(OffenseDesc),
            labelOptions = labelOptions(direction = "bottom",
                                        style = list(
                                          "color" = "red",
                                          "font-family" = "serif",
                                          "font-style" = "italic",
                                          "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                          "font-size" = "14px",
                                          "border-color" = "rgba(0,0,0,0.5)")),
            clusterOptions = markerClusterOptions())
        
        }
        
        
      }
    }
    
  })
  
  
  #*************************************

  #****************************************************************************
  #****************************************************************************
  #PLOTS TAB
  #****************************************************************************
  #****************************************************************************

  #*************************************  
  #Reactive Expressions for Plot Tab
  crime_data_filt <- reactive({
    req(input$dateRange)
    NycAppData %>% filter(DateStart >= input$dateRange[1] & DateStart <= input$dateRange[2]) %>% drop_na()
  })
  
  output$DataPlot <- renderPlotly({
    req(crime_data_filt())
    crimeSummary <- crime_data_filt() %>% group_by_(input$y, input$clr) %>% summarize(Count = n())
    p <- ggplotly(ggplot(data = crimeSummary, aes_string(x = "reorder(eval(as.name(input$y)), Count, sum)", y = "Count", fill = input$clr, label=input$y, label2="Count", label3=input$clr)) + geom_col() + coord_flip() + labs(x = input$y, y = "Crime Incidents"), tooltip = c("label", "label2", "label3"))
    p$elementId <- NULL
    p
  })

  output$SummaryTable = renderDataTable({
    req(crime_data_filt())
    if (input$y != input$clr) {
      crime_data_summary <- count(crime_data_filt(), eval(as.name(input$y)), eval(as.name(input$clr)))
      names(crime_data_summary) <- c(input$y, input$clr, "Count")
      crime_data_summary %>% group_by_(input$y) %>% mutate(sum1 = sum(Count)) %>% arrange(desc(sum1), desc(eval(as.name(input$y)))) %>% select(-sum1)
    } else {
      crime_data_filt() %>% group_by_(input$y) %>% summarize(Count = n()) %>% arrange(-Count)
    }
  })
  #*************************************  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

