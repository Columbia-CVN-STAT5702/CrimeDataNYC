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

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
   
   # Application title
   titlePanel("NYC Crime Data Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        HTML('<p><img src="Columbia.png"/></p>'),
        conditionalPanel(condition="input.tabselected==1",
                         uiOutput("dateSelect"),
                         uiOutput("boroSelect")),
        
        conditionalPanel(condition="input.tabselected==2",
                         uiOutput("dateRangeSelect"),
                         uiOutput("y_var"),
                         uiOutput("color_var"))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Map", value = 1, leafletOutput("NycMap")),
                    tabPanel("Plots", value = 2, plotlyOutput("DataPlot", height=800), dataTableOutput("SummaryTable")),
                    id = "tabselected")
      )
   )
)

#Load the data files for the application
load("NycAppData.RData")
#load("Precincts.RData")

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
  
  output$boroSelect <- renderUI({
    selectInput(inputId = "boro",
                label = "Borough",
                choices = c("ALL", "MANHATTAN", "BROOKLYN", "QUEENS", "BRONX", "STATEN ISLAND"),
                selected = "ALL")
  })
  #*************************************
  
  #*************************************  
  #Render input UI when on the Plot tab
  output$dateRangeSelect <- renderUI({
    dateRangeInput(inputId = "dateRange",
                   label = "Select Date Range",
                   start = "06-01-2012",
                   end = "06-30-2012",
                   min = "01-01-2006",
                   max = "12-31-2016")
  })
  
  output$y_var <- renderUI({
    selectInput(inputId = "y",
                label = "Select Y-axis",
                choices = c("Level", "Boro", "Pct"),
                selected = "Pct")
  })
  
  output$color_var <- renderUI({
    selectInput(inputId = "clr",
                label = "Select Alternate Variable",
                choices = c("Level", "Boro", "Pct"),
                selected = "Level")
  })
  #*************************************  
  
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
    #cbind(filtered_data$Long, filtered_data$Lat)
    filtered_data
   })
  
  output$NycMap = renderLeaflet({
    req(points())
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
    
  })
  
  #*************************************  
  
  #*************************************  
  #Reactive Expressions for Plot Tab
  crime_data_filt <- reactive({
    req(input$dateRange)
    NycAppData %>% filter(DateStart >= input$dateRange[1] & DateStart <= input$dateRange[2])
  })
  
  output$DataPlot <- renderPlotly({
    req(crime_data_filt())
    crimeSummary <- crime_data_filt() %>% group_by_(input$y, input$clr) %>% summarize(Count = n())
    p <- ggplotly(ggplot(data = crimeSummary, aes_string(x = input$y, y = "Count", fill = input$clr)) + geom_col() + coord_flip() + labs(y = "Crime Incidents"))
    p$elementId <- NULL
    p
  })
  
  output$SummaryTable = renderDataTable({
    req(crime_data_filt())
    crime_data_filt() %>% group_by_(input$y, input$clr) %>% summarize(Count = n())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

