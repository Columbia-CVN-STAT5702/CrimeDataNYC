#
# This is a web application submitted in partial fulfillment of the requirements for
# the STAT W5701 Final Project

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NYC Crime Data Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput("startDate", "Start Date", "06-01-2012"),
         selectInput("boro", "Borough", choices = c("ALL", "MANHATTAN", "BROOKLYN", "QUEENS", "BRONX", "STATEN ISLAND"), "ALL"),
         selectInput("Pct", "Precinct", choices = c("1", "104"), "1")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("NycMap"),
         p(),
         actionButton("updateMap", "Show Points")
      )
   )
)

#Load the data file for the application
load("NycAppData.RData")

#Server Function
server <- function(input, output) {
  
  points <- eventReactive(input$updateMap, {
    if (input$boro == "ALL"){
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% select("Lat", "Long") %>% drop_na()
    }
    else {
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% filter(Boro == input$boro) %>% 
        select("Lat", "Long") %>% drop_na()
    }
    cbind(filtered_data$Long, filtered_data$Lat)
   }, ignoreNULL = FALSE)
  
  output$NycMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

