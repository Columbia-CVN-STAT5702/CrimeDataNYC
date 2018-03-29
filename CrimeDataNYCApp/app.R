#
# This is a web application submitted in partial fulfillment of the requirements for
# the STAT W5701 Final Project

library(shiny)
library(leaflet)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NYC Crime Data Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput("startDate", "Start Date"),
         dateInput("endDate", "End Date"),
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

#Read in the NycAppData1.csv file for use in the app
appData <- fread("../Data_Files/NycAppData1.csv", na.strings="", stringsAsFactors = TRUE)
#Convert start date to the proper date format
appData$DateStart <- as.Date(appData$DateStart, format='%m/%d/%Y')

#Server Function
server <- function(input, output) {
  points <- eventReactive(input$updateMap, {
    data_filter <- appData %>% filter(DateStart >= input$startDate, DateStart <= input$endDate) %>% select("Lat", "Long")
   }, ignoreNULL = FALSE)
  
  output$NycMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = as.numeric(points())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

