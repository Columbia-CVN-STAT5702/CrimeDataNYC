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
         dateInput("startDate", "Date", value = "06-01-2012", min = "01-01-2006", max = "12-31-16"),
         selectInput("boro", "Borough", choices = c("ALL", "MANHATTAN", "BROOKLYN", "QUEENS", "BRONX", "STATEN ISLAND"), "ALL")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("NycMap"),
         p(),
         actionButton("updateMap", "Update Points")
      )
   )
)

#Load the data file for the application
load("NycAppData.RData")

#Server Function
server <- function(input, output) {
  
  points <- eventReactive(input$updateMap, {
    if (input$boro == "ALL"){
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% drop_na()
    }
    else {
      filtered_data <- NycAppData %>% filter(DateStart == input$startDate) %>% filter(Boro == input$boro) %>% drop_na()
    }
    #cbind(filtered_data$Long, filtered_data$Lat)
    filtered_data
   }, ignoreNULL = FALSE)
  
  output$NycMap <- renderLeaflet({
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)

