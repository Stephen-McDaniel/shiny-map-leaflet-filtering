# 2017-01-03 in response to Google Groups thread by julia 
# Response from Stephen McDaniel at PowerTrip Analytics
#
# Program: app.R 
#    Data: ./data/2016_Satisfaction.csv
#
# Some of the tricky areas for newcomers to shiny and leaflet are
# addressed in this example
# 1) Map data is unhappy with unmappable latitude/longitude values
#    You can either filter them in the initial source (read at app initialization)
#    Or you can filter these out just in the mapping data pipeline
# 2) Map is drawn with non-reactive data, as we won't redraw this (better experience)
# 3) Map inputs require the tilde (~) prefix
# 4) When using range sliders, it is a good practice 
#    to show the full-range of survey values, not just the min/max data values
#    as the endpoints
# 5) For better visualization of the results, used circle sizing rather than 
#    just pins, you can also click these for the popup
# 6) Added a provider tile that makes the data popout versus the default map used by leaflet
#    There are many provider maps to choose from, see https://leaflet-extras.github.io/leaflet-providers/preview/
#
# License: MIT License
# Attribution, package authors for shiny and leaflet on CRAN.
#    Location data from Patrick Sinclair, https://classic.scraperwiki.com/scrapers/list_of_uk_univeristies_with_latitudelongitude/

library(dplyr)
library(leaflet)
library(shiny)

# Randomly generated 2016 satisfaction values by SMD
mapData <- read.csv("./data/2016_Satisfaction.csv") %>%
    filter(!is.na(Latitude) & !is.na(Longitude))

ui <- bootstrapPage(
   tags$style(type = "text/css", "html, 
              body {width:100%;height:100%}"),
   
   leafletOutput("uniSmap", width = "100%", height = "100%"),
   
   absolutePanel(
      top = 50,
      right = 50,
      sliderInput(
         "range",
         "Satisfaction Score",
         min = 1,
         max = 5,
         value = round(range(mapData$Sat_2016, na.rm = TRUE), 1),
         step = 0.1
      )
   )
)

server <- function(input, output, session) {

   filteredData <- reactive({
      mapData %>%
         filter(Sat_2016 >= input$range[1] &
            Sat_2016 <= input$range[2]) 
   })
   
   output$uniSmap <- renderLeaflet({
      # as the map is only drawn once
      # use non-reactive dataframe, mapData 
      leaflet(mapData) %>%
         addTiles() %>%
         fitBounds(~min(Longitude), ~min(Latitude), 
             ~max(Longitude), ~max(Latitude))
   })
   
   # Incremental changes to the map performed in an observer.
   
   observe({

      leafletProxy("uniSmap", data = filteredData()) %>%
         clearShapes() %>% 
         clearPopups() %>% 
         clearMarkers() %>%
         addMarkers(
            lng = ~Longitude, # note the tildes before values, required
            lat = ~Latitude,
            popup = ~paste(
               Institution,
               "<br>",
               "Overall Satisfaction:",
               Sat_2016,
               "<br>"
            )
         )
      
   })
   
}

shinyApp(ui = ui, server = server)