#
# Dit is een Shiny web app.
# Je kunt 'm starten door op de Run App knop rechtsboven in dit scherm te klikken.
#

# Libraries
library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(jsonlite)

# Data 
bevingen <- read_csv("../../data/bevingen.csv")
veld <- read_json("../../data/groningen-veld.json")
brtAchtergrondkaart <- paste0("http://geodata.nationaalgeoregister.nl/",
                              "tiles/service/wmts/brtachtergrondkaartgrijs/",
                              "EPSG:3857/{z}/{x}/{y}.png")

# Kleurenpalet
pal <- colorNumeric(palette = "OrRd", domain = c(-1:4))

# Gebruikersinterface
header <- dashboardHeader(title = "Kaart")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  box(width = 6,
      leafletOutput(outputId = "kaart", height = 400)),
  box(width = 2,
      numericInput(inputId = "drempelwaarde", label = "Drempelwaarde",
                   min = -1, max = 4, step = 0.1, value = -1),
      hr(),
      checkboxInput(inputId = "veld", label = "Groningen veld tonen", value = FALSE),
      hr(),
      sliderInput(inputId = "doorzichtigheid", label = "Doorzichtigheid", 
                  min = 0, max = 100, step = 5, value = 30, ticks = FALSE, post = "%"))
)
ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output) {
  gefilterdeBevingen <- reactive({
    bevingen %>%
      filter(magnitude >= input$drempelwaarde)
  })
  
  output$kaart <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = brtAchtergrondkaart,
               attribution = "Kaartgegevens &copy; Kadaster",
               options = tileOptions(minZoom = 6, maxZoom = 18)) %>%
      addMapPane("groningen veld", zIndex = 410) %>%
      addMapPane("bevingen", zIndex = 420) %>%
      addTopoJSON(topojson = veld, 
                  weight = 1, color = "grey", fillOpacity = 0.3,
                  options = pathOptions(pane = "groningen veld"), 
                  group = "groningen veld") %>%
      setView(6.8, 53.3, zoom = 9) %>%
      hideGroup("groningen veld")
  }) 

    observe({
    leafletProxy(mapId = "kaart") %>%
      clearShapes() %>%
      addCircles(data = gefilterdeBevingen(), 
                 lng = ~ longitude, lat = ~ latitude,
                 color = ~ pal(magnitude), weight = 1,
                 radius = ~ 10 ^ magnitude / 10, 
                 # fillOpacity is afhankelijk van slider input!
                 fillOpacity = 1 - (input$doorzichtigheid / 100), 
                 popup = ~ paste("Datum: ", format(datum, "%d-%m-%Y"), "<br>",
                                 "Locatie:", locatie, "<br>",
                                 "Magnitude:" , magnitude),
                 options = pathOptions(pane = "bevingen")) 
  })  
  
  observe({
    if (input$veld == TRUE) {
      leafletProxy(mapId = "kaart") %>% showGroup("groningen veld")
    } else {
      leafletProxy(mapId = "kaart") %>% hideGroup("groningen veld")
    }
  })
}

# Start de app
shinyApp(ui, server)