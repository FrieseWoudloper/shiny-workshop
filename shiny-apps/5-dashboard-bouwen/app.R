# Libraries
library(readr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(jsonlite)

# Data
bevingen <- read_csv("../../data/bevingen.csv") 
veld <- read_json("../../data/groningen-veld.json")
brtAchtergrondkaart <- "http://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaartgrijs/EPSG:3857/{z}/{x}/{y}.png"
jaargegevens <- read_csv("../../data/gegevens-geaggregeerd-per-jaar.csv") %>%
                  mutate(hovertekst_aantal  = paste("Jaar:", jaar, "<br>", "Aantal:", aantal),
                         hovertekst_winning = paste("Jaar:", jaar, "<br>", "Gaswinning:", gaswinning, "miljard Nm<sup>3</sup>"))

# Gebruikersinterface
header <- dashboardHeader(title = "Bevingen dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Kaart",     tabName = "kaartTab",     icon = icon("map-marker", lib = "glyphicon")),
    menuItem("Histogram", tabName = "histogramTab", icon = icon("stats",      lib = "glyphicon")),
    menuItem("Grafiek",   tabName = "grafiekTab",   icon = icon("line-chart")),
    menuItem("Tabel",     tabName = "tabelTab",     icon = icon("list-alt",   lib = "glyphicon")),
    menuItem("Info",      tabName = "infoTab",      icon = icon("info-sign",  lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "kaartTab",
      box(
        width = 6, 
        leafletOutput(outputId = "kaart", height = 400)
      ),
      box(
        width = 2, 
        numericInput(inputId = "drempelwaarde", label = HTML("Magnitude &ge;"), 
                     min = -1, max = 4, step = 0.1, value = -0.8),
        hr(),
        checkboxInput(inputId = "veld", label = HTML("<strong>Groningen veld tonen</strong>"), value = FALSE),
        hr(),
        sliderInput(inputId = "doorzichtigheid", label = "Doorzichtigheid", 
                    min = 0, max = 100, step = 5, value = 30, ticks = FALSE, post = "%")
      )
    ),
    tabItem(
      "histogramTab",
      box(
        width = 6, 
        plotlyOutput(outputId = "histogram")
      ),
      box(
        width = 4, 
        sliderInput(inputId = "jaar", label = "Jaar", 
                    min = 1986, max = 2019, value = c(1986, 2019), sep = "")
      )
    ),
    tabItem(
      "grafiekTab",
      box(
        width = 6, 
        plotlyOutput(outputId = "grafiek"), 
        radioButtons(inputId = "y", label = "y-variabele:", 
                     choices = c("Aantal bevingen", "Gaswinning"), inline = TRUE)
      )  
    ),
    tabItem(
      "tabelTab",
      DTOutput(outputId = 'tabel'),
      downloadButton(outputId = 'downloadData', label = "Download gegevens")
            
    ),
    tabItem(
      "infoTab",
      HTML("<p>Dit is een voorbeeld Shiny app voor de workshop tijdens <a href = 'http://www.foss4g.nl'>FOSS4G.NL</a> op 20 juni 2019 in Delft.</p>"),
      HTML("<p>De code is te downloaden van <a href = 'https://github.com/friesewoudloper'>github.com/friesewoudloper</a>.</p>"),
      HTML("<p>De gegevens over de bevingen zijn afkomstig van het KNMI, die over de gaswinning en -velden van de NAM.</p>"),
      HTML("<p>Voor bevingen en gaswinning is de distributie gebruikt van <a href = 'https://Bevinggevoeld.nl'>Bevinggevoeld.nl</a>.</p>")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output) { 
  # Kaart
  pal <- colorNumeric(palette = "OrRd", domain = c(-1:4))
  
  opMagnitudeGefilterdeBevingen <- reactive({
                                     bevingen %>% 
                                       filter(magnitude >= input$drempelwaarde)
                                   })
     
  output$kaart <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = brtAchtergrondkaart,
               attribution = "Kaartgegevens &copy; Kadaster",
               options = tileOptions(minZoom = 6, maxZoom = 18)) %>%
      addMapPane("groningen veld",  zIndex = 410) %>%
      addMapPane("bevingen",  zIndex = 420) %>%
      addTopoJSON(topojson = veld, weight = 1, color = "#555555", opacity = 1, fillOpacity = 0.3,
                  options = pathOptions(pane = "groningen veld"), group = "groningen veld") %>%
      setView(6.8, 53.3, zoom = 9) %>%
      hideGroup("groningen veld")
  })
      
  observe({
    leafletProxy(mapId = "kaart") %>%
      clearShapes() %>%
      addCircles(data = opMagnitudeGefilterdeBevingen(), lng = ~ longitude, lat = ~ latitude, 
                 color = ~ pal(magnitude), weight = 1, 
                 radius = ~ 10 ^ magnitude / 10, 
                 fillOpacity = 1 - (input$doorzichtigheid / 100),
                 popup = ~ paste("Datum: ", format(datum, "%d-%m-%Y"), "<br>",
                                 "Locatie:", locatie, "<br>",
                                 "Magnitude:" , magnitude),
                 options = pathOptions(pane = "bevingen"))
  })

  observe({
    if (input$veld) {
      leafletProxy(mapId = "kaart") %>% showGroup("groningen veld")
    } else {
      leafletProxy(mapId = "kaart") %>% hideGroup("groningen veld")
    }
  })
      
  # Histogram
  opJaarGefilterdeBevingen <- reactive({
                                bevingen %>% 
                                  filter(jaar >= input$jaar[1] & jaar <= input$jaar[2])              
                              })
  
  output$histogram <- renderPlotly({
    plot_ly(data = opJaarGefilterdeBevingen(), x = ~ factor(magnitude)) %>%     
      add_histogram() %>%
      layout(
        xaxis = list(title = "Magnitude (Richter)", dtick = 10), 
        yaxis = list(title = "Frequentie"), 
        hovermode = 'compare'
      ) %>%
      config(displayModeBar = F)
  }) 
    
  # Grafieken met tijdreeksen
  y <- reactive(
    case_when(
      input$y == "Aantal bevingen" ~ jaargegevens$aantal,
      input$y == "Gaswinning"      ~ jaargegevens$gaswinning
    )
  )
  
  hovertekst <- reactive(
    case_when(
      input$y == "Aantal bevingen" ~ jaargegevens$hovertekst_aantal,
      input$y == "Gaswinning"      ~ jaargegevens$hovertekst_winning
    )
  )
  
  titel <- reactive(
    case_when(
      input$y == "Aantal bevingen" ~ "Aantal bevingen",
      input$y == "Gaswinning"      ~ "Gaswinning (miljard Nm<sup>3</sup>)"
    )
  )
  
  output$grafiek <- renderPlotly({
    plot_ly(data = jaargegevens, x = ~ jaar, y = y(), type = 'scatter', mode = 'markers+lines',
            hoverinfo = "text", text = hovertekst()) %>%
      layout(
        xaxis = list(title = "Jaar"),
        yaxis = list(title = titel())
      ) %>%
      config(displayModeBar = F)
  }) 
    
  # Tabel
  dutch <- "http://cdn.datatables.net/plug-ins/1.10.19/i18n/Dutch.json"
    
  output$tabel <- renderDT(
    bevingen, 
    options = list(language = list(url = dutch))
  )
    
  output$downloadData <- downloadHandler(
    filename = "bevingen.csv", 
    content = function(file){write_csv(bevingen, file)}
  )  
}

# Start de web app
shinyApp(ui, server)
