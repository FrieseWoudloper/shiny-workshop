#
# Dit is een Shiny web app.
# Je kunt 'm starten door op de Run App knop rechtsboven in dit scherm te klikken.
#

# Libraries
library(readr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)

# Data
bevingen <- read_csv("../../data/bevingen.csv")

# Gebruikersinterface
header <- dashboardHeader(title = "Histogram")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(box(width = 6, plotlyOutput(outputId = "histogram")),
                      box(
                        width = 4,
                        sliderInput(inputId = "jaar", label = "Jaar",
                                    min = 1986, max = 2019, value = c(1986, 2019), sep = "")
                      ))

ui <- dashboardPage(header, sidebar, body)

# Server 
server <- function(input, output) {
  # Filter data op basis van slider input
  gefilterdeBevingen <- reactive({
    bevingen %>%
      filter(jaar >= input$jaar[1] & jaar <= input$jaar[2])
  })
  
  # Render histogram
  output$histogram <- renderPlotly({
    # Databron is nu de reactive source!
    plot_ly(data = gefilterdeBevingen(), x = ~ factor(magnitude)) %>%
      add_histogram() %>%
      layout(
        xaxis = list(title = "Magnitude (Richter)", dtick = 10),
        yaxis = list(title = "Frequentie"),
        hovermode = 'compare'
      ) %>%
      config(displayModeBar = F)
  }) 
}

# Start de web app
shinyApp(ui, server)