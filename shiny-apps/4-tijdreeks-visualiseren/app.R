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
jaargegevens <- read_csv("../../data/gegevens-geaggregeerd-per-jaar.csv") %>%
                  mutate(hovertekst_aantal  = paste("Jaar:", jaar, "<br>", "Aantal:", aantal),
                         hovertekst_winning = paste("Jaar:", jaar, "<br>", "Gaswinning:", gaswinning, "miljard Nm<sup>3</sup>")) 

# Gebruikersinterface
header <- dashboardHeader(title = "Bevingen in de tijd")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(box(width = 6,
                          plotlyOutput(outputId = "grafiek"),
                          radioButtons(inputId = "y", label = "y-variabele:", 
                                       choices = c("Aantal bevingen", "Gaswinning"),
                                       inline = TRUE)
))

ui <- dashboardPage(header, sidebar, body)

# Server 
server <- function(input, output) {
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
    plot_ly(data = jaargegevens, x = ~ jaar, y = y(),
            type = 'scatter', mode = 'markers+lines',
            hoverinfo = "text", text = hovertekst()) %>%
      layout(
        xaxis = list(title = "Jaar"),
        yaxis = list(title = titel())
      ) %>%
      config(displayModeBar = F)
  }) 
}

# Start de web app
shinyApp(ui, server)