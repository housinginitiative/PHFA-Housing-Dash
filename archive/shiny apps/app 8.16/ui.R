library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(pander)
library(stringr)
library(kableExtra)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML("
        #layout {
          display: flex;
        }
      #sidebar {
        width: 300px;
        height: 100vh;
        background-color: rgba(255, 255, 255, 0.7); /* Adjust alpha (0.7) for translucency */
        padding: 20px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease-in-out;
      }
        #map-container {
          flex-grow: 1;
          height: 100vh;
        }
      ")
    )
  ),

  titlePanel(title = "Pennsylvania Housing Conditions Explorer", windowTitle = "PHFA Housing Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("dataLayers", "Select Indicator",
                         choices = c(
                           "Rural Counties" = "rural"),
                         selected = character(0))
    ),
    
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Interactive Map", leafletOutput(outputId = "leaflet", width = "100%", height = "100vh")),
                tabPanel("Distribution", plotOutput("plot", width = "100%", height = "100vh"),
                         h6(textOutput("caption", container = span))),
                tabPanel("Summary Statistics", verbatimTextOutput("summary"))),

  )
  )
)
