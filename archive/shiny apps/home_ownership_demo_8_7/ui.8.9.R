library(tidyr)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
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
  div(
    id = "layout",
    div(
      id = "sidebar",
      titlePanel("Housing Conditions in Pennsylvania Counties, 2021"),
      selectInput("county_select", "County",
                  choices = unique(dat$NAME),
                  selected = "Philadelphia"),
      shiny::p("Use this web app to check the home ownership rate in your county as of 2021. Hover over specific counties for more detailed information."),
      checkboxGroupInput("dataLayers", "Select Indicator",
                         choices = c(
                           "Rural Counties" = "rural"),
                         selected = character(0))),
    div(
      id = "map-container",
      leafletOutput(outputId = "leaflet", width = "100%", height = "100vh"),
      verbatimTextOutput("caption")
    )
  )
)

# 
# ui <- fluidPage(
#   titlePanel(title = "Housing Conditions in Pennsylvania Counties, 2021", windowTitle = "PHFA Housing Conditions 2021 Dashboard"),
# sidebarLayout(
#   position = "left",
#   sidebarPanel(
#     selectInput("county_select", "County",
#                 choices = unique(dat$NAME),
#     selected = "Philadelphia"),
#     shiny::p("Use this web app to check the home ownership rate in your county as of 2021. This map visualizes Pennsylvania counties' home ownership rate by chloropleth, and you can hover over specific counties for more detailed information."),
#     checkboxGroupInput("dataLayers", "Select Indicator",
#                        choices = c(
#                          "Rural Counties" = "rural"),
#                        selected = character(0))),
#   mainPanel(
#     tags$style(HTML("
#     .shiny-text-output {
#       background-color: transparent;
#       font-family: Arial, sans-serif;
#       color: gray;
#       font-size: 14px;
#       border: none;
#       padding: 0;
#     }
#   "))
#   ,
#   fluidRow(
#     column(
#       width = 12,
#       div(id = "map-container", leafletOutput(outputId = "leaflet", width = "100%", height = "80vh"))
#       ,
#       verbatimTextOutput("caption"))
#       )
#   )
#   )
# )

## Tester for zoom function
# 
# ui <- fluidPage(
#   leafletOutput("leaflet"),
#   selectInput("county_select", "Select County:", choices = unique(dat$NAME)),
#   verbatimTextOutput("caption")
# ))


