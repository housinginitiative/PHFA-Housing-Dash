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

ui <- fluidPage(

  titlePanel(title = "Pennsylvania Housing Conditions Explorer", windowTitle = "Idk"),
  
  sidebarLayout(
    sidebarPanel(),
    
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Interactive Map", leafletOutput("leaflet")),
                tabPanel("Distribution", plotOutput("plot"),
                         h6(textOutput("caption", container = span))),
                tabPanel("Summary Statistics", verbatimTextOutput("summary"))),

  )
  )
)
