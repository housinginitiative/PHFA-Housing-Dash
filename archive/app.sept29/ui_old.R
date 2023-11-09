library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(stringr)
library(jsonlite)
library(magrittr)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("united"),
                 title = "Pennsylvania Counties Housing Explorer",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                      selectInput("variable", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct2021",
                                                                               "Vacant rental units (%)" = "renter_vacant_pct2011",
                                                                               "Median Rent ($)" = "med_gross_rent2016"), selected = "owner_occ_hh_pct2021"),
                      actionButton("rural", "Show Rural Counties"),
                      shiny::p("Use this web app to explore housing trends across Pennsylvania counties."),
                     )
                  ,
                    mainPanel(
                      tabsetPanel(type = "pills",
                                tabPanel(width = 8, title = h4("Data Mapper"), h3("Interactive Map"), leafletOutput("leaflet", height = "800px",
                                                        width = "1000px")),
                                 tabPanel(width = 8, title = h4("Statewide Comparisons"), h3("Homeownership Across Counties"), plotOutput("plot", height = "1000px",
                                                   width = "800px"),
                                          h6(textOutput("caption", container = span))),
                                 tabPanel(width = 8, title = h4("Data Summary and Download"), h3("Data Viewer"), tableOutput("tab")))
                      )
                    )
                 )



