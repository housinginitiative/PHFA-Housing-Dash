library(shiny)
library(rsconnect)
library(rgdal)
library(tidyr)
library(ggplot2)
library(sf)
library(tidycensus)
library(dplyr)
library(mapview)
library(leaflet)
library(htmltools)
library(leafletCN)
library(tigris)
library(ggmap)

# Read data
dat <- st_read("/Users/annaduan-admin/Box\ Sync/PHFA\ dashboard/data\ panels/homeownership_pa_2021.geojson") %>%
  st_as_sf() %>%
  st_transform("EPSG:4326") %>%
  st_make_valid() %>%
  filter(is.na(owner_occ_hh_pct) == FALSE) 

# dat$quintile <- cut(dat$owner_occ_hh_pct, quantile(dat$owner_occ_hh_pct, probs = seq(0, 1, 0.2)), 
#                     labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# Map SAFMR categories
# Compute quintiles
quintiles <- quantile(dat$owner_occ_hh_pct, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Define a color palette for quintiles
color_palette <- colorQuantile("YlGnBu", domain = dat$owner_occ_hh_pct, n = 5)


labs_dat <- sprintf(
  "<strong>%s</strong><br/>
  Home Ownership Rate: %.0f%%<sup></sup>",
  dat$NAME, dat$owner_occ_hh_pct
) %>% lapply(htmltools::HTML)


title <- tags$div(
  HTML("<strong>2021 ACS Estimates</strong><br/>
        Hover to see individual counties"))



# Create leaflet 
server <- function(input, output, session) {
  output$leaflet <- renderLeaflet({
    leaflet(dat) %>%
      addPolygons(fillColor = ~color_palette(owner_occ_hh_pct),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labs_dat,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
       addProviderTiles(providers$CartoDB.Positron) %>%
        addControl(title, position = "topright") %>%
     addLegend(pal = color_palette, values = ~owner_occ_hh_pct, opacity = 0.7, title = "Home Ownership Rate<br/>by County (%)",
                position = "bottomright")
  })
}

shinyApp(ui, server)

