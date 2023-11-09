library(tidyr)
library(sf)
library(tidycensus)
library(dplyr)
library(leaflet)
library(htmltools)
library(leafletCN)
library(tigris)
library(stringr)
library(rsconnect)
library(shiny)
library(rsconnect)



title <- tags$div(
  HTML("<strong>2021 ACS Estimates</strong><br/>
        Hover to see individual counties"))




# Create leaflet
server <- function(input, output, session) {
  
  # Read & prep data
  # original geojson
  
  dat <- st_read("homeownership_pa_2021.geojson") %>%
    st_as_sf() %>%
    st_transform("EPSG:4326") %>%
    st_make_valid() %>%
    filter(is.na(owner_occ_hh_pct) == FALSE) %>%
    dplyr::mutate(NAME = word(NAME, 1))
  
  ## use centroids for search/zoom function
  dat_centroids <- dat %>%
    st_centroid() %>%
    dplyr::select(NAME) %>%
    mutate(lat = st_coordinates(geometry)[, "Y"],
           lon = st_coordinates(geometry)[, "X"]) %>%
    st_drop_geometry()
  
  dat <- dat %>% left_join(dat_centroids, by = "NAME")
  
  # Map SAFMR categories
  # Compute quintiles
  quintiles <- c(52.00, 71.00, 74.00, 78.00, 85.00)
  
  # Create color palette based on data range
  color_palette <- colorQuantile("YlGnBu", domain = 51:86, n = 5, probs = seq(.51, .86, (.86-.51)/3))
  
  
 # labels
  labs_dat <- sprintf(
    "<strong>%s</strong><br/>
  Home Ownership Rate: %.0f%%<sup></sup><br/>
  Statewide Median: 74%%",
    dat$NAME, dat$owner_occ_hh_pct
  ) %>% lapply(htmltools::HTML)
  
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
     addLegend(pal = color_palette, values = ~quintiles, opacity = 0.7, title = "Home Ownership Rate<br/>by County (%)",
                position = "bottomright")
  })

  
  # zoom to county
  observeEvent(input$county_select, {
    County <- input$county_select
    dat <- dat[dat$NAME == County, ]

    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = dat$lon, lat = dat$lat, zoom = 10) %>%
      addMarkers(lng = dat$lon, lat = dat$lat)
  })


  # caption
  output$caption <- renderText({
    "Data from the American Community Survey 5 year estimates, 2021."
  })

}

# # Create leaflet 
# server <- function(input, output, session) {
#   
#   output$leaflet <- renderLeaflet({
#     leaflet(dat) %>%
#       addPolygons(fillColor = ~color_palette(owner_occ_hh_pct),
#                   weight = 2,
#                   opacity = 1,
#                   color = "white",
#                   dashArray = "3",
#                   fillOpacity = 0.7,
#                   highlightOptions = highlightOptions(
#                     weight = 5,
#                     color = "#666",
#                     dashArray = "",
#                     fillOpacity = 0.7,
#                     bringToFront = TRUE),
#                   label = labs_dat,
#                   labelOptions = labelOptions(
#                     style = list("font-weight" = "normal", padding = "3px 8px"),
#                     textsize = "15px",
#                     direction = "auto")) %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       addControl(title, position = "topright") %>%
#       addLegend(pal = color_palette, values = ~owner_occ_hh_pct, opacity = 0.7, title = "Home Ownership Rate<br/>by County (%)",
#                 position = "bottomright") %>%
#       fitBounds(lng1 = -80.51989, lat1 = 39.7198, lng2 = -74.68952, lat2 = 42.26986)  # Set bounding box for Pennsylvania
#   })
#   
#   observeEvent(input$county_select, {
#     county <- input$county_select
#     county_data <- dat[dat$NAME == county, ]
#     
#     leafletProxy("leaflet") %>%
#       clearMarkers() %>%
#       setView(lng = county_data$lon, lat = county_data$lat, zoom = 10)
#   })
# 
# }

shinyApp(ui = ui, server = server)

