library(conflicted)
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
library(HatchedPolygons)
library(tidyverse)
library(tigris)
library(mapview)


#### Data processing ####  

panel.sf <- st_read("PHFA_dash_data_October3.geojson") %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf()

panel <- st_drop_geometry(panel.sf)


rural <- hatched.SpatialPolygons(panel.sf %>% dplyr::filter(rural == 1), density = 13, angle = c(45, 135)) %>%
  st_union() %>%
  st_as_sf()

#### Leaflet formatting ####  
##### palette #####
# Compute quintiles

# quintiles <- c(52, 71, 74, 78, 85)
# names(quintiles) = c("52%", "71%", "74%", "78%", "85%")
# # Create color palette based on data range
# color_palette <- colorBin("YlGnBu", bins = quintiles, domain = dat$owner_occ_hh_pct_21)


##### title #####
title_dat <- tags$div(
  HTML("<strong>Indicators by County (%)</strong><br/>
        Hover to see individual counties"))

#### server function ####
server <- function(input, output, session) {
  

  #### reactive palette ####
  mapPalette <- reactive({
    colorNumeric(
      palette = "YlGnBu",
      domain = NULL,
      na.color = "gray",
      reverse = FALSE)
  })
  
  #### reactive dataframe ####
  varInput.sf <- reactive({
    input$variable
    print(input$variable)
  })
  
  varInput <- reactive({
    input$variable
  })
  
  dat.sf = reactive({
    panel.sf %>%
      dplyr::select(variable = input$variable, county, geometry, lat, lon)
  })
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable)
  })
  
  

#### leaflet ####
output$leaflet <- renderLeaflet({
  leaflet() %>%
    addPolygons(data = dat.sf(), fillColor = ~mapPalette()(dat.sf()$variable),
                color = "white",
                weight = 1,
                opacity = 1,
                dashArray = "3",
                fillOpacity = 0.8,  # Reduce opacity here
                highlightOptions = highlightOptions(
                  weight = 1,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.5,
                  bringToFront = TRUE)
               # label = labs_dat,
               # labelOptions = labelOptions(
               #   style = list("font-weight" = "normal", padding = "3px 8px"),
               #   textsize = "15px",
               #   direction = "auto")
               ) %>%
    # addLegend(pal = mapPalette(), title = "", opacity = 1, values = dat.sf()$variable,
    #           position = "bottomright") %>%
    # addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, label =  ~as.character(dat.sf()$county),
    #                     labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
    #                       "color" = "DarkCyan",
    #                       "font-family" = "sans-serif",
    #                       "font-size" = "12px")),
    #                     group = "txt_labels") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    groupOptions("txt_labels", zoomLevels = 12:100)
  })
#     addLabelOnlyMarkers(~lon, ~lat, label =  ~as.character(NAME),
#                         labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
#                           "color" = "DarkSlateBlue",
#                           "font-family" = "sans-serif",
#                          # "font-weight" = "bold",
#                           "font-size" = "12px")),
#                         group = "txt_labels") %>%
#     addControl(title_dat, position = "topright") %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addLegend(pal = color_palette, title = "Percent", opacity = 1, values = ~quintiles,
#               position = "bottomright") %>%
#     groupOptions("txt_labels", zoomLevels = 8:100)  
# })

x = reactiveVal(1)
observeEvent(input$rural,{
  x(x()+1) # increment x by 1
  x <- as.numeric(x())
})

observeEvent(input$rural, {
  if((x() %% 2) == 0) {
    leafletProxy("leaflet") %>%
      addPolylines(
        data = rural,
        weight = 1.5,
        layerId  = "rural") 
  } else {
    leafletProxy("leaflet") %>%
      removeShape(layerId  = "rural")
  }
    })


##### plot #####
output$plot <- renderPlot({
  v <- input$variable
  
  ggplot(data = dat(), aes(x = county, y = variable, fill = variable)) +
    geom_bar(color = "transparent", stat = "identity") +
    geom_text(aes(label=variable), hjust=0, colour = "navy", alpha = 0.6,  position = "dodge") +
    scale_fill_distiller(palette = "YlGnBu", direction = 1) +
    labs(title = "", fill = v, color = "Rural County") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = 16),
          axis.title.y = element_text(face = "bold")) +
    coord_flip()
})

##### summary #####
output$tab <- renderTable({
    data.frame(quartile_1 = quantile(dat()$variable, probs = 0.25, na.rm = TRUE),
              mean = mean(dat()$variable, na.rm = TRUE),
              median = median(dat()$variable, na.rm = TRUE),
              quartile_3 = quantile(dat()$variable, probs = 0.75, na.rm = TRUE),
              max = max(dat()$variable, na.rm = TRUE)) 
  
  
})

}
