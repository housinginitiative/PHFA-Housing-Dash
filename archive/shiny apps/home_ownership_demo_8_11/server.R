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

#### Data processing ####  
dat <- st_read("homeownership_pa_2021.geojson") %>%
st_as_sf() %>%
  st_transform("EPSG:4326") %>%
  st_make_valid() %>%
  filter(is.na(owner_occ_hh_pct) == FALSE) %>%
  dplyr::mutate(NAME = word(NAME, 1))

#### Leaflet formatting ####  
##### palette #####
# Compute quintiles
#quintiles <- quantile(dat$owner_occ_hh_pct)
quintiles <- c(52, 71, 74, 78, 85)
names(quintiles) = c("52%", "71%", "74%", "78%", "85%")
# Create color palette based on data range
color_palette <- colorBin("YlGnBu", bins = quintiles, domain = dat$owner_occ_hh_pct)

##### labels #####
labs_dat <- sprintf(
  "<strong>%s</strong><br/>
  Home Ownership Rate: %.0f%%<sup></sup><br/>
  Statewide Median: 74%%",
  dat$NAME, dat$owner_occ_hh_pct
) %>% lapply(htmltools::HTML)

##### title #####
title_dat <- tags$div(
  HTML("<strong>Homeownership rates by County (%)</strong><br/>
        Hover to see individual counties"))

#### server function ####
server <- function(input, output, session) {
  output$caption <- renderText({
    "Data from the American Community Survey 5 year estimates, 2021."
  })
  
##### summary #####
  output$summary <- renderPrint({
    dat %>%
      dplyr::select(owner_occ_hh, owner_occ_hh_pct) %>%
      st_drop_geometry() %>%
      rename(owner_households = owner_occ_hh,
             ownership_rate = owner_occ_hh_pct) %>%
   summary() %>%
   pander(caption = "Home Ownership Rate")
  })
  
##### plot #####
  output$plot <- renderPlot({
    ggplot(dat, aes(x = reorder(NAME, owner_occ_hh_pct), y = owner_occ_hh_pct, fill = owner_occ_hh_pct)) +
             geom_bar(color = "transparent", stat = "identity") +
             scale_fill_distiller(palette = "YlGnBu", direction = 1) +
             labs(title = "Homeownership rate by County", subtitle = "PA Counties, 2021", x = "", y = "Homeownership Rate (%)", fill = "") +
             theme_minimal() +
      coord_flip()
    })
  
##### leaflet #####
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
      addControl(title_dat, position = "topright") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend(pal = color_palette, title = "", opacity = 1, values = ~quintiles,
                position = "bottomright")
  })
  
}


