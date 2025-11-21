library(conflicted)
library(shiny)
library(rsconnect)
library(tidyr)
library(sf)
library(leaflet)
library(stringr)
library(tidyverse)
library(mapview)
library(plotly)
library(DT)
library(scales)
library(tigris)
conflicts_prefer(dplyr::filter)


#### Data processing ####  
# original data
dat <- st_read("PHFA_dash_data_June.25.geojson") %>%
  dplyr::mutate(housing_balance = ifelse(housing_balance < 0, abs(housing_balance), 0))

# spatial panel
panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  dplyr::mutate(
    lat = st_coordinates(.)[, 2],
    lon = st_coordinates(.)[, 1]
  ) %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf()

# non-spatial panel
panel <- st_drop_geometry(panel.sf)

# rural counties
rural <- panel.sf %>% 
  dplyr::filter(rural == 1) %>%
  st_union() %>%
  st_as_sf()

# state averages for variables
state_avg <- st_read("state_avg_06-25.csv") %>%
  dplyr::mutate(housing_balance = as.numeric(ifelse(as.numeric(housing_balance) < 0, abs(as.numeric(housing_balance)), 0)))

# variable aliases for display
variable_aliases <- c(
"owner_occ_hh_pct2023" = "Homeownership rate (2023)",
"white_own_occ_hh_pct2023" = "White homeownership rate (2023)",
"black_own_occ_hh_pct2023" = "Black homeownership rate (2023)",
"hisp_lat_own_occ_hh_pct2023" = "Hispanic or Latino homeownership rate (2023)",
"renter_occ_hh_pct2023" = "Rentership rate (2023)",
"renter_vacant_pct2023" = "Rental vacancy rate (2023)",
"med_age_home2023" = "Median year home built (2023)",
"med_home_value2023" = "Median home value (2023)",
"internet_hh_pct2023" = "Internet access rate (2023)",
"rent_burdened_pct2023" = "Rent burden rate (2023)",
"mortgage_burdened_pct2023" = "Mortgage burden rate (2023)",
"med_gross_rent2023" = "Median gross rent (2023)",
"housing_balance" = "Affordable housing shortage (2021)",
"resunitpermitsrate23" = "Residential permit rate (2023)"
)

# prefixes for legend labels 
variable_prefix <- c(
"owner_occ_hh_pct2023" = "",
"white_own_occ_hh_pct2023" = "",
"black_own_occ_hh_pct2023" = "",
"hisp_lat_own_occ_hh_pct2023" = "",
"renter_occ_hh_pct2023" = "",
"renter_vacant_pct2023" = "",
"med_age_home2023" = "",
"med_home_value2023" = "$",
"internet_hh_pct2023" = "",
"rent_burdened_pct2023" = "",
"mortgage_burdened_pct2023" = "",
"med_gross_rent2023" = "$",
"housing_balance" = "",
"resunitpermitsrate23" = ""
)

# suffixes for legend labels
variable_suffix <- c(
  "owner_occ_hh_pct2023" = "%",
  "white_own_occ_hh_pct2023" = "%",
  "black_own_occ_hh_pct2023" = "%",
  "hisp_lat_own_occ_hh_pct2023" = "%",
  "renter_occ_hh_pct2023" = "%",
  "renter_vacant_pct2023" = "%",
  "med_age_home2023" = "",
  "med_home_value2023" = "",
  "internet_hh_pct2023" = "%",
  "rent_burdened_pct2023" = "%",
  "mortgage_burdened_pct2023" = "%",
  "med_gross_rent2023" = "",
  "housing_balance" = " units",
  "resunitpermitsrate23" = " permits"
)

# variable type
variable_type <- c(
  "owner_occ_hh_pct2023" = "percent",
  "white_own_occ_hh_pct2023" = "percent",
  "black_own_occ_hh_pct2023" = "percent",
  "hisp_lat_own_occ_hh_pct2023" = "percent",
  "renter_occ_hh_pct2023" = "percent",
  "renter_vacant_pct2023" = "percent",
  "med_age_home2023" = "year",
  "med_home_value2023" = "currency",
  "internet_hh_pct2023" = "percent",
  "rent_burdened_pct2023" = "percent",
  "mortgage_burdened_pct2023" = "percent",
  "med_gross_rent2023" = "currency",
  "housing_balance" = "",
  "resunitpermitsrate23" = ""
)
#### Server ####
server <- function(input, output, session) {

  pa_avg = reactive({
    state_avg %>%
      dplyr::select(variable = input$variable)})
  
  dat.sf = reactive({
    panel.sf %>%
      dplyr::select(variable = input$variable, county, geometry, lat, lon, rural) %>%
      st_as_sf()
    })
  observeEvent(dat.sf(), {print(dat.sf())})
  
  dat = reactive({
    panel %>%
      dplyr::select(county, variable = input$variable, 
                    variable_bar = input$variable_bar, 
                    variable_scatter_x = input$variable_scatter_x, variable_scatter_y = input$variable_scatter_y, 
                    variable_tab = input$variable_tab,
                    rural)})
  
  palette_blue <- c("#C4E8F8", "#4292c6", "#2171b5", "#08519c", "#08306b")
  #### reactive palette ####
  mapPalette <- reactive({
    leaflet::colorQuantile(
      na.color = "gray",
      palette = palette_blue,
      domain = NULL,
      n = 5, 
      reverse = FALSE
      )})
  
  #### bar plot ####
  output$plot <- renderPlotly({
    counties <- input$barp_counties %>%
      as.list()
    
    v <- input$variable_bar
    df <- dat() %>% as.data.frame()
    
    df <- df %>%
      dplyr::mutate(rural_score = ifelse(rural == 1, 100000, 0),
             order_id = rural_score + variable_bar) %>%
      dplyr::filter(county %in% counties) %>%
      dplyr::mutate(rural = case_when(rural == 1 ~ "Rural",
                                      rural == 0 ~ "Urban"))
    
    v <- input$variable_bar
    prefix <- variable_prefix[v]
    alias <- variable_aliases[v]
    suffix <- variable_suffix[v]
    type <- variable_type[v]
    
##custom hover text##
    df$hover_text <- paste0(alias, ": ", prefix, format(df$variable_bar, big.mark = ifelse(type == "year", "", ",")), suffix, "<br>",
                           "Rural or Urban: ", df$rural, "<br>",
                           "County: ", df$county, "<br>")
  
barp <- ggplot(data = df, aes(x = reorder(county, order_id), y = variable_bar)) +
  geom_bar(color = "transparent", stat = "identity", aes(fill = as.factor(rural), text = hover_text)) +
  scale_fill_manual(values = c("#4e72aa", "#94bcda")) +
  labs(title = paste(alias, "by PA county", sep = " "),
       caption = "Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, July 2025, https://housinginitiative.shinyapps.io/PHFA_Housing_Dashboard/. ", 
       fill = "",
       y = alias, x = "") +
  scale_y_continuous(labels = scales::number_format(prefix = prefix, suffix = suffix, big.mark = ifelse(type == "year", "", ","))) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() 

ggplotly(barp,
         tooltip = "text") %>%
  plotly::layout(
    margin = list(l = 50, r = 50, b = 100, t = 50),
    annotations = list(x = 0.5, y = -0.2, text = "Source: Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, July 2025, https://housinginitiative.shinyapps.io/PHFA_Housing_Dashboard/. ",
                            xref='paper', yref='paper', showarrow = F, 
                            xanchor='center', yanchor='bottom', xshift=0, yshift=0,
                            font = list(size = 12, color = "gray")),
    yaxis = list(showticksuffix = "first",
                 ticksuffix = "%"),
    legend = list(
      orientation = "h",
      x = "0.5",
      xanchor = "center",
      y = -.25)
    )

  })
  
#### leaflet menu indicator information #####
  output$indicator_desc_text <- renderText({
  description <- c(
    "owner_occ_hh_pct2023" = "Homeownership rate (%) is the percentage of all households that own their homes. A higher rate indicates a greater proportion of homeowners in the area. Hover over a county to see that county's rate and the state average rate.",
    "white_own_occ_hh_pct2023" = "White homeownership rate (%) is the percentage of white households that own their homes. A higher rate indicates a greater proportion of white homeowners in the area. Hover over a county to see that county's rate and the state average rate.",
    "black_own_occ_hh_pct2023" = "Black homeownership rate (%) is the percentage of Black households that own their homes. A higher rate indicates a greater proportion of Black homeowners in the area. Hover over a county to see that county's rate and the state average rate.",
    "hisp_lat_own_occ_hh_pct2023" = "Hispanic or Latino homeownership rate (%) is the percentage of Hispanic or Latino households that own their homes. A higher rate indicates a greater proportion of Hispanic or Latino homeowners in the area. Hover over a county to see that county's rate and the state average rate.",
    "renter_occ_hh_pct2023" = "The rentership rate shows the share of households in a county that rent their homes. Younger households and households with limited incomes are more likely to rent than older households and households with higher incomes. Hover over a county to see that county's rate and the state average rate.",
    "renter_vacant_pct2023" = "Rental vacancy rate (%) represents the percentage of rental units that are currently unoccupied. A higher percentage can suggest that there's a surplus of rental housing or potentially decreased demand. Hover over a county to see that county's rate and the state average rate.",
    "med_age_home2023" = "Median year home built indicates the midpoint age of homes in a specific area. Older median years can suggest historical neighborhoods, while later median years might indicate newer developments. Hover over a county to see that county's median year of home construction and the state median year of home construction.",
    "med_home_value2023" = "Median home value ($) is the midpoint value of homes in the area. This can provide an insight into the overall affordability and property values of a region. Hover over a county to see that county's median value and the state median value.",
    "internet_hh_pct2023" = "Internet access rate (%) is the percentage of households that have access to the internet. This can provide insights into the area's technological infrastructure and development. Hover over a county to see that county's rate and the state average rate.",
    "rent_burdened_pct2023" = "Rent-burdened households represents the share of renter households with incomes less than $35,000 that spend 30% or more of their income on rent. Low-income households that spend a high share of their income on housing costs have limited residual income to spend on other household expenses, much less save for emergencies. These households are more vulnerable to setbacks to their household finances and to more wide scale economic shocks. Hover over a county to see that county's rate and the state average rate.",
    "mortgage_burdened_pct2023" = "Mortgage burden rate (%) indicates the share of owner-occupied households with incomes less than $35,000 that spend 30% or more of their income on mortgage payments. Higher percentages may show potential financial strain for low-income homeowners. Hover over a county to see that county's rate and the state average rate.",
    "med_gross_rent2023" = "Median gross rent conveys the midpoint amount that households pay in total for their contract rent, utilities, and fuel costs. Low-income households living in areas with higher median gross rent tend to have greater challenges with housing affordability. Hover over a county to see that county's madian value and the state median value.",
    "housing_balance" = "Affordable housing shortage (units) refers to the difference between the demand for rental units affordable to extremely low-income households (income < 30% of area median income) and the supply available. A positive number indicates a shortage of affordable housing units. Hover over a county to see that county's shortage and the state total shortage.",
    "resunitpermitsrate23" = "The residential permit rate represents the number of residential permits that were approved per 10,000 housing units for each county in 2023. 'Residential permits' specifically include approvals of new, privately-owned residential construction. A higher number indicates that a county is approving new housing unit permits at a quicker rate compared to a county with a lower number. Hover over a county to see that county's rate and the state average rate.")
    v <- input$variable
    desc <- description[v]
    return(desc)
  })
  
  #### scatter plot ####
  output$scatter <- renderPlotly({
    df <- dat() %>% as.data.frame() %>%
      mutate(rural = case_when(rural == 1 ~ "Rural",
                               rural == 0 ~ "Urban")) %>%
      mutate(rural = as.factor(rural))

x <- input$variable_scatter_x
y <- input$variable_scatter_y
prefix_x <- variable_prefix[x]
prefix_y <- variable_prefix[y]
alias_x <- variable_aliases[x]
alias_y <- variable_aliases[y]
suffix_x <- variable_suffix[x]
suffix_y <- variable_suffix[y]
type_x <- variable_type[x]
type_y <- variable_type[y]

# custom hover text
df$hover_text <- paste0(alias_x, ": ", prefix_x, format(df$variable_scatter_x, big.mark = ifelse(type_x == "year", "", ",")), suffix_x, "<br>",
                       alias_y, ": ", prefix_y, format(df$variable_scatter_y, big.mark = ifelse(type_y == "year", "", ",")), suffix_y, "<br>",
                       "Rural or Urban: ", df$rural, "<br>",
                       "County: ", df$county, "<br>")

scatterp <- ggplot(df, aes(x = variable_scatter_x, y = variable_scatter_y)) +
  geom_smooth(se = FALSE, colour = "gray", size = 0.5) +
  geom_point(stat = "identity", 
             aes(color = rural, text = hover_text), 
             size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#4e72aa", "#94bcda"), name = "") +
  scale_x_continuous(labels = scales::number_format(prefix = prefix_x, suffix = suffix_x, big.mark = ifelse(type_x == "year", "", ","))) + 
  scale_y_continuous(labels = scales::number_format(prefix = prefix_y, suffix = suffix_y, big.mark = ifelse(type_y == "year", "", ","))) + 
  labs(title = paste(alias_x, "as a function of", alias_y, sep = " "),
       x = alias_x, y = alias_y) + theme_minimal()

ggplotly(scatterp + theme(legend.position = c(0.6, 0.6)),
         tooltip = "text") %>%
  plotly::layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                 annotations = list(x = 0.5, y = -0.2, text = "Source: Pennsylvania Affordable Housing Dashboard, Housing Initiative at Penn, July 2025, https://housinginitiative.shinyapps.io/PHFA_Housing_Dashboard/. ",
                                    xref='paper', yref='paper', showarrow = F,
                                    xanchor='center', yanchor='bottom', xshift=0, yshift=0,
                                    font = list(size = 12, color = "gray")),
                 legend = list(
                   orientation = "h",
                   x = "0.5",
                   xanchor = "center",
                   y = -.25))
})

  #### Data table ####
  output$table <- DT::renderDataTable({
    v <- input$variable_tab
    alias <- variable_aliases[v]
    
    dat.tab <- dat() %>% 
      dplyr::select(county, variable_tab, rural) %>%
      as.data.frame() 
    
    names(dat.tab) <- c("county", alias, "rural")
    
    DT::datatable(as.data.frame(dat.tab), options = list(pageLength = 10))
  })
  
  #####  data table text #####
  output$tableheader <- renderText({

    v <- input$variable_tab
    alias <- variable_aliases[v]
    return(paste(alias, "rankings", sep = " "))
  })
  
  output$sum <- renderTable({
    data.frame(minimum = min(dat()$variable_tab, na.rm = TRUE),
               percentile_25th = quantile(dat()$variable_tab, probs = 0.25, na.rm = TRUE),
               mean = mean(dat()$variable_tab, na.rm = TRUE),
               median = median(dat()$variable_tab, na.rm = TRUE),
               percentile_75th = quantile(dat()$variable_tab, probs = 0.75, na.rm = TRUE),
               maximum = max(dat()$variable_tab, na.rm = TRUE)) 
    
    
  })

  #### Leaflet prep ####
  
  ##### title #####
  title_dat <- tags$div(
    HTML("<strong>Indicators by County</strong><br/>
        Hover to see individual counties"))
  
  ##### labels #####
  labs_dat <- reactive({
    
    v <- input$variable
    prefix <- variable_prefix[v]
    suffix <- variable_suffix[v]
    alias <- variable_aliases[v]
    type <- variable_type[v]
    
    # Format the county value
    val_county <- dat.sf()$variable
    formatted_val_county <- format(val_county, big.mark = ifelse(type == "year", "", ","), scientific = FALSE)
    formatted_val_county <- gsub("\\.0+$", "", formatted_val_county)
    
    # Format the state average value
    val_state <- round(as.numeric(pa_avg()$variable))
    formatted_val_state <- format(val_state, big.mark = ifelse(type == "year", "", ","), scientific = FALSE)
    formatted_val_state <- gsub("\\.0+$", "", formatted_val_state)
    
    sprintf(
      "<strong>%s County</strong><br/>
      <strong>%s</strong>: %s<sup></sup><br/>
      <strong>Pennsylvania:</strong> %s",
      dat.sf()$county, alias, paste(prefix, formatted_val_county, ifelse(!is.na(val_county), suffix, ""), sep = ""), paste(prefix, formatted_val_state, suffix, sep = "")
    ) %>% lapply(htmltools::HTML)
  })

  # Custom function to format numbers with commas for thousands
  formatWithComma <- function(value) {
    # Ensure the value is a numeric
    value_num <- as.numeric(value)
    
    # Check if the value is 1000 or larger
    if (value_num >= 1000) {
      # Format with comma
      return(format(value_num, big.mark = ",", scientific = FALSE))
    } else {
      # If less than 1000, return the value as is
      return(value)
    }
  }
  output$leaflet <- renderLeaflet({

    v <- input$variable
    alias <- variable_aliases[v]
    prefix <- variable_prefix[v]
    suffix <- variable_suffix[v]
    var_map <- dat.sf()$variable
    variableType <- variable_type[v]
    
    ####format legend labels ####
    # Define the formatting function
    formatValue <- function(value, type) {
      if (type == "currency") {
        return(scales::dollar(value))
      } else if (type == "percent") {
        return(scales::percent(value))
      } else {
        # Fallback to a default formatting if the type is not recognized
        return(format(value))
      }
    }

    # legend labels ## suspect these are not used and can be deleted ##
    labels_map <- c(
      as.character(round(quantile(var_map, probs = c(0.2), na.rm = TRUE) ,0)),
      as.character(round(quantile(var_map, probs = c(0.4), na.rm = TRUE) ,0)),
      as.character(round(quantile(var_map, probs = c(0.6), na.rm = TRUE) ,0)),
      as.character(round(quantile(var_map, probs = c(0.8), na.rm = TRUE) ,0)),
      as.character(round(quantile(var_map, probs = c(1), na.rm = TRUE) ,0)))
    
    #### Leaflet map ####
    leaflet(options = leafletOptions(minZoom = 7)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView( lng = -77.83069
               , lat = 40.94503
               , zoom = 8 ) %>%
      setMaxBounds(lng1 = -82
                    , lat1 = 38.5
                    , lng2 = -73
                    , lat2 = 43.3) %>%
      addPolygons(data = dat.sf(), fillColor = ~ mapPalette()(dat.sf()$variable),
                  color = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  dashArray = "3",
                  highlightOptions = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE),
                  label = labs_dat(),
                  group = "counties") %>%
      addControl(title_dat, position = "bottomright") %>%
      addLegend(group = "counties",
                pal = mapPalette(),
                title = paste(as.character(alias), "<br>(Quintile breaks)", sep = ""),
                opacity = 1,
                labFormat = function(type, cuts, p) {
                  labels <- vector("character", length(cuts) - 1)
                  for (i in 1:(length(cuts) - 1)) {
                    # Apply the prefix and suffix to each bound + format numbers with commas
                    lower_bound <- paste0(prefix, formatC(cuts[i], format = "f", big.mark = ifelse(variableType == "year", "", ","), digits = 0), suffix)
                    upper_bound <- paste0(prefix, formatC(cuts[i + 1], format = "f", big.mark = ifelse(variableType == "year", "", ","), digits = 0), suffix)
                    # Construct the label for each bin
                    labels[i] <- paste(lower_bound, "-", upper_bound)
                  }
                 return(labels)},
                values = quantile(dat.sf()$variable, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
                position = "bottomright") %>%
      addLabelOnlyMarkers(data = dat.sf(), ~dat.sf()$lon, ~dat.sf()$lat, 
                          label = ~as.character(dat.sf()$county),
                          labelOptions = labelOptions(
                            noHide = TRUE, 
                            direction = 'center', 
                            textOnly = TRUE, 
                            style = list(
                              "color" = "white",
                              "font-family" = "sans-serif",
                              "font-size" = "14px",
                              "text-shadow" = "-0.25px -0.25px 0 #607d8b, 0.25px -0.25px 0 #607d8b, -0.25px 0.25px 0 #607d8b, 0.25px 0.25px 0 #607d8b" # Dark gray outline
                              )),
                          group = "County Names") %>%
      addPolygons(data = rural,
        color = "orchid",
        weight = 5,
        opacity = 1,
        group  = "Rural Counties") %>%
      addLegend(group = "Rural Counties",
                colors = "orchid",
                labels = "Rural Counties - as defined by ACS<br/>See GitHub repository for more detail",
                position = "bottomright") %>%
      addLayersControl(position = "bottomright",
        overlayGroups = c("Rural Counties", "County Names"), 
        options = layersControlOptions(collapsed = F))%>%
      groupOptions("County Names", zoomLevels = 8:100) %>%
      groupOptions("Rural Counties", zoomLevels = FALSE)
  })

  
  #### data download ####
  output$downloadDataSel <- downloadHandler(
    filename = function() {
      paste(input$variable_tab, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataAll <- downloadHandler(
    filename = function() {
      "phfa_dash_2023.csv"
    },
    content = function(file) {
      write.csv(panel, file, row.names = FALSE)
    }
  )
}

## runGitHub("housinginitiative/PHFA-Housing-Dash", subdir = "dashboard files")
