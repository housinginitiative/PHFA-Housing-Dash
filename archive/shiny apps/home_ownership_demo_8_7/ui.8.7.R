
ui <- fluidPage(
  titlePanel(title = "Home Ownership Rate in Pennsylvania Counties, 2021", windowTitle = "Idk"),
sidebarLayout(
  sidebarPanel(
    selectInput("variable", "County",
                choices = c("Philadelphia" = "Philadelphia County, Pennsylvania",
                            "Delaware" = "Delaware County, Pennsylvania"),
    selected = "Philadelphia"
    ),
    shiny::p("Use this web app to check the home ownership rate in your county as of 2021. This map visualizes Pennsylvania counties' home ownership rate by chloropleth, and you can hover over specific counties for more detailed information."),
    checkboxGroupInput("dataLayers", "Select Indicator",
                       choices = c(
                         "Home Ownership Rate (%)" = "healthCenters",
                         "Rentership Rate (%)" = "stores",
                         "Gross Rent" = "schools",
                         "Home Values" = "homeVal"),
                       selected = character(0))),
  mainPanel(
    fluidRow(
      column(
        width = 12,
        div(id = "map-container", leafletOutput(outputId = "leaflet")))
    )
  )
)
)
