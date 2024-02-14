# Pennsylvania Housing Finance Agency Dashboard

![Dashboard landing page](https://github.com/housinginitiative/PHFA-Housing-Dash/blob/80ccdf7ead5fe87b68a48cea421b6050f63509e9/dashboard%20files/www/dashboard%20screenshot.png)

## Overview

This [dashboard](https://housinginitiative.shinyapps.io/PHFA_Housing_Dashboard/) is a collaboration between the Pennsylvania Housing Finance Agency and the Housing Initiative at Penn, designed to visualize housing affordability trends across Pennsylvania. It showcases current housing trends in counties statewide, based on data from the U.S. Census Bureau's 5-year American Community Survey and the U.S. Department of Housing and Urban Development's Comprehensive Housing Affordability Strategy.

## Data Sources and Methods

The dashboard utilizes the American Community Survey's 2022 5-year estimates and the HUD’s Comprehensive Housing Affordability Strategy dataset (2019) for the Affordable Housing Shortage variable. Data was accessed through the US Census Bureau API using the `tidycensus` package, with processing and preparation done via `dplyr`, `tigris`, `sf`, `stringr`, and `tidyr`. Visualizations are created using `leaflet`, `plotly`, `ggplot2`, and `pander` in the R-language with Posit's R-Shiny app interface.

### Indicators

-   **Homeownership rate (%) 2022**
    -   **Description:** The percentage of households that own their homes. A higher rate indicates a greater proportion of homeowners in the area.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Rentership rate (%) 2022**
    -   **Description:** The share of households in a county that rent their homes. Younger households and households with limited incomes are more likely to rent than older households and households with higher incomes.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Vacant rental units (%) 2022**
    -   **Description:** The percentage of rental units that are currently unoccupied. A higher percentage can suggest that there's a surplus of rental housing or potentially decreased demand.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Median age of home (years) 2022**
    -   **Description:** The midpoint age of homes in a specific area. Older median ages can suggest historical or older neighborhoods, while lower values might indicate newer developments.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Median home value (\$) 2022**
    -   **Description:** The midpoint value of homes in the area. This can provide an insight into the overall affordability and property values of a region.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Households with internet access (%) 2022**
    -   **Description:** The percentage of households that have access to the internet. This can provide insights into the area's technological infrastructure and development.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Rent-burdened households (%) 2022**
    -   **Description:** The share of renter households with incomes less than \$35,000 that spend 30% or more of their income on rent. These households are more vulnerable to financial setbacks and economic shocks.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Mortgage burdened households (%) 2022**
    -   **Description:** Low-income households spending 30% or more of their income on mortgage payments. Higher percentages may show potential financial strain for low-income homeowners.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Median gross rent (\$) 2022**
    -   **Description:** The midpoint amount that households pay in total for their contract rent, utilities, and fuel costs. Higher median gross rent can indicate greater challenges with housing affordability for low-income households.
    -   **Source:** 2022 American Community Survey 5-Year Estimates, U.S. Census Bureau
-   **Affordable housing shortage (units)**
    -   **Description:** The difference between the demand for rental units affordable to extremely low-income households (income \< 30% of area median income) and the supply available. A positive number indicates a shortage of affordable housing units.
    -   **Source:** Comprehensive Housing Affordability Strategy (CHAS) dataset, U.S. Department of Housing and Urban Development

## Features

-   Interactive map visualizations with `leaflet`
-   Dynamic data visualization using `plotly`
-   Easy csv export of filtered data
-   Customizable filters for cross-county comparisons

## Libraries Used

-   R Shiny, `leaflet`, `plotly`, `ggplot2`, `pander` for development and visualization
-   `tidycensus`, `dplyr`, `tigris`, `sf`, `stringr`, `tidyr` for data processing

## Installation

To run this dashboard locally, you need to have R and RStudio installed on your machine. Follow these steps:

1.  Install R from [CRAN](https://cran.r-project.org/)
2.  Install RStudio from [RStudio's website](https://www.rstudio.com/products/rstudio/download/)
3.  Open RStudio and install the required packages using the following command: `R     install.packages(c("shiny", "rsconnect", "conflicted", "tidyr", "shinythemes", "bslib", "leaflet", "shinyWidgets", "plotly", "sf", "stringr", "tidyverse", "mapview"))`
4.  Clone this repository or download the `ui.R` and `server.R` files to your local machine.
5.  Open either `ui.R` or `server.R` in RStudio, and run the application by clicking 'Run App'.

## Usage

After launching the dashboard, you can:\
- Use the map to explore housing trends across different regions in Pennsylvania.\
- Apply filters to narrow down the data by specific criteria such as time period, housing type, etc.\
- Interact with the visualizations to gain detailed insights into housing affordability.

## About the Collaborators

### Pennsylvania Housing Finance Agency

The Pennsylvania Housing Finance Agency is dedicated to providing affordable homeownership and rental apartment options for older adults, low- and moderate-income families, and people with special housing needs. It promotes economic development across the Commonwealth through its mortgage programs, investments in multifamily housing developments, and community development projects funding.

### Housing Initiative at Penn

Based in the University of Pennsylvania's Weitzman School of Design, the Housing Initiative at Penn is a housing policy research initiative. It conducts rigorous academic and empirical research to advance evidence-based policymaking and analyzes current conditions and future trends in housing, aiming for more effective and equitable housing policy at various levels.


## Contact

For questions about this dashboard, please contact:

**Anna Duan**, Housing Research Analyst, at [annaduan\@sas.upenn.edu](mailto:annaduan@sas.upenn.edu)

**Amy Sechrist**, Senior Policy Officer, at [asechrist\@phfa.org](mailto:asechrist@phfa.org)


### Last Update  
January 2024
