#### Set up #### 
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)
library(mapview)

mapTheme <- function(base_size = 15) {   
  theme(     text = element_text(color = "black", family="Helvetica"),     
             plot.title = element_text(size = 20,colour = "black", hjust=0, face="bold"),     
             plot.subtitle=element_text(face="italic", hjust = 0),     
             plot.caption=element_text(size = 7, hjust = 0),     
             axis.ticks = element_blank(),     
             axis.text = element_blank(),
             axis.title = element_blank(),     
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),     
             panel.border = element_rect(colour = "white", fill=NA, size=2),
             panel.background = element_rect(fill = "grey90", size = 0, colour = NA),     
             legend.background = element_blank(),     
             legend.position="right"   ) }

#### Pull data ####
#### census panels
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)
census_vars <- load_variables(year = 2021, dataset = "acs5")

# owner occupied: B25011_002
# all HH: B11007_001

dat_21 <- get_acs(geography = "county", 
                  variables = c("owner_occ_hh" = "B25011_002", 
                                "total_hh" = "B11007_001",
                                "renter_occ_hh" = "B25003_003",
                                "income_below10k" = "B19101_002",
                                "income_10k_15k" = "B19101_003",
                                "income_15k_20k" = "B19101_004",
                                "income_20k_25k" = "B19101_005",
                                "income_25k_30k" = "B19101_006",
                                "income_30k_35k" = "B19101_007",
                                "income_35k_40k" = "B19101_008",
                                "income_40k_45k" = "B19101_009",
                                "income_45k_50k" = "B19101_010",
                                "income_50k_60k" = "B19101_011",
                                "income_60k_75k" = "B19101_012",
                                "income_75k_100k" = "B19101_013",
                                "income_100k_125k" = "B19101_014",
                                "income_125k_150k" = "B19101_015",
                                "income_150k_200k" = "B19101_016",
                                "income_200k_plus" = "B19101_017",
                                "vacant_rental_units" = "B25004_002",
                                "internet_hh" = "B28002_002",
                                "med_year_built" = "B25035_001",
                                "med_home_value" = "B25107_001",
                                "medhhinc" = "B19013A_001",
                                "med_owner_costs_mortgaged" = "B25088_002",
                                "med_gross_rent" = "B25064_001"), year = 2021, state = "PA",
                  geometry = TRUE, survey = "acs5", output = "wide") %>%
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         age_home = 2023-med_year_builtE,
         internet_hh = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         rent_burdened_pct = ifelse(med_gross_rentE > 5000*.3, income_below10kE,
                                    ifelse(med_gross_rentE > 12500*.3, income_below10kE + income_10k_15kE,
                                           ifelse(med_gross_rentE > 17500*.3, income_below10kE + income_10k_15kE + income_15k_20kE,
                                                  ifelse(med_gross_rentE > 22500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE,
                                                         ifelse(med_gross_rentE > 27500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE,
                                                                ifelse(med_gross_rentE > 32500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE,
                                                                       ifelse(med_gross_rentE > 37500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE,
                                                                              ifelse(med_gross_rentE > 42500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE,
                                                                                     ifelse(med_gross_rentE > 47500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE,
                                                                                            ifelse(med_gross_rentE > 55000*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE,
                                                                                                   ifelse(med_gross_rentE > 67500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE,
                                                                                                          ifelse(med_gross_rentE > 87500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE,
                                                                                                                 ifelse(med_gross_rentE > 112,500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE,
                                                                                                                        ifelse(med_gross_rentE > 137,500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE,
                                                                                                                               ifelse(med_gross_rentE > 175,000*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE,
                                                                                                                                      ifelse(med_gross_rentE > 162,500*0.3, income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE,
                                                                                                                                             0)))))))))))))))),
         
         NAME = word(NAME, 1)) %>%
  rename(owner_occ_hh_21 = owner_occ_hhE,
         owner_occ_hh_pct_21 = owner_occ_hh_pct,
         total_hh_21 = total_hhE) %>%
  dplyr::select(NAME, owner_occ_hh_pct, renter_occ_hh_pct, age_home, internet_hh, rent_burdened_pct, med_gross_rentE, geometry)
  

dat <- dat_21 %>%
  left_join(dat_20, by = "NAME") %>%
  left_join(dat_19, by = "NAME") %>%
  left_join(dat_18, by = "NAME") %>%
  left_join(dat_17, by = "NAME")

#### Census rural-urban by county 
county <- st_read("/Users/annaduan/Library/CloudStorage/Box-Box/PHFA\ dashboard/data\ panels/2020_UA_COUNTY.xlsx") %>% 
  filter(Field1 == "42") %>%
  mutate(rural = ifelse(as.numeric(Field22)/as.numeric(Field5) > 0.5, 1, 0)) %>%
  dplyr::select(Field4, rural) %>%
  rename(NAME = Field4)

#### Write panel #### 
dat <- dat %>%
  left_join(county, by = "NAME")

st_write(dat, "homeownership_17_21.geojson")

#### Leaflet test run ####
dat <- st_read("/Users/annaduan/Library/CloudStorage/Box-Box/PHFA\ dashboard/data\ panels/homeownership_17_21.geojson") %>%
  st_as_sf()


pal <- colorFactor("YlGnBu", dat$owner_occ_hh_pct_21)


labs_dat <- sprintf(
  "<strong>%s</strong><br/>
  Home Ownership Rate $%s<sup></sup>",
  dat$NAME, dat$owner_occ_hh_pct_21
) %>% lapply(htmltools::HTML)

# 
# title <- tags$div(
#   HTML("<strong>2021 ACS Estimates</strong><br/>
#         Hover to see individual counties"))  


leaflet(dat) %>%
  addPolygons(fillColor = ~pal(owner_occ_hh_pct_21),
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
                bringToFront = TRUE)
              # ,
              #  label = labs_dat,
              #  labelOptions = labelOptions(
              #    style = list("font-weight" = "normal", padding = "3px 8px"),
              #    textsize = "15px",
              #    direction = "auto")
)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
 # addControl(title, position = "topright") %>%
  addLegend(pal = pal, values = ~owner_occ_hh_pct_21, opacity = 0.7, title = "Home Ownership Rate<br/>by County (%)",
            position = "bottomright")
  