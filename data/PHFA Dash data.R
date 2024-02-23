#### Set up #### 
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)
library(mapview)
library(stringr)
library(tigris)

#### Pull data ####
#### census panels
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)
census_vars <- load_variables(year = 2022, dataset = "acs5")

#### statewide avg - 2022 ####
dat_PA <- get_acs(geography = "state", 
                  variables = c("owner_occ_hh" = "B25011_002", 
                                "total_hh" = "B25011_001",
                                "renter_occ_hh" = "B25011_026",
                                "vacant_rental_units" = "B25004_002",
                                "internet_hh" = "B28002_002",
                                "med_year_built" = "B25035_001",
                                "med_home_value" = "B25107_001",
                                "medhhinc" = "B19013A_001",
                                "med_owner_costs_mortgaged" = "B25088_002",
                                "med_gross_rent" = "B25064_001",
                                
                                # rent burden
                                "inc_less10k_rent30_35" = "B25074_006", #less 10k 30-35%
                                "inc_less10k_rent35_40" = "B25074_007",  # less 10k 35-40%
                                "inc_less10k_rent40_50" = "B25074_008", #less 10k 40-50%
                                "inc_less10k_rent50plus" = "B25074_009", #Less 10k 50%+
                                
                                "inc_10_20k_rent30_35" = "B25074_015", #10-20k 30-35%
                                "inc_10_20k_rent35_40" = "B25074_016", #10-20k 35-40%
                                "inc_10_20k_rent40_50" = "B25074_017", #10-20k 40-50%
                                "inc_10_20k_rent50plus" = "B25074_018", #10-20k 50%+
                                
                                "inc_20_35k_rent30_35" = "B25074_024", #20-35k 30-35%
                                "inc_20_35k_rent35_40" = "B25074_025", #20-35k 35-40%
                                "inc_20_35k_rent40_50" = "B25074_026", #20-35k 40-50%
                                "inc_20_35k_rent50plus" = "B25074_027",
                                
                                # mortgage burden
                                "inc_bel20k_mort30plus" = "B25101_006",
                                "inc_20_35k_mort30plus" = "B25101_010",
                                
                                # owner low income
                                "owner_inc_bel20k" = "B25101_003", #owner income bel 20k
                                "owner_inc_20k_35k" = "B25101_007",
                                
                                # renter low income
                                "renter_inc_bel10k" = "B25074_002",
                                "renter_inc_10k_20k" = "B25074_011",
                                "renter_inc_20k_35k" = "B25074_020"
                                
                  ), year = 2022, state = "PA", #20-35k 50%+
                  geometry = FALSE, survey = "acs5", output = "wide") %>%
  
          # ownership rate
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         
         # rentership rate
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         
         # vacant rentals pct
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         
         # median home age
         med_age_home = 2024-med_year_builtE,
         
         # hh with internet %
         internet_hh_pct = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         
         # renter low inc
         renter_low_inc = renter_inc_bel10kE + renter_inc_10k_20kE + renter_inc_20k_35kE,
         
         # owner low inc
         owner_low_inc = owner_inc_bel20kE + owner_inc_20k_35kE,
         
         # rent burdened hh
         income_bel35k_rent_more30 = inc_less10k_rent30_35E + inc_less10k_rent35_40E + inc_less10k_rent40_50E + inc_less10k_rent50plusE +
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE + 
           inc_20_35k_rent30_35E + inc_20_35k_rent35_40E + inc_20_35k_rent40_50E + inc_20_35k_rent50plusE,
         
         # rent burdened pct
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100*(income_bel35k_rent_more30/renter_low_inc)), 0),
         
         # mortgage burdened hh
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         
         # mortgage burdened pct
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100*(income_bel35k_mort_more30/owner_low_inc)), 0),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, internet_hh_pct, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat_PA), 2022)

# Assign the modified column names back to the data frame
names(dat_PA) <- new_column_names



#### 2022 ####
dat22 <- get_acs(geography = "county", 
                 variables = c("owner_occ_hh" = "B25011_002", 
                               "total_hh" = "B25011_001",
                               "renter_occ_hh" = "B25011_026",
                               "vacant_rental_units" = "B25004_002",
                               "internet_hh" = "B28002_002",
                               "med_year_built" = "B25035_001",
                               "med_home_value" = "B25107_001",
                               "medhhinc" = "B19013A_001",
                               "med_owner_costs_mortgaged" = "B25088_002",
                               "med_gross_rent" = "B25064_001",
                               
                               # rent burden
                               "inc_less10k_rent30_35" = "B25074_006", #less 10k 30-35%
                               "inc_less10k_rent35_40" = "B25074_007",  # less 10k 35-40%
                               "inc_less10k_rent40_50" = "B25074_008", #less 10k 40-50%
                               "inc_less10k_rent50plus" = "B25074_009", #Less 10k 50%+
                               
                               "inc_10_20k_rent30_35" = "B25074_015", #10-20k 30-35%
                               "inc_10_20k_rent35_40" = "B25074_016", #10-20k 35-40%
                               "inc_10_20k_rent40_50" = "B25074_017", #10-20k 40-50%
                               "inc_10_20k_rent50plus" = "B25074_018", #10-20k 50%+
                               
                               "inc_20_35k_rent30_35" = "B25074_024", #20-35k 30-35%
                               "inc_20_35k_rent35_40" = "B25074_025", #20-35k 35-40%
                               "inc_20_35k_rent40_50" = "B25074_026", #20-35k 40-50%
                               "inc_20_35k_rent50plus" = "B25074_027",
                               
                               # mortgage burden
                               "inc_bel20k_mort30plus" = "B25101_006",
                               "inc_20_35k_mort30plus" = "B25101_010",
                               
                               # owner low income
                               "owner_inc_bel20k" = "B25101_003", #owner income bel 20k
                               "owner_inc_20k_35k" = "B25101_007",
                               
                               # renter low income
                               "renter_inc_bel10k" = "B25074_002",
                               "renter_inc_10k_20k" = "B25074_011",
                               "renter_inc_20k_35k" = "B25074_020"
                               
                 ), year = 2022, state = "PA", #20-35k 50%+
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  
  # ownership rate
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         
         # rentership rate
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         
         # vacant rentals pct
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         
         # median home age
         med_age_home = 2024-med_year_builtE,
         
         # hh with internet %
         internet_hh_pct = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         
         # renter low inc
         renter_low_inc = renter_inc_bel10kE + renter_inc_10k_20kE + renter_inc_20k_35kE,
         
         # owner low inc
         owner_low_inc = owner_inc_bel20kE + owner_inc_20k_35kE,
         
         # rent burdened hh
         income_bel35k_rent_more30 = inc_less10k_rent30_35E + inc_less10k_rent35_40E + inc_less10k_rent40_50E + inc_less10k_rent50plusE +
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE + 
           inc_20_35k_rent30_35E + inc_20_35k_rent35_40E + inc_20_35k_rent40_50E + inc_20_35k_rent50plusE,
         
         # rent burdened pct
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100*(income_bel35k_rent_more30/renter_low_inc)), 0),
         
         # mortgage burdened hh
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         
         # mortgage burdened pct
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100*(income_bel35k_mort_more30/owner_low_inc)), 0),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(county, owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, internet_hh_pct, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat22), 2022)

# Assign the modified column names back to the data frame
names(dat22) <- new_column_names

dat <- dat22  %>%
  rename(county = county2022) 

#### CHAS Data 
chas <- st_read("/Users/annaduan/Desktop/GitHub/PHFA-Housing-Dash/data/PACounty_2015-2019.xlsx") %>%
  mutate(Geography = word(Geography, end = 1))
names(chas) <- c("county", "renter_hh", "afford_avail_units", "housing_balance")




#### correct PA housing balance
tab8 <- read.csv("/Users/annaduan/Desktop/GitHub/PHFA-Housing-Dash/data/chas_pa_2016-2020/2016thru2020-040-csv/Table8.csv") %>%
  dplyr::select(T8_est69, name) %>% #occupied by low-inc hh
  filter(name == "Pennsylvania")

tab14b <- read.csv("/Users/annaduan/Desktop/GitHub/PHFA-Housing-Dash/data/chas_pa_2016-2020/2016thru2020-040-csv/Table14B.csv") %>%
  dplyr::select(T14B_est4, name) %>% #affordable and vacant for rent
  filter(name == "Pennsylvania")

tab15c <- read.csv("/Users/annaduan/Desktop/GitHub/PHFA-Housing-Dash/data/chas_pa_2016-2020/2016thru2020-040-csv/Table15C.csv") %>%
  dplyr::select(T15C_est4, #affordable + occupied + below 30% RHUD30
                T15C_est5, name) %>% #affordable and occupied by low-inc hh
  filter(name == "Pennsylvania")

housing_balance = tab14b$T14B_est4 + tab15c$T15C_est5 - tab8$T8_est69 - tab15c$T15C_est4


chas_pa <- chas %>%
  mutate(weight = renter_hh/10000,
         afford_avail_units_weighted = round(afford_avail_units*weight),
         housing_balance_weighted = round(housing_balance*weight)) %>%
  summarize(afford_avail_units = sum(afford_avail_units_weighted)/sum(weight),
            housing_balance = -267074) %>%
  mutate(county = "statewide_avg",
         renter_hh = 434595)

#### Census rural-urban by county 
rural <- st_read("/Users/annaduan/Desktop/GitHub/PHFA-Housing-Dash/data/2020_UA_COUNTY.xlsx") %>% 
  dplyr::filter(Field1 == "42") %>%
  mutate(rural = ifelse(as.numeric(Field22)/as.numeric(Field5) > 0.5, 1, 0)) %>%
  dplyr::select(Field4, rural) %>%
  rename(county = Field4)

#### County shapefiles
pa_counties <- counties(state = "PA") %>%
  dplyr::select("county" = "NAME") %>%
  st_transform("WGS84")

erie <- pa_counties %>%
  filter(county == "Erie") %>%
  erase_water(area_threshold = 0.999)

noterie <- pa_counties %>%
  filter(county != "Erie")

counties.sf <- rbind(erie, noterie) %>%
  st_cast("POLYGON")

#### Write panel #### 
dat <- dat %>%
  left_join(chas, by = "county") %>%
  left_join(rural, by = "county") %>%
  left_join(counties.sf, by = "county") %>%
  st_as_sf() %>%
  st_make_valid()
  
# panel
panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf()

st_write(panel.sf, "PHFA_dash_data_Feb.23.geojson", driver="GeoJSON")


state_avg = cbind(chas_pa, dat_PA)

st_write(state_avg, "state_avg_01-23.csv", driver = "CSV")



#####
leaflet(counties.sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white", color = "black", weight = 1, opacity = 1, fillOpacity = 0.7)
