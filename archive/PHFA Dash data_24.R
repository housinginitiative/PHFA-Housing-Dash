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
census_vars <- load_variables(year = 2023, dataset = "acs5")

#### statewide avg - 2023 ####
dat_PA <- get_acs(geography = "state", 
                  variables = c("total_hh" = "B25011_001",
                                "owner_occ_hh" = "B25011_002", 
                                "total_white_hh" = "B25003A_001",
                                "white_own_occ_hh" = "B25003A_002",
                                "total_black_hh" = "B25003B_001",
                                "black_own_occ_hh" = "B25003B_002",
                                "total_hisp_lat_hh" = "B25003I_001",
                                "hisp_lat_own_occ_hh" = "B25003I_002",
                                "renter_occ_hh" = "B25011_026",
                                "vacant_rental_units" = "B25004_002",
                                "internet_hh" = "B28002_002",
                                "med_year_built" = "B25035_001",
                                "med_home_value" = "B25107_001",
                                "medhhinc" = "B19013A_001",
                                "med_owner_costs_mortgaged" = "B25088_002",
                                "med_gross_rent" = "B25064_001",
                                
                                # rent burden
                                "rent_inc_less10k_20less" = "B25074_003", #less 10k under 20%
                                "rent_inc_less10k_20_25" = "B25074_004", #less 10k 20-25%
                                "rent_inc_less10k_25_30" = "B25074_005", #less 10k 25-30%
                                "rent_inc_less10k_30_35" = "B25074_006", #less 10k 30-35%
                                "rent_inc_less10k_35_40" = "B25074_007",  # less 10k 35-40%
                                "rent_inc_less10k_40_50" = "B25074_008", #less 10k 40-50%
                                "rent_inc_less10k_50plus" = "B25074_009", #Less 10k 50%+
                                
                                "rent_inc_10_20k_20less" = "B25074_012", #10-20k under 20%
                                "rent_inc_10_20k_20_25" = "B25074_013", #10-20k 20-25%
                                "rent_inc_10_20k_25_30" = "B25074_014", #10-20k 25-30%
                                "rent_inc_10_20k_30_35" = "B25074_015", #10-20k 30-35%
                                "rent_inc_10_20k_35_40" = "B25074_016", #10-20k 35-40%
                                "rent_inc_10_20k_40_50" = "B25074_017", #10-20k 40-50%
                                "rent_inc_10_20k_50plus" = "B25074_018", #10-20k 50%+
                                
                                "rent_inc_20_35k_20less" = "B25074_021", #20-35k under 20%
                                "rent_inc_20_35k_20_25" = "B25074_022", #20-35k 20-25%
                                "rent_inc_20_35k_25_30" = "B25074_023", #20-35k 25-30%
                                "rent_inc_20_35k_30_35" = "B25074_024", #20-35k 30-35%
                                "rent_inc_20_35k_35_40" = "B25074_025", #20-35k 35-40%
                                "rent_inc_20_35k_40_50" = "B25074_026", #20-35k 40-50%
                                "rent_inc_20_35k_50plus" = "B25074_027", #20-35k 50%+
                                
                                # mortgage burden
                                "inc_bel20k_mort30plus" = "B25101_006",
                                "inc_20_35k_mort30plus" = "B25101_010",
                                
                                # owner low income
                                "owner_inc_bel20k" = "B25101_003", #owner income bel 20k
                                "owner_inc_20k_35k" = "B25101_007"
                                
                  ), year = 2023, state = "PA", #20-35k 50%+
                  geometry = FALSE, survey = "acs5", output = "wide") %>%
  
          # ownership rate
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100 * owner_occ_hhE / total_hhE), 0),
         white_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * white_own_occ_hhE / total_white_hhE), 0),
         black_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * black_own_occ_hhE / total_black_hhE), 0),
         hisp_lat_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * hisp_lat_own_occ_hhE / total_hisp_lat_hhE), 0),
         
         # rentership rate
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100 * renter_occ_hhE / total_hhE), 0),
         
         # vacant rentals pct
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100 * vacant_rental_unitsE / renter_occ_hhE), 0),
         
         # median home age
         med_age_home = 2025 - med_year_builtE,
         
         # hh with internet %
         internet_hh_pct = ifelse(total_hhE > 0, round(100 * internet_hhE / total_hhE), 0),
         
         # renter low inc
         renter_low_inc = sum(across(starts_with("rent_inc"))),
         
         # owner low inc
         owner_low_inc = owner_inc_bel20kE + owner_inc_20k_35kE,
         
         # rent burdened hh
         income_bel35k_rent_more30 = sum(across(ends_with(c("_30_35E", "_35_40E", "_40_50E", "_50plusE")))),
         
         # rent burdened pct
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100 * (income_bel35k_rent_more30 / renter_low_inc)), 0),
         
         # mortgage burdened hh
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         
         # mortgage burdened pct
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100 * (income_bel35k_mort_more30 / owner_low_inc)), 0),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(owner_occ_hh_pct, 
                white_own_occ_hh_pct,
                black_own_occ_hh_pct,
                hisp_lat_own_occ_hh_pct,
                renter_occ_hh_pct,
                renter_vacant_pct,
                med_age_home,
                med_home_value,
                internet_hh_pct,
                rent_burdened_pct,
                mortgage_burdened_pct,
                med_gross_rent)

new_column_names <- paste0(names(dat_PA), 2023)

# Assign the modified column names back to the data frame
names(dat_PA) <- new_column_names

#### county data - 2023 ####
dat23 <- get_acs(geography = "county", 
                 variables = c("total_hh" = "B25011_001",
                               "owner_occ_hh" = "B25011_002", 
                               "total_white_hh" = "B25003A_001",
                               "white_own_occ_hh" = "B25003A_002",
                               "total_black_hh" = "B25003B_001",
                               "black_own_occ_hh" = "B25003B_002",
                               "total_hisp_lat_hh" = "B25003I_001",
                               "hisp_lat_own_occ_hh" = "B25003I_002",
                               "renter_occ_hh" = "B25011_026",
                               "vacant_rental_units" = "B25004_002",
                               "internet_hh" = "B28002_002",
                               "med_year_built" = "B25035_001",
                               "med_home_value" = "B25107_001",
                               "medhhinc" = "B19013A_001",
                               "med_owner_costs_mortgaged" = "B25088_002",
                               "med_gross_rent" = "B25064_001",
                               
                               # rent burden
                               "rent_inc_less10k_20less" = "B25074_003", #less 10k under 20%
                               "rent_inc_less10k_20_25" = "B25074_004", #less 10k 20-25%
                               "rent_inc_less10k_25_30" = "B25074_005", #less 10k 25-30%
                               "rent_inc_less10k_30_35" = "B25074_006", #less 10k 30-35%
                               "rent_inc_less10k_35_40" = "B25074_007",  # less 10k 35-40%
                               "rent_inc_less10k_40_50" = "B25074_008", #less 10k 40-50%
                               "rent_inc_less10k_50plus" = "B25074_009", #Less 10k 50%+
                               
                               "rent_inc_10_20k_20less" = "B25074_012", #10-20k under 20%
                               "rent_inc_10_20k_20_25" = "B25074_013", #10-20k 20-25%
                               "rent_inc_10_20k_25_30" = "B25074_014", #10-20k 25-30%
                               "rent_inc_10_20k_30_35" = "B25074_015", #10-20k 30-35%
                               "rent_inc_10_20k_35_40" = "B25074_016", #10-20k 35-40%
                               "rent_inc_10_20k_40_50" = "B25074_017", #10-20k 40-50%
                               "rent_inc_10_20k_50plus" = "B25074_018", #10-20k 50%+
                               
                               "rent_inc_20_35k_20less" = "B25074_021", #20-35k under 20%
                               "rent_inc_20_35k_20_25" = "B25074_022", #20-35k 20-25%
                               "rent_inc_20_35k_25_30" = "B25074_023", #20-35k 25-30%
                               "rent_inc_20_35k_30_35" = "B25074_024", #20-35k 30-35%
                               "rent_inc_20_35k_35_40" = "B25074_025", #20-35k 35-40%
                               "rent_inc_20_35k_40_50" = "B25074_026", #20-35k 40-50%
                               "rent_inc_20_35k_50plus" = "B25074_027", #20-35k 50%+
                               
                               # mortgage burden
                               "inc_bel20k_mort30plus" = "B25101_006",
                               "inc_20_35k_mort30plus" = "B25101_010",
                               
                               # owner low income
                               "owner_inc_bel20k" = "B25101_003", #owner income bel 20k
                               "owner_inc_20k_35k" = "B25101_007"
                               
                 ), year = 2023, state = "PA", #20-35k 50%+
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  
  # ownership rate
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100 * owner_occ_hhE / total_hhE), 0),
         white_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * white_own_occ_hhE / total_white_hhE), 0),
         black_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * black_own_occ_hhE / total_black_hhE), 0),
         hisp_lat_own_occ_hh_pct = ifelse(total_hhE > 0, round(100 * hisp_lat_own_occ_hhE / total_hisp_lat_hhE), 0),
         
         # rentership rate
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100 * renter_occ_hhE / total_hhE), 0),
         
         # vacant rentals pct
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100 * vacant_rental_unitsE / renter_occ_hhE), 0),
         
         # median home age
         med_age_home = 2025 - med_year_builtE,
         
         # hh with internet %
         internet_hh_pct = ifelse(total_hhE > 0, round(100 * internet_hhE / total_hhE), 0),
         
         # owner low inc
         owner_low_inc = owner_inc_bel20kE + owner_inc_20k_35kE,
         
         # mortgage burdened hh
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         
         # mortgage burdened pct
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100 * (income_bel35k_mort_more30 / owner_low_inc)), 0),
         county = word(NAME, 1)) %>%
  rowwise() %>%
  mutate(renter_low_inc = sum(across(starts_with("rent_inc"))), # renter low inc
         
         income_bel35k_rent_more30 = sum(across(ends_with(c("_30_35E", "_35_40E", "_40_50E", "_50plusE")))), # rent burdened hh
         
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100 * (income_bel35k_rent_more30 / renter_low_inc)), 0)) %>% # rent burdened pct
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  
  #Fix NAs
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  
  dplyr::select(county,
                owner_occ_hh_pct,
                white_own_occ_hh_pct,
                black_own_occ_hh_pct,
                hisp_lat_own_occ_hh_pct,
                renter_occ_hh_pct,
                renter_vacant_pct,
                med_age_home,
                med_home_value,
                internet_hh_pct,
                rent_burdened_pct,
                mortgage_burdened_pct,
                med_gross_rent) 

new_column_names <- paste0(names(dat23), 2023)

# Assign the modified column names back to the data frame
names(dat23) <- new_column_names

dat <- dat23  %>%
  rename(county = county2023) 
                                                                                                                    
#### CHAS Data   
tab8 <- read.csv("/Users/jstaro/Documents/GitHub/PHFA-Housing-Dash/data/chas_pa_2017-2021/2017thru2021-050-csv/Table8.csv") %>%
  filter(st == 42) %>%
  dplyr::select(name, T8_est69) %>% # occupied by extremely low-income hh (less than or equal to 30% HAMFI)
  rename(renter_hh_eli = T8_est69)

tab14b <- read.csv("/Users/jstaro/Documents/GitHub/PHFA-Housing-Dash/data/chas_pa_2017-2021/2017thru2021-050-csv/Table14B.csv") %>%
  filter(st == 42) %>%
  dplyr::select(T14B_est4, T14B_est8, T14B_est12, name) %>%
  mutate(afford_avail_units_eli = T14B_est4) # affordable for hh below 30% RHUD30 and vacant

tab15c <- read.csv("/Users/jstaro/Documents/GitHub/PHFA-Housing-Dash/data/chas_pa_2017-2021/2017thru2021-050-csv/Table15C.csv") %>%
  filter(st == 42) %>%
  dplyr::select(T15C_est5, name) %>% # affordable for hh below 30% RHUD and occupied by eli hh
  rename(afford_occ_units_eli = T15C_est5)

chas <- left_join(tab8,
                  tab14b,
                  by = "name") %>%
  left_join(tab15c, by = "name") %>%
  mutate(housing_balance = afford_avail_units_eli + afford_occ_units_eli - renter_hh_eli) %>%
  rename(county = name) %>%
  mutate(county = gsub(" .*", "", county)) %>%
  mutate(across(!contains("county"), as.numeric))

chas_pa <- chas %>%
  summarize(afford_avail_units_eli = sum(afford_avail_units_eli),
            housing_balance = sum(housing_balance),
            county = "statewide_avg",
            renter_hh_eli = sum(renter_hh_eli))

#### Census rural-urban by county 
rural <- st_read("/Users/jstaro/Documents/GitHub/PHFA-Housing-Dash/data/2020_UA_COUNTY.xlsx") %>% 
  dplyr::filter(Field1 == "42") %>%
  mutate(rural = ifelse(as.numeric(Field22) / as.numeric(Field5) > 0.5, 1, 0)) %>%
  dplyr::select(Field4, rural) %>%
  rename(county = Field4)

#### County shapefiles
pa_counties <- counties(state = "PA") %>%
  dplyr::select("county" = "NAME") %>%
  st_transform("WGS84")

erie <- pa_counties %>%
  filter(county == "Erie") # %>%
  # erase_water(area_threshold = 0.999) #################### EDIT 

noterie <- pa_counties %>%
  filter(county != "Erie")

counties.sf <- rbind(erie, noterie) %>%
  st_cast("POLYGON") %>%
  distinct(county, .keep_all = TRUE)

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

st_write(panel.sf, "PHFA_dash_data_May.25.geojson", driver = "GeoJSON")

state_avg = cbind(chas_pa, dat_PA)

st_write(state_avg, "state_avg_05-25.csv", driver = "CSV")



#####
leaflet(counties.sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white", color = "black", weight = 1, opacity = 1, fillOpacity = 0.7)

