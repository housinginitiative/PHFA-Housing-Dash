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

#### statewide avg - 2021 ####
dat_PA <- get_acs(geography = "state", 
                 variables = c("owner_occ_hh" = "B25011_002", 
                               "total_hh" = "B11007_001",
                               "renter_occ_hh" = "B25003_003",
                               "hh_with_mortgage" = "B25096_002",
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
                               "owner_inc_less5k" = "B25118_003", #owner income bel 5k
                               "owner_inc_5_10k" = "B25118_004", #owner income 5 10k
                               "owner_inc_10_15k" = "B25118_005", #owner income 10 15k
                               "owner_inc_15_20k" = "B25118_006", #owner income 15 20k
                               "owner_inc_20_25k" = "B25118_007", #owner income 20 25k
                               "owner_inc_25_35k" = "B25118_008", #owner income 25 35k
                              
                               # renter low income
                               "renter_inc_less5k" = "B25118_015", # renter income below 5k
                               "renter_inc_5_10k" = "B25118_016",  # renter income 5 to 10k
                               "renter_inc_10_15k" = "B25118_017", # renter income 10 to 15k
                               "renter_inc_15_20k" = "B25118_018", # renter income 15 to 20k
                               "renter_inc_20_25k" = "B25118_019", # renter income 20 to 25k
                               "renter_inc_25_35k" = "B25118_020"  # renter income 25 to 35k
                               ), year = 2021, state = "PA", #20-35k 50%+
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         med_age_home = 2023-med_year_builtE,
         internet_hh_pct = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         # renter low inc
         renter_low_inc = renter_inc_less5kE + renter_inc_5_10kE + renter_inc_10_15kE + renter_inc_15_20kE + renter_inc_20_25kE + renter_inc_25_35kE,
         
         # owner low inc
         owner_low_inc = owner_inc_less5kE + owner_inc_5_10kE + owner_inc_10_15kE + owner_inc_15_20kE + owner_inc_20_25kE + owner_inc_25_35kE,
         #rent burden
         income_bel35k_rent_more30 = inc_less10k_rent30_35E + inc_less10k_rent35_40E + inc_less10k_rent40_50E + inc_less10k_rent50plusE +
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE + 
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE,
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100*(income_bel35k_rent_more30/renter_low_inc)), 0),
         # mortgage burden
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100*(income_bel35k_mort_more30/owner_low_inc)), 0),
         # rent_burdened_pct = ifelse(med_gross_rentE*12 > 5000*.3, income_below10kE/(renter_occ_hhE *0.01),
         #                            ifelse(med_gross_rentE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(renter_occ_hhE *0.01),
         #                                   ifelse(med_gross_rentE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(renter_occ_hhE *0.01),
         #                                          ifelse(med_gross_rentE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(renter_occ_hhE *0.01),
         #                                                 ifelse(med_gross_rentE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(renter_occ_hhE *0.01),
         #                                                        ifelse(med_gross_rentE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(renter_occ_hhE *0.01),
         #                                                               ifelse(med_gross_rentE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(renter_occ_hhE *0.01),
         #                                                                      ifelse(med_gross_rentE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(renter_occ_hhE *0.01),
         #                                                                             ifelse(med_gross_rentE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(renter_occ_hhE *0.01),
         #                                                                                    ifelse(med_gross_rentE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(renter_occ_hhE *0.01),
         #                                                                                           ifelse(med_gross_rentE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(renter_occ_hhE *0.01),
         #                                                                                                  ifelse(med_gross_rentE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(renter_occ_hhE *0.01),
         #                                                                                                         ifelse(med_gross_rentE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(renter_occ_hhE *0.01),
         #                                                                                                                ifelse(med_gross_rentE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(renter_occ_hhE *0.01),
         #                                                                                                                       ifelse(med_gross_rentE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(renter_occ_hhE *0.01),
         #                                                                                                                              ifelse(med_gross_rentE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(renter_occ_hhE *0.01),
         #                                                                                                                                     0)))))))))))))))),
         # mortgage_burdened_pct = ifelse(med_owner_costs_mortgagedE*12 > 5000*.3, income_below10kE/(hh_with_mortgageE*0.01),
         #                                ifelse(med_owner_costs_mortgagedE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(hh_with_mortgageE*0.01),
         #                                       ifelse(med_owner_costs_mortgagedE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(hh_with_mortgageE*0.01),
         #                                              ifelse(med_owner_costs_mortgagedE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(hh_with_mortgageE*0.01),
         #                                                     ifelse(med_owner_costs_mortgagedE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(hh_with_mortgageE*0.01),
         #                                                            ifelse(med_owner_costs_mortgagedE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(hh_with_mortgageE*0.01),
         #                                                                   ifelse(med_owner_costs_mortgagedE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(hh_with_mortgageE*0.01),
         #                                                                          ifelse(med_owner_costs_mortgagedE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(hh_with_mortgageE*0.01),
         #                                                                                 ifelse(med_owner_costs_mortgagedE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(hh_with_mortgageE*0.01),
         #                                                                                        ifelse(med_owner_costs_mortgagedE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(hh_with_mortgageE*0.01),
         #                                                                                               ifelse(med_owner_costs_mortgagedE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(hh_with_mortgageE*0.01),
         #                                                                                                      ifelse(med_owner_costs_mortgagedE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(hh_with_mortgageE*0.01),
         #                                                                                                             ifelse(med_owner_costs_mortgagedE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                    ifelse(med_owner_costs_mortgagedE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                           ifelse(med_owner_costs_mortgagedE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                                  ifelse(med_owner_costs_mortgagedE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(hh_with_mortgageE*0.01),
         #                                                                                                                                         0)))))))))))))))),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, internet_hh_pct, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat_PA), 2021)

# Assign the modified column names back to the data frame
names(dat_PA) <- new_column_names



#### 2021 ####
dat21 <- get_acs(geography = "county", 
                 variables = c("owner_occ_hh" = "B25011_002", 
                               "total_hh" = "B11007_001",
                               "renter_occ_hh" = "B25003_003",
                               "hh_with_mortgage" = "B25096_002",
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
                               "owner_inc_less5k" = "B25118_003", #owner income bel 5k
                               "owner_inc_5_10k" = "B25118_004", #owner income 5 10k
                               "owner_inc_10_15k" = "B25118_005", #owner income 10 15k
                               "owner_inc_15_20k" = "B25118_006", #owner income 15 20k
                               "owner_inc_20_25k" = "B25118_007", #owner income 20 25k
                               "owner_inc_25_35k" = "B25118_008", #owner income 25 35k
                               
                               # renter low income
                               "renter_inc_less5k" = "B25118_015", # renter income below 5k
                               "renter_inc_5_10k" = "B25118_016",  # renter income 5 to 10k
                               "renter_inc_10_15k" = "B25118_017", # renter income 10 to 15k
                               "renter_inc_15_20k" = "B25118_018", # renter income 15 to 20k
                               "renter_inc_20_25k" = "B25118_019", # renter income 20 to 25k
                               "renter_inc_25_35k" = "B25118_020"  # renter income 25 to 35k
                 ), year = 2021, state = "PA", #20-35k 50%+
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         med_age_home = 2023-med_year_builtE,
         internet_hh_pct = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         # renter low inc
         renter_low_inc = renter_inc_less5kE + renter_inc_5_10kE + renter_inc_10_15kE + renter_inc_15_20kE + renter_inc_20_25kE + renter_inc_25_35kE,
         
         # owner low inc
         owner_low_inc = owner_inc_less5kE + owner_inc_5_10kE + owner_inc_10_15kE + owner_inc_15_20kE + owner_inc_20_25kE + owner_inc_25_35kE,
         #rent burden
         income_bel35k_rent_more30 = inc_less10k_rent30_35E + inc_less10k_rent35_40E + inc_less10k_rent40_50E + inc_less10k_rent50plusE +
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE + 
           inc_10_20k_rent30_35E + inc_10_20k_rent35_40E + inc_10_20k_rent40_50E + inc_10_20k_rent50plusE,
         rent_burdened_pct = ifelse(renter_low_inc > 0, round(100*(income_bel35k_rent_more30/renter_low_inc)), 0),
         # mortgage burden
         income_bel35k_mort_more30 = inc_bel20k_mort30plusE + inc_20_35k_mort30plusE,
         mortgage_burdened_pct = ifelse(owner_low_inc > 0, round(100*(income_bel35k_mort_more30/owner_low_inc)), 0),
         # rent_burdened_pct = ifelse(med_gross_rentE*12 > 5000*.3, income_below10kE/(renter_occ_hhE *0.01),
         #                            ifelse(med_gross_rentE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(renter_occ_hhE *0.01),
         #                                   ifelse(med_gross_rentE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(renter_occ_hhE *0.01),
         #                                          ifelse(med_gross_rentE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(renter_occ_hhE *0.01),
         #                                                 ifelse(med_gross_rentE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(renter_occ_hhE *0.01),
         #                                                        ifelse(med_gross_rentE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(renter_occ_hhE *0.01),
         #                                                               ifelse(med_gross_rentE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(renter_occ_hhE *0.01),
         #                                                                      ifelse(med_gross_rentE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(renter_occ_hhE *0.01),
         #                                                                             ifelse(med_gross_rentE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(renter_occ_hhE *0.01),
         #                                                                                    ifelse(med_gross_rentE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(renter_occ_hhE *0.01),
         #                                                                                           ifelse(med_gross_rentE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(renter_occ_hhE *0.01),
         #                                                                                                  ifelse(med_gross_rentE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(renter_occ_hhE *0.01),
         #                                                                                                         ifelse(med_gross_rentE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(renter_occ_hhE *0.01),
         #                                                                                                                ifelse(med_gross_rentE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(renter_occ_hhE *0.01),
         #                                                                                                                       ifelse(med_gross_rentE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(renter_occ_hhE *0.01),
         #                                                                                                                              ifelse(med_gross_rentE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(renter_occ_hhE *0.01),
         #                                                                                                                                     0)))))))))))))))),
         # mortgage_burdened_pct = ifelse(med_owner_costs_mortgagedE*12 > 5000*.3, income_below10kE/(hh_with_mortgageE*0.01),
         #                                ifelse(med_owner_costs_mortgagedE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(hh_with_mortgageE*0.01),
         #                                       ifelse(med_owner_costs_mortgagedE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(hh_with_mortgageE*0.01),
         #                                              ifelse(med_owner_costs_mortgagedE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(hh_with_mortgageE*0.01),
         #                                                     ifelse(med_owner_costs_mortgagedE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(hh_with_mortgageE*0.01),
         #                                                            ifelse(med_owner_costs_mortgagedE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(hh_with_mortgageE*0.01),
         #                                                                   ifelse(med_owner_costs_mortgagedE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(hh_with_mortgageE*0.01),
         #                                                                          ifelse(med_owner_costs_mortgagedE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(hh_with_mortgageE*0.01),
         #                                                                                 ifelse(med_owner_costs_mortgagedE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(hh_with_mortgageE*0.01),
         #                                                                                        ifelse(med_owner_costs_mortgagedE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(hh_with_mortgageE*0.01),
         #                                                                                               ifelse(med_owner_costs_mortgagedE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(hh_with_mortgageE*0.01),
         #                                                                                                      ifelse(med_owner_costs_mortgagedE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(hh_with_mortgageE*0.01),
         #                                                                                                             ifelse(med_owner_costs_mortgagedE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                    ifelse(med_owner_costs_mortgagedE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                           ifelse(med_owner_costs_mortgagedE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(hh_with_mortgageE*0.01),
         #                                                                                                                                  ifelse(med_owner_costs_mortgagedE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(hh_with_mortgageE*0.01),
         #                                                                                                                                         0)))))))))))))))),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(county, owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, internet_hh_pct, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat21), 2021)

# Assign the modified column names back to the data frame
names(dat21) <- new_column_names

#### 2016 ####
dat16 <- get_acs(geography = "county", 
                 variables = c("owner_occ_hh" = "B25011_002", 
                               "total_hh" = "B11007_001",
                               "renter_occ_hh" = "B25003_003",
                               "hh_with_mortgage" = "B25096_002",
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
                              # "internet_hh" = "B28002_002",
                               "med_year_built" = "B25035_001",
                               "med_home_value" = "B25107_001",
                               "medhhinc" = "B19013A_001",
                               "med_owner_costs_mortgaged" = "B25088_002",
                               "med_gross_rent" = "B25064_001"), year = 2016, state = "PA",
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         med_age_home = 2016-med_year_builtE,
        # internet_hh = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         rent_burdened_pct = ifelse(med_gross_rentE*12 > 5000*.3, income_below10kE/(renter_occ_hhE *0.01),
                                    ifelse(med_gross_rentE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(renter_occ_hhE *0.01),
                                           ifelse(med_gross_rentE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(renter_occ_hhE *0.01),
                                                  ifelse(med_gross_rentE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(renter_occ_hhE *0.01),
                                                         ifelse(med_gross_rentE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(renter_occ_hhE *0.01),
                                                                ifelse(med_gross_rentE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(renter_occ_hhE *0.01),
                                                                       ifelse(med_gross_rentE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(renter_occ_hhE *0.01),
                                                                              ifelse(med_gross_rentE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(renter_occ_hhE *0.01),
                                                                                     ifelse(med_gross_rentE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(renter_occ_hhE *0.01),
                                                                                            ifelse(med_gross_rentE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(renter_occ_hhE *0.01),
                                                                                                   ifelse(med_gross_rentE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(renter_occ_hhE *0.01),
                                                                                                          ifelse(med_gross_rentE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(renter_occ_hhE *0.01),
                                                                                                                 ifelse(med_gross_rentE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(renter_occ_hhE *0.01),
                                                                                                                        ifelse(med_gross_rentE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(renter_occ_hhE *0.01),
                                                                                                                               ifelse(med_gross_rentE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(renter_occ_hhE *0.01),
                                                                                                                                      ifelse(med_gross_rentE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(renter_occ_hhE *0.01),
                                                                                                                                             0)))))))))))))))),
         mortgage_burdened_pct = ifelse(med_owner_costs_mortgagedE*12 > 5000*.3, income_below10kE/(hh_with_mortgageE*0.01),
                                        ifelse(med_owner_costs_mortgagedE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(hh_with_mortgageE*0.01),
                                               ifelse(med_owner_costs_mortgagedE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(hh_with_mortgageE*0.01),
                                                      ifelse(med_owner_costs_mortgagedE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(hh_with_mortgageE*0.01),
                                                             ifelse(med_owner_costs_mortgagedE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(hh_with_mortgageE*0.01),
                                                                    ifelse(med_owner_costs_mortgagedE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(hh_with_mortgageE*0.01),
                                                                           ifelse(med_owner_costs_mortgagedE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(hh_with_mortgageE*0.01),
                                                                                  ifelse(med_owner_costs_mortgagedE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(hh_with_mortgageE*0.01),
                                                                                         ifelse(med_owner_costs_mortgagedE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(hh_with_mortgageE*0.01),
                                                                                                ifelse(med_owner_costs_mortgagedE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(hh_with_mortgageE*0.01),
                                                                                                       ifelse(med_owner_costs_mortgagedE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(hh_with_mortgageE*0.01),
                                                                                                              ifelse(med_owner_costs_mortgagedE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(hh_with_mortgageE*0.01),
                                                                                                                     ifelse(med_owner_costs_mortgagedE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(hh_with_mortgageE*0.01),
                                                                                                                            ifelse(med_owner_costs_mortgagedE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(hh_with_mortgageE*0.01),
                                                                                                                                   ifelse(med_owner_costs_mortgagedE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(hh_with_mortgageE*0.01),
                                                                                                                                          ifelse(med_owner_costs_mortgagedE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(hh_with_mortgageE*0.01),
                                                                                                                                                 0)))))))))))))))),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(county, owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat16), 2016)

# Assign the modified column names back to the data frame
names(dat16) <- new_column_names

#### 2011 #### 
dat11 <- get_acs(geography = "county", 
                 variables = c("owner_occ_hh" = "B25011_002", 
                               "total_hh" = "B11007_001",
                               "renter_occ_hh" = "B25003_003",
                               "hh_with_mortgage" = "B25096_002",
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
                               # "internet_hh" = "B28002_002",
                               "med_year_built" = "B25035_001",
                               "med_home_value" = "B25107_001",
                               "medhhinc" = "B19013A_001",
                               "med_owner_costs_mortgaged" = "B25088_002",
                               "med_gross_rent" = "B25064_001"), year = 2016, state = "PA",
                 geometry = FALSE, survey = "acs5", output = "wide") %>%
  mutate(owner_occ_hh_pct = ifelse(total_hhE > 0, round(100*owner_occ_hhE/total_hhE), 0),
         renter_occ_hh_pct = ifelse(total_hhE > 0, round(100*renter_occ_hhE/total_hhE), 0),
         renter_vacant_pct = ifelse(renter_occ_hhE > 0, round(100*vacant_rental_unitsE/renter_occ_hhE), 0),
         med_age_home = 2011-med_year_builtE,
         # internet_hh = ifelse(total_hhE > 0, round(100*internet_hhE/total_hhE), 0),
         rent_burdened_pct = ifelse(med_gross_rentE*12 > 5000*.3, income_below10kE/(renter_occ_hhE *0.01),
                                    ifelse(med_gross_rentE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(renter_occ_hhE *0.01),
                                           ifelse(med_gross_rentE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(renter_occ_hhE *0.01),
                                                  ifelse(med_gross_rentE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(renter_occ_hhE *0.01),
                                                         ifelse(med_gross_rentE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(renter_occ_hhE *0.01),
                                                                ifelse(med_gross_rentE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(renter_occ_hhE *0.01),
                                                                       ifelse(med_gross_rentE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(renter_occ_hhE *0.01),
                                                                              ifelse(med_gross_rentE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(renter_occ_hhE *0.01),
                                                                                     ifelse(med_gross_rentE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(renter_occ_hhE *0.01),
                                                                                            ifelse(med_gross_rentE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(renter_occ_hhE *0.01),
                                                                                                   ifelse(med_gross_rentE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(renter_occ_hhE *0.01),
                                                                                                          ifelse(med_gross_rentE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(renter_occ_hhE *0.01),
                                                                                                                 ifelse(med_gross_rentE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(renter_occ_hhE *0.01),
                                                                                                                        ifelse(med_gross_rentE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(renter_occ_hhE *0.01),
                                                                                                                               ifelse(med_gross_rentE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(renter_occ_hhE *0.01),
                                                                                                                                      ifelse(med_gross_rentE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(renter_occ_hhE *0.01),
                                                                                                                                             0)))))))))))))))),
         mortgage_burdened_pct = ifelse(med_owner_costs_mortgagedE*12 > 5000*.3, income_below10kE/(hh_with_mortgageE*0.01),
                                        ifelse(med_owner_costs_mortgagedE*12 > 12500*.3, (income_below10kE + income_10k_15kE)/(hh_with_mortgageE*0.01),
                                               ifelse(med_owner_costs_mortgagedE*12 > 17500*.3, (income_below10kE + income_10k_15kE + income_15k_20kE)/(hh_with_mortgageE*0.01),
                                                      ifelse(med_owner_costs_mortgagedE*12 > 22500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE)/(hh_with_mortgageE*0.01),
                                                             ifelse(med_owner_costs_mortgagedE*12 > 27500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE)/(hh_with_mortgageE*0.01),
                                                                    ifelse(med_owner_costs_mortgagedE*12 > 32500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE)/(hh_with_mortgageE*0.01),
                                                                           ifelse(med_owner_costs_mortgagedE*12 > 37500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE)/(hh_with_mortgageE*0.01),
                                                                                  ifelse(med_owner_costs_mortgagedE*12 > 42500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE)/(hh_with_mortgageE*0.01),
                                                                                         ifelse(med_owner_costs_mortgagedE*12 > 47500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE)/(hh_with_mortgageE*0.01),
                                                                                                ifelse(med_owner_costs_mortgagedE*12 > 55000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE)/(hh_with_mortgageE*0.01),
                                                                                                       ifelse(med_owner_costs_mortgagedE*12 > 67500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE)/(hh_with_mortgageE*0.01),
                                                                                                              ifelse(med_owner_costs_mortgagedE*12 > 87500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE)/(hh_with_mortgageE*0.01),
                                                                                                                     ifelse(med_owner_costs_mortgagedE*12 > 112500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE)/(hh_with_mortgageE*0.01),
                                                                                                                            ifelse(med_owner_costs_mortgagedE*12 > 137500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE)/(hh_with_mortgageE*0.01),
                                                                                                                                   ifelse(med_owner_costs_mortgagedE*12 > 175000*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_150k_200kE)/(hh_with_mortgageE*0.01),
                                                                                                                                          ifelse(med_owner_costs_mortgagedE*12 > 162500*0.3, (income_below10kE + income_10k_15kE + income_15k_20kE + income_20k_25kE + income_25k_30kE + income_30k_35kE + income_35k_40kE + income_40k_45kE + income_45k_50kE + income_50k_60kE + income_60k_75kE + income_75k_100kE + income_100k_125kE + income_125k_150kE + income_200k_plusE)/(hh_with_mortgageE*0.01),
                                                                                                                                                 0)))))))))))))))),
         county = word(NAME, 1)) %>%
  rename(med_gross_rent = med_gross_rentE,
         med_home_value = med_home_valueE) %>%
  dplyr::select(county, owner_occ_hh_pct, renter_occ_hh_pct, renter_vacant_pct, med_age_home, med_home_value, rent_burdened_pct, mortgage_burdened_pct, med_gross_rent) 

new_column_names <- paste0(names(dat11), 2011)

# Assign the modified column names back to the data frame
names(dat11) <- new_column_names


dat <- dat21  %>%
#   left_join(dat16, by = c("county2021" = "county2016")) %>%
#   left_join(dat11, by = c("county2021" = "county2011")) %>%
  rename(county = county2021) 

#### CHAS Data 
chas <- st_read("/Users/annaduan/Desktop/GitHub/PHFA\ dashboard/data/PACounty_2015-2019.xlsx") %>%
  mutate(Geography = word(Geography, end = 1))
names(chas) <- c("county", "renter_hh", "afford_avail_units", "housing_balance")

chas_pa <- chas %>%
  mutate(weight = renter_hh/10000,
         afford_avail_units_weighted = round(afford_avail_units*weight),
         housing_balance_weighted = round(housing_balance*weight)) %>%
  summarize(afford_avail_units = sum(afford_avail_units_weighted)/sum(weight),
            housing_balance = sum(housing_balance_weighted)/sum(weight)) %>%
  mutate(county = "statewide_avg",
         renter_hh = 434595)

#### Census rural-urban by county 
rural <- st_read("/Users/annaduan/Desktop/GitHub/PHFA\ dashboard/data/2020_UA_COUNTY.xlsx") %>% 
  dplyr::filter(Field1 == "42") %>%
  mutate(rural = ifelse(as.numeric(Field22)/as.numeric(Field5) > 0.5, 1, 0)) %>%
  dplyr::select(Field4, rural) %>%
  rename(county = Field4)

#### County shapefiles
counties.sf <- counties(state = "PA") %>%
  dplyr::select("county" = "NAME") %>%
  st_transform("WGS84")

#### Write panel #### 
dat <- dat %>%
  left_join(chas, by = "county") %>%
  left_join(rural, by = "county") %>%
  left_join(counties.sf, by = "county") %>%
  st_as_sf() %>%
  st_make_valid()
  
### Oct 3 2023
# counties <- counties(state = 42) %>%
# rename(county = NAME) %>%
#   dplyr::select(county) %>%
#   st_transform("WGS84")

# dat <- st_read("phfa_dash_data_9.28.geojson") %>%
#   st_drop_geometry() %>%
#   left_join(counties, by = "county") %>%
#   st_as_sf()

panel.sf <- dat %>%
  dplyr::select(county) %>%
  st_centroid() %>%
  st_drop_geometry() %>%
  left_join(dat, by = "county") %>%
  st_as_sf()

st_write(panel.sf, "PHFA_dash_data_nov8.geojson", driver="GeoJSON")


state_avg = cbind(chas_pa, dat_PA)

st_write(state_avg, "state_avg_nov8.csv", driver = "CSV")
#### Leaflet test run ####
dat <- st_read("/Users/annaduan/Documents/GitHub/PHFA\ dashboard/app.sept29/PHFA_dash_data_October3.geojson") %>%
  st_as_sf()


pal <- colorFactor("YlGnBu", dat$owner_occ_hh_pct_2021)


labs_dat <- sprintf(
  "<strong>%s</strong><br/>
  Home Ownership Rate $%s<sup></sup>",
  dat$county, dat$owner_occ_hh_pct_2021
) %>% lapply(htmltools::HTML)

# 
# title <- tags$div(
#   HTML("<strong>2021 ACS Estimates</strong><br/>
#         Hover to see individual counties"))  


leaflet(dat) %>%
  addPolygons(fillColor = ~pal(owner_occ_hh_pct_2021),
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
              ,
               label = labs_dat,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto")
  )%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addControl(title, position = "topright") %>%
  addLegend(pal = pal, values = ~owner_occ_hh_pct_2021, opacity = 0.7, title = "Home Ownership Rate<br/>by County (%)",
            position = "bottomright")
