# Packages ----
library(tidyverse)
library(lubridate)
library(tidycensus)
library(RPostgres)
library(DBI)
library(dplyr)
library(stringr)
library(mapcdatakeys)

# High Income Mortgage Denial

# Data Source ----

# Pre 2017
# https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=ma&records=all-records&field_descriptions=labels

# Latest: post 2017
# https://ffiec.cfpb.gov/data-publication/aggregate-reports

# muni version draft for Statewide Housing Plan request
# the main functions were pulled from the script by Aseem Deodhar: hmda_processing_ct_ma_aggr.R
# note: if else checks for corrections of ct_id (for example 2020 use ct10_id!)
# lberman 2024-06-25 rev 2014-07-30

# -------------------------------------------------------------------------


## 0. set variables
hmda_yr <- 2020

# 0.1 note:  mapcdatakeys have not been updated after 2021 
hmda_yr_fix <- if(hmda_yr < 2020) {
  hmda_yr
} else if(hmda_yr == 2020 ) {
  2019 
} else {
  2021 
}


# 0.2 derive year for keys
cosub_yr <- str_sub(hmda_yr, -2)

cosub_col <- if(hmda_yr < 2020) {
  "ct10_id"
} else if(hmda_yr == 2020) {
  "ct10_id"  
} else {
  "ct20_id"  
} 


# 0.3 set paths
import_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/"

exp_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/"

test_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

# 0.4  Database Connection -----------------------------------------------------
# for VM
config_path <- "K:/DataServices/Code/Python/Python_ACS_Script/config_local/"
# for laptop in office
#config_path <- "S:/Network Shares/NEW K Drive/DataServices/Code/Python/Python_ACS_Script/config_local/"
source(paste0(config_path,"conf.R"))


####
## 1. import data
raw_import <- read_csv(paste0(import_path,hmda_yr,"/hmda_ma_",hmda_yr,".csv"))

## 2. get data keys xw for the year
# see: https://github.com/MAPC/mapcdatakeys/blob/main/census_geog_keys.md

keys <- if(hmda_yr < 2021) {
  mapcdatakeys::geog_xw_2010  
} else if(hmda_yr == 2020) {
  mapcdatakeys::geog_xw_2010  
} else {
  mapcdatakeys::geog_xw_2020  
} 

# 2.1 trim to distinct ct id

dist_keys <- if(hmda_yr < 2021) {
  distinct(keys, ct10_id, .keep_all = TRUE) %>% 
  mutate(cosub_id = ct10_id)
} else if(hmda_yr == 2020) {
  distinct(keys, ct10_id, .keep_all = TRUE) %>% 
    mutate(cosub_id = ct10_id)
} else {
  distinct(keys, ct20_id, .keep_all = TRUE) %>% 
  mutate(cosub_id = ct20_id)
} 

# note there are only 1619 ct20_id rows in keys
# missing ct20_id = 25023990003 (which has null geometry)

# 2.3  join keys to raw data

join_keys <- if(hmda_yr < 2021) {
  raw_import %>%
    left_join(.,
              dist_keys %>% select(ct10_id,muni_id,muni_name,county,cosub_id),
              by = c('census_tract' = 'ct10_id')) 
} else if(hmda_yr == 2020) {
  raw_import %>%
    left_join(.,
              dist_keys %>% select(ct10_id,muni_id,muni_name,county,cosub_id),
              by = c('census_tract' = 'ct10_id')) 
} else {
  raw_import %>%
    left_join(.,
              dist_keys %>% select(ct20_id,muni_id,muni_name,county,cosub_id),
              by = c('census_tract' = 'ct20_id')) 
} 


## 2.4 rm rows with blank muni_id (which had blank or incorrect ct id)
keys_import <- join_keys %>% 
  filter(!is.na(muni_id))

# 2.5  check rows to be dropped 
missing_muni_id <- join_keys %>% 
  filter(is.na(muni_id))

# 2.6 of these, which have distinct tract ids?
x <- distinct(missing_muni_id, census_tract, .keep_all = TRUE)


# 2.7 export for research
write_csv(x, paste0(test_path,"/",hmda_yr,"_non_join_census_tracts.csv"))

rm(missing_muni_id,x)

## 3. process raw data for muni level years 2018 - 2021

# 3.1 clean up names found in derived_ethnicity and derived_race into column race_ethnicity
#  expand with new lines to account for errors in the data, eg "joint" "Free From Text Only" 
clean_import <- keys_import %>% 
  mutate(race_ethnicity = case_when(derived_ethnicity == "Hispanic or Latino" & derived_race == "White" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Black or African American" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Asian" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Native Hawaiian or Other Pacific Islander" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "American Indian or Alaska Native" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Race Not Available" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "2 or more minority races" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Free Form Text Only" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "joint" ~ "lat",
                                    derived_ethnicity == "Hispanic or Latino" & derived_race == "Joint" ~ "lat",
                                    
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "White" ~ "whi",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Black or African American" ~ "baa",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Asian" ~ "asn",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Native Hawaiian or Other Pacific Islander" ~ "nhp",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "American Indian or Alaska Native" ~ "nav",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Joint" ~ "na",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Race Not Available" ~ "na",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "2 or more minority races" ~ "mlt",
                                    derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Free Form Text Only" ~ "na",
                                    
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Race Not Available" ~ "na",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "White" ~ "whi",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "2 or more minority races" ~ "lat",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Black or African American" ~ "baa",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Asian" ~ "asn",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Native Hawaiian or Other Pacific Islander" ~ "nhp",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "American Indian or Alaska Native" ~ "nav",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "joint" ~ "na",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Joint" ~ "na",
                                    derived_ethnicity == "Ethnicity Not Available" & derived_race == "Free Form Text Only" ~ "na",
                                    
                                    
                                    derived_ethnicity == "White" & derived_race == "Joint" ~ "whi",
                                    
                                    derived_ethnicity == "Joint" & derived_race == "White" ~ "whi",
                                    derived_ethnicity == "Joint" & derived_race == "Black or African American" ~ "baa",
                                    derived_ethnicity == "Joint" & derived_race == "Asian" ~ "asn",
                                    derived_ethnicity == "Joint" & derived_race == "Native Hawaiian or Other Pacific Islander" ~ "nhp",
                                    derived_ethnicity == "Joint" & derived_race == "American Indian or Alaska Native" ~ "nav",
                                    derived_ethnicity == "Joint" & derived_race == "Race Not Available" ~ "na",
                                    derived_ethnicity == "Joint" & derived_race == "Joint" ~ "na",                                 
                                    derived_ethnicity == "Joint" & derived_race == "2 or more minority races" ~ "mlt",                                
                                    derived_ethnicity == "Joint" & derived_race == "Free Form Text Only" ~ "na",
                                    
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "White" ~ "whi",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Black or African American" ~ "baa",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Asian" ~ "asn",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Native Hawaiian or Other Pacific Islander" ~ "nhp",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "American Indian or Alaska Native" ~ "nav",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Race Not Available" ~ "na",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Free Form Text Only" ~ "na",
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "joint" ~ "na",  
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "Joint" ~ "na",  
                                    derived_ethnicity == "Free Form Text Only" & derived_race == "2 or more minority races" ~ "mlt"),                                      
         
         income = income*1000 
         ) 

# 3.2  ck removal of error in previous step  (note, categories added for "na" and "mlt")
ck_muni <- clean_import %>%
  filter(is.na(race_ethnicity))

#note: when step 3.2 returns zero rows, then proceed
rm(ck_muni)


# 4. get HUD Income limits 

# 4.1 retrieve by year from sdvm
qry <- paste0("SELECT * FROM tabular.hous_section8_income_limits_by_year_m WHERE fy_year = ",hmda_yr) #keep hmda_yr for the HUD income limits
inc_lim <- dbGetQuery(con_sdevm, qry) %>% 
  select(c(muni_id,municipal,median)) %>% 
  mutate(hud_muni_id = muni_id) %>% 
  mutate(hud_muni_nm = municipal) %>% 
  mutate(hud_median = median) %>% 
  select(-c(muni_id,municipal,median))

mfi_import <- clean_import %>%
  left_join(.,
            inc_lim %>% select(hud_muni_id,hud_muni_nm,hud_median),
            by = c('muni_id' = 'hud_muni_id')) %>% 
            select(-c(hud_muni_nm))        

#rm(inc_lim,keys_import,raw_import,keys,join_keys)



## 5 analysis functions

##  note:  amfi [adjusted median family income] is joined from ACS B19113 for the data year
##  high_inc is based on (income/amfi * 100) where > 120 = 120% above the amfi

## replacing b19113.amfi  with sdvm.hous_section8_income_limits_by_year_m.median = hud_median


hmda_mu_func_1821 <- function(hmda_yr){
  mfi_import %>% 
    select(activity_year,
           muni_id,
           muni_name,
           race_ethnicity,
           action_taken,
           income,
           hud_median) %>%
    
    # removed left_join to tidycensus table B19113

    mutate(
      #calculate median income percent value
      med_inc_pc = (income/hud_median)*100,
      #create binary for high income applicants     
      high_inc = case_when(med_inc_pc >= 120 ~ 'hinc'),
      # create loan action_taken id
      loan_action_adj = case_when(action_taken == 3 ~ 'den',
                                  action_taken == 1 ~ 'app')) %>% 
    
    # Filter to only high income loan applications
    filter(high_inc == 'hinc') %>% 
    group_by(race_ethnicity,
             loan_action_adj,
             muni_id, muni_name) %>% 
    summarise(count = n()) %>% 
    filter(!is.na(loan_action_adj) & !is.na(race_ethnicity)) %>% 
    
    pivot_wider(names_from = loan_action_adj,
                values_from = count) %>% 
    mutate(across(.cols = c(app, den), .fns = function(x){replace_na(x, 0)}),
           rate = (den/(den+app))*100) %>%
    
    pivot_wider(names_from = race_ethnicity,
                names_glue = "{race_ethnicity}_{.value}",
                values_from = c(app, den, rate)) %>% 
    full_join(.,
              #mapcdatakeys::all_muni_data_keys %>% select(muni_id, muni_name) %>% mutate(muni_id = as.character(muni_id))) %>%
              mapcdatakeys::all_muni_data_keys %>% select(muni_id, muni_name) %>% mutate(muni_id = muni_id)) %>%
    ungroup() %>% 
    mutate(year = hmda_yr) %>% 
    rename(municipal = muni_name)
}

## 5 set up muni aggregation levels function

hmda_mu_aggr_func_1821 <- function(hmda_yr, grp_id, grp_name){
  mfi_import %>% 
    select(activity_year,
           muni_id,
           muni_name,
           race_ethnicity,
           action_taken,
           income,
           hud_median,
           cosub_id
           ) %>% 
  
    # left_join(., get_acs(geography = "county subdivision", state = 25,
    #                      survey = "acs5", year = hmda_yr, table = 'B19113', cache_table = TRUE) %>% 
    #             select(GEOID, estimate) %>% rename(cosub_id = GEOID, amfi = estimate) %>% 
    #             left_join(.,
    #                       mapcdatakeys::census_muni_keys %>% 
    #                         select(muni_id, muni_name, 
    #                                c(ends_with(str_sub(hmda_yr_fix, start = 3, end = 4)) & !contains("cosub_cn"))
    #                         ) %>% rename(cosub_id = 3) %>% mutate(across(.cols = c(cosub_id, muni_id),
    #                                                                      .fns = function(x){as.character(x)}))) %>% 
    #             mutate(muni_id = as.integer(muni_id)) %>% 
    #             filter(!is.na(muni_id)) %>% arrange(muni_id)) %>% arrange(race_ethnicity) %>%

    # left_join(.,
    #           dist_keys %>% select(ct20_id,muni_id),
    #           by = c('census_tract' = ct20_id)) %>%      
    
    mutate(
      #calculate median income percent value
      med_inc_pc = (income/hud_median)*100,
      #create binary for high income applicants     
      high_inc = case_when(med_inc_pc >= 120 ~ 'hinc'),
      # create loan action_taken id
      loan_action_adj = case_when(action_taken == 3 ~ 'den',
                                  action_taken == 1 ~ 'app')) %>% 
    
    # Filter to only high income loan applications
    filter(high_inc == 'hinc') %>% 
    group_by(race_ethnicity,
             loan_action_adj,
             muni_name, muni_id, cosub_id) %>% 
    summarise(count = n()) %>% 
    filter(!is.na(loan_action_adj) & !is.na(race_ethnicity)) %>% 
    
    pivot_wider(names_from = loan_action_adj,
                values_from = count) %>% 
    
    
    # join aggregation geographies
    
    left_join(., mapcdatakeys::all_muni_data_keys %>% 
                select(muni_id, muni_name, 
                       subrg_id, subrg_acr, county_id, county, 
                       cmtyp08_id, cmtyp08, cmsbt08_id, cmsbt08, 
                       rpa_id, rpa_name, region_id, region) %>% 
                mutate(state_id = 353, state = "Massachusetts", muni_id = muni_id), 
              by = c("muni_id", "muni_name")) %>%
    #group_by(race_ethnicity, county_id, county) %>% 
    group_by(race_ethnicity, {{grp_id}}, {{grp_name}}) %>%
    summarise(across(.cols = c(app, den), sum, na.rm = TRUE)) %>%
    
    mutate(across(.cols = c(app, den), .fns = function(x){replace_na(x, 0)}),
           rate = (den/(den+app))*100) %>%
    
    pivot_wider(names_from = race_ethnicity,
                names_glue = "{race_ethnicity}_{.value}",
                values_from = c(app, den, rate)) %>% 
    mutate(year = hmda_yr) %>% 
    dplyr::rename(muni_id = 1, municipal = 2)
}

## 6. run muni data function
# Municipal ---------------------------------------------------------------
hmda_mortgage_denials_by_race_120pct_m <- hmda_mu_func_1821(hmda_yr = hmda_yr)

## 7. run aggregation function
# Aggregations ---------

hmda_mortgage_denials_by_race_120pct_aggr <-
  # Subregion # id = 3 # nm = 4
  bind_rows(
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr, grp_id = subrg_id, grp_name = subrg_acr),
    # County # id = 5 # nm = 6
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = county_id, grp_name = county),
    # Community Type # id = 7 # nm = 8
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = cmtyp08_id, grp_name = cmtyp08),
    # Community Subtype # id = 9 # nm = 10
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = cmsbt08_id, grp_name = cmsbt08),
    # RPA # id = 11 # nm = 12
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = rpa_id, grp_name = rpa_name),
    # Region # id = 13 # nm = 14
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = region_id, grp_name = region),
    # State # id = 15 # nm = 16
    hmda_mu_aggr_func_1821(hmda_yr = hmda_yr,grp_id = state_id, grp_name = state)) %>% 
  filter(!is.na(muni_id))  %>% 
  filter(municipal != "Metropolitan Area Planning Council") %>% 
  dplyr::distinct()

## 8  Binding 351 + 53 = 404 --------------------------------------------------

## 8.1 keeping the na and mlt columns (not part of orig db schema)
hmda_mortgage_denials_by_race_120pct <- hmda_mortgage_denials_by_race_120pct_m %>%
   bind_rows(., hmda_mortgage_denials_by_race_120pct_aggr)

## 9 decimals fix rates before export
df <- hmda_mortgage_denials_by_race_120pct 
df[df == Inf] <- 0
df[is.na(df)] <- 0
df <- df %>% 
  mutate(asn_rate = round(asn_rate, 2)) %>% 
  mutate(baa_rate = round(baa_rate, 2)) %>% 
  mutate(lat_rate = round(lat_rate, 2)) %>% 
  mutate(nav_rate = round(nav_rate, 2)) %>% 
  mutate(nhp_rate = round(nhp_rate, 2)) %>% 
  mutate(whi_rate = round(whi_rate, 2)) %>%  
  mutate(mlt_rate = round(mlt_rate, 2)) %>% 
  mutate(na_rate = round(na_rate, 2))
  
## 10. export to csv
write_csv(df, paste0(exp_path,"/",hmda_yr,"/hmda_mortgage_denials_by_race_120pct.csv"))

#ck output at exp_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/"