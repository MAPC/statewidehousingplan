# Packages ----
library(tidyverse)
library(lubridate)
library(tidycensus)
library(RPostgres)
library(DBI)
library(dplyr)
library(stringr)
library(mapcdatakeys)

# High Income Mortgage Denials by Race for years BEGINNING 2018

# Data Source ----


# Latest: post 2017
# https://ffiec.cfpb.gov/data-browser/data/2018?category=states


# HMDA Dataset Filtering
# select Data Year, Step One: Geography = State, MASSACHUSETTS
# Step Two, Financial Institution (no selection) Step Three, Filter Variables (no selection)
# download the file, for example: state_MA.csv
# change filename to syntax:  hmda_ma_yyyy.csv, like hmda_ma_2018.csv
# move file to input folder: K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/yyyy/ 

# muni version draft for Statewide Housing Plan request
# the main functions were pulled from the script by Aseem Deodhar: hmda_processing_ct_ma_aggr.R
# note: if else checks for corrections of ct_id (for example 2020 use ct10_id!)
# lberman 2024-06-25 rev 2014-08-13

# -------------------------------------------------------------------------


## 0. set variables
hmda_yr <- 2023


# 0.2 derive year for keys
cosub_yr <- str_sub(hmda_yr, -2)

# note census tract IDs for 2020 were not applied until 2022
cosub_col <- if(hmda_yr < 2022) {
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

keys <- if(hmda_yr < 2022) { #hmda data uses 2010 census tracts through 2021
  mapcdatakeys::geog_xw_2010  
} else {
  mapcdatakeys::geog_xw_2020  
} 

# 2.1 trim to distinct ct id

dist_keys <- if(hmda_yr < 2022) {
  distinct(keys, ct10_id, .keep_all = TRUE) %>% 
    mutate(cosub_id = ct10_id)
} else {
  distinct(keys, ct20_id, .keep_all = TRUE) %>% 
    mutate(cosub_id = ct20_id)
} 

# note there are only 1619 ct20_id rows in keys
# missing ct20_id = 25023990003 (which has null geometry)

# 2.3  join keys to raw data

join_keys <- if(hmda_yr < 2022) {
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

## 2.4 rm rows with blank muni_id (which had blank or incorrect ct id) (fix trailing -1)
keys_import <- join_keys %>% 
  filter(!is.na(muni_id)) %>% 
  mutate(applicant_ethnicity = `applicant_ethnicity-1`) %>% 
  mutate(applicant_race = `applicant_race-1`)
  
# 2.5  check rows to be dropped 
missing_muni_id <- join_keys %>% 
  filter(is.na(muni_id))

# 2.6 of these, which have distinct tract ids?
x <- distinct(missing_muni_id, census_tract, .keep_all = TRUE)


# 2.7 export for research
#write_csv(x, paste0(test_path,"/",hmda_yr,"_non_join_census_tracts.csv"))

rm(missing_muni_id,x)

## 3. process raw data for muni level years 2018 - 2021

# 3.1 clean up names found in derived_ethnicity and derived_race into column race_ethnicity
#  expand with new lines to account for errors in the data, eg "joint" "Free From Text Only" 

# note from 2018 the applicant-ethnicity-1 and applicant_race-1 values were augmented with new items
# compare pre-2018: https://files.consumerfinance.gov/hmda-historic-data-dictionaries/lar_record_codes.pdf
# to 2018 on: https://ffiec.cfpb.gov/documentation/publications/loan-level-datasets/lar-data-fields

clean_import <- keys_import %>% 
  mutate(race_ethnicity = case_when(applicant_ethnicity == 1 & applicant_race == 1 ~ "lat", #eth hispanic = 1
                                    applicant_ethnicity == 1 & applicant_race == 2 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 21 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 22 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 23 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 24 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 25 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 26 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 27 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 3 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 4 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 41 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 42 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 43 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 44 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 5 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 6 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race == 7 ~ "lat",
                                    
                                    applicant_ethnicity == 11 & applicant_race == 1 ~ "lat", #eth hispanic = 11 Mexican
                                    applicant_ethnicity == 11 & applicant_race == 2 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 21 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 22 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 23 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 24 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 25 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 26 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 27 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 3 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 4 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 41 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 42 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 43 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 44 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 5 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 6 ~ "lat",
                                    applicant_ethnicity == 11 & applicant_race == 7 ~ "lat",
                                    
                                    applicant_ethnicity == 12 & applicant_race == 1 ~ "lat", #eth hispanic = 12 Puerto Rican
                                    applicant_ethnicity == 12 & applicant_race == 2 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 21 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 22 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 23 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 24 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 25 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 26 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 27 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 3 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 4 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 41 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 42 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 43 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 44 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 5 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 6 ~ "lat",
                                    applicant_ethnicity == 12 & applicant_race == 7 ~ "lat",
                                    
                                    applicant_ethnicity == 13 & applicant_race == 1 ~ "lat", #eth hispanic = 13 Cuban
                                    applicant_ethnicity == 13 & applicant_race == 2 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 21 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 22 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 23 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 24 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 25 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 26 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 27 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 3 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 4 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 41 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 42 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 43 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 44 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 5 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 6 ~ "lat",
                                    applicant_ethnicity == 13 & applicant_race == 7 ~ "lat",
                                    
                                    applicant_ethnicity == 14 & applicant_race == 1 ~ "lat", #eth hispanic = 14 other hispanic or latino
                                    applicant_ethnicity == 14 & applicant_race == 2 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 21 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 22 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 23 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 24 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 25 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 26 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 27 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 3 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 4 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 41 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 42 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 43 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 44 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 5 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 6 ~ "lat",
                                    applicant_ethnicity == 14 & applicant_race == 7 ~ "lat",
                                    
                                    applicant_ethnicity == 2 & applicant_race == 1 ~ "nav", #eth hispanic = 2 not hispanic or latino
                                    applicant_ethnicity == 2 & applicant_race == 2 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 21 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 22 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 23 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 24 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 25 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 26 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 27 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race == 3 ~ "baa",
                                    applicant_ethnicity == 2 & applicant_race == 4 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race == 41 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race == 42 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race == 43 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race == 44 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race == 5 ~ "whi",
                                    applicant_ethnicity == 2 & applicant_race == 6 ~ "na",
                                    applicant_ethnicity == 2 & applicant_race == 7 ~ "na",
                                    
                                    
                                    applicant_ethnicity == 3 & applicant_race == 1 ~ "nav", #eth hispanic = 3 info not provided
                                    applicant_ethnicity == 3 & applicant_race == 2 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 21 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 22 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 23 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 24 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 25 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 26 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 27 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race == 3 ~ "baa",
                                    applicant_ethnicity == 3 & applicant_race == 4 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race == 41 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race == 42 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race == 43 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race == 44 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race == 5 ~ "whi",
                                    applicant_ethnicity == 3 & applicant_race == 6 ~ "na",
                                    applicant_ethnicity == 3 & applicant_race == 7 ~ "na",
                                    
                                    applicant_ethnicity == 4 & applicant_race == 1 ~ "nav", #eth hispanic = 4 not applicable
                                    applicant_ethnicity == 4 & applicant_race == 2 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 21 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 22 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 23 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 24 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 25 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 26 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 27 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race == 3 ~ "baa",
                                    applicant_ethnicity == 4 & applicant_race == 4 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race == 41 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race == 42 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race == 43 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race == 44 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race == 5 ~ "whi",
                                    applicant_ethnicity == 4 & applicant_race == 6 ~ "na",
                                    applicant_ethnicity == 4 & applicant_race == 7 ~ "na",
                                    
                                    is.na(applicant_ethnicity) & applicant_race == 1 ~ "nav", #eth hispanic = NA not applicable
                                    is.na(applicant_ethnicity) & applicant_race == 2 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 21 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 22 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 23 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 24 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 25 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 26 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 27 ~ "asn",
                                    is.na(applicant_ethnicity) & applicant_race == 3 ~ "baa",
                                    is.na(applicant_ethnicity) & applicant_race == 4 ~ "nhp",
                                    is.na(applicant_ethnicity) & applicant_race == 41 ~ "nhp",
                                    is.na(applicant_ethnicity) & applicant_race == 42 ~ "nhp",
                                    is.na(applicant_ethnicity) & applicant_race == 43 ~ "nhp",
                                    is.na(applicant_ethnicity) & applicant_race == 44 ~ "nhp",
                                    is.na(applicant_ethnicity) & applicant_race == 5 ~ "whi",
                                    is.na(applicant_ethnicity) & applicant_race == 6 ~ "na",
                                    is.na(applicant_ethnicity) & applicant_race == 7 ~ "na",

                                    applicant_ethnicity == 1 & is.na(applicant_race) ~ "lat", #eth hispanic = 1,11,12,13 and race NA
                                    applicant_ethnicity == 11 & is.na(applicant_race) ~ "lat",
                                    applicant_ethnicity == 12 & is.na(applicant_race) ~ "lat",                                    applicant_ethnicity == 12 & is.na(applicant_race) ~ "lat",
                                    applicant_ethnicity == 13 & is.na(applicant_race) ~ "lat",
                                    applicant_ethnicity == 14 & is.na(applicant_race) ~ "lat"),
         
         
         income = income*1000 
  )
  

# 3.2  ck removal of error in previous step  (note, category added for "na")
ck_muni <- clean_import %>%
  filter(is.na(race_ethnicity))
#note: when step 3.2 returns only rows with [applicant_ethnicity = 2, 3, 4 or NA], and [applicant_race = NA], then proceed

ck_muni_step <- ck_muni %>% 
  filter(applicant_ethnicity != 2) %>% # 2 = not hispanic
  filter(applicant_ethnicity != 3) %>% # 3 = hisp info not provided
  filter(applicant_ethnicity != 4) %>% # 4 = not applicable
  filter(applicant_ethnicity != "NA")

rm(ck_muni,ck_muni_step)

# 3.3 filter out any remaining rows with is.na(race_ethnicity)
clean_import <- clean_import %>%
  filter(!is.na(race_ethnicity))



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
  mutate(na_rate = round(na_rate, 2))

## 10. export to csv
write_csv(df, paste0(exp_path,"/",hmda_yr,"/hmda_mortgage_denials_by_race_120pct_",hmda_yr,".csv"))

#ck output at exp_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/"