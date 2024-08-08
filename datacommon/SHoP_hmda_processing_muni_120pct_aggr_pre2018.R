# Packages ----
library(tidyverse)
library(lubridate)
library(tidycensus)
library(RPostgres)
library(DBI)
library(dplyr)
library(stringr)
library(mapcdatakeys)

# High Income Mortgage Denials by Race for years BEFORE 2018

# Data Source ----

# Pre 2017
# https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=ma&records=all-records&field_descriptions=labels

#
# muni version draft for Statewide Housing Plan request
# the main functions were pulled from the script by Aseem Deodhar: hmda_processing_ct_ma_aggr.R
# added section for ct00_id (reading from draft .csv)
# added section to concatenate census tract ID (from Taylor Perez example)
# lberman 2024-06-25 rev 2014-08-07

# -------------------------------------------------------------------------


## 0. set variables
hmda_yr <- 2016


# 0.2 derive year for keys
cosub_yr <- str_sub(hmda_yr, -2)

# note census tract IDs for 2020 were not applied until 2022
cosub_col <- if(hmda_yr < 2013) {
  "ct00_id"
} else if (hmda_yr < 2022) {
  "ct10_id"  
} else {
  "ct20_id"  
} 


# 0.3 set paths
import_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/"

exp_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/2007-2017/redo/"

test_path = "H:/0_PROJECTS/2024_hmda_mort_denial/output/"

# 0.4  Database Connection -----------------------------------------------------
# for VM
config_path <- "K:/DataServices/Code/Python/Python_ACS_Script/config_local/"
# for laptop in office
#config_path <- "S:/Network Shares/NEW K Drive/DataServices/Code/Python/Python_ACS_Script/config_local/"
source(paste0(config_path,"conf.R"))



#### NOTE ON SOURCE DATA
## downloaded from URL above as "hmda_2017_ma_all-records_labels.csv"
## placed in RAW data folder for year and changed filename to:  hmda_ma_2017.csv
## for example: K:\DataServices\Datasets\Housing\HMDA\Data\Raw\Tabular\2017\hmda_ma_2017.csv

####
## 1. import data
raw_import <- read_csv(paste0(import_path,hmda_yr,"/hmda_ma_",hmda_yr,".csv"))

## 1.1 rm blank rows  (2017 had 713 rows with na)
import <- raw_import %>% 
  filter(!is.na(census_tract_number))

# 1.2 reset tract number
import <- import %>% 
  mutate(census_tract_number = as.numeric(census_tract_number)) %>% 
  mutate(tract_raw = census_tract_number*100) %>% 
  mutate(tract_raw = as.character(tract_raw)) %>% 
  mutate(state_id = state_code)

# 1.3 pad county code to three digits, and tract number to six digits
import$county_id <- str_pad(import$county_code, width=3, side='left', pad='0')
import$tract_pad <- str_pad(import$tract_raw, width=6, side='left', pad='0')

# 1.4 concatenate tract, cleanup interim cols
import <- import %>% 
  mutate(census_tract = as.numeric(paste0(state_code,county_id,tract_pad))) %>% 
  select(-c(state_code,county_code,tract_pad,tract_raw,census_tract_number))


## 2. get data keys xw for the year
# see: https://github.com/MAPC/mapcdatakeys/blob/main/census_geog_keys.md

keys <- if(hmda_yr < 2013) {
  read_csv(paste0(test_path,'geog_xw_2000_DRAFT.csv'))  
} else if(hmda_yr < 2022) { #hmda data uses 2010 census tracts through 2021
    mapcdatakeys::geog_xw_2010  
} else {
  mapcdatakeys::geog_xw_2020  
} 

# 2.1 trim to distinct ct id

dist_keys <- if(hmda_yr < 2013) {
    distinct(keys, ct00_id, .keep_all = TRUE) %>% 
      mutate(cosub_id = ct00_id)
} else if(hmda_yr < 2022) {
  distinct(keys, ct10_id, .keep_all = TRUE) %>% 
    mutate(cosub_id = ct10_id)
} else {
  distinct(keys, ct20_id, .keep_all = TRUE) %>% 
  mutate(cosub_id = ct20_id)
} 

# note there are only 1619 ct20_id rows in keys
# missing ct20_id = 25023990003 (which has null geometry)

# 2.3  join keys to raw data

join_keys <- if(hmda_yr < 2013) {
  import %>%
    left_join(.,
              dist_keys %>% select(ct00_id,muni_id,muni_name,county,cosub_id),
              by = c('census_tract' = 'ct00_id'))  
} else if(hmda_yr < 2022) {
  import %>%
    left_join(.,
              dist_keys %>% select(ct10_id,muni_id,muni_name,county,cosub_id),
              by = c('census_tract' = 'ct10_id'))
} else {
  import %>%
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
#write_csv(x, paste0(test_path,"/",hmda_yr,"_non_join_census_tracts.csv"))

rm(missing_muni_id,x)

## 3. process raw data for muni level years 2018 - 2021

# this section for cleaning pre 2018 values for eth and race
# based on definitions in: lar_record_codes.pdf 
# replacement values based on code numbers

# ethn_ck <- distinct(keys_import, applicant_ethnicity, applicant_ethnicity_name, .keep_all = FALSE) %>% 
#   arrange(applicant_ethnicity)

# update race ethnicity into two fields

# 3.1 cols [applicant_ethnicity] and [applicant_race_1] instead of [derived_ethnicity] and [derived_race]
clean_import <- keys_import %>% 
  mutate(race_ethnicity = case_when(applicant_ethnicity == 1 & applicant_race_1 == 1 ~ "lat", #eth hispanic
                                    applicant_ethnicity == 1 & applicant_race_1 == 2 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 3 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 4 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 5 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 6 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 7 ~ "lat",
                                    applicant_ethnicity == 1 & applicant_race_1 == 8 ~ "lat",
                                    
                                    applicant_ethnicity == 2 & applicant_race_1 == 1 ~ "nav", #eth not hispanic
                                    applicant_ethnicity == 2 & applicant_race_1 == 2 ~ "asn",
                                    applicant_ethnicity == 2 & applicant_race_1 == 3 ~ "baa",
                                    applicant_ethnicity == 2 & applicant_race_1 == 4 ~ "nhp",
                                    applicant_ethnicity == 2 & applicant_race_1 == 5 ~ "whi",
                                    applicant_ethnicity == 2 & applicant_race_1 == 6 ~ "na",
                                    applicant_ethnicity == 2 & applicant_race_1 == 7 ~ "na",
                                    applicant_ethnicity == 2 & applicant_race_1 == 8 ~ "na",
                                    
                                    applicant_ethnicity == 3 & applicant_race_1 == 1 ~ "nav", #eth info not provided
                                    applicant_ethnicity == 3 & applicant_race_1 == 2 ~ "asn",
                                    applicant_ethnicity == 3 & applicant_race_1 == 3 ~ "baa",
                                    applicant_ethnicity == 3 & applicant_race_1 == 4 ~ "nhp",
                                    applicant_ethnicity == 3 & applicant_race_1 == 5 ~ "whi",
                                    applicant_ethnicity == 3 & applicant_race_1 == 6 ~ "na",
                                    applicant_ethnicity == 3 & applicant_race_1 == 7 ~ "na",
                                    applicant_ethnicity == 3 & applicant_race_1 == 8 ~ "na",
                                    
                                    applicant_ethnicity == 4 & applicant_race_1 == 1 ~ "nav", #eth not applicable
                                    applicant_ethnicity == 4 & applicant_race_1 == 2 ~ "asn",
                                    applicant_ethnicity == 4 & applicant_race_1 == 3 ~ "baa",
                                    applicant_ethnicity == 4 & applicant_race_1 == 4 ~ "nhp",
                                    applicant_ethnicity == 4 & applicant_race_1 == 5 ~ "whi",
                                    applicant_ethnicity == 4 & applicant_race_1 == 6 ~ "na",
                                    applicant_ethnicity == 4 & applicant_race_1 == 7 ~ "na",
                                    applicant_ethnicity == 4 & applicant_race_1 == 8 ~ "na",
                                    
                                    applicant_ethnicity == 5 & applicant_race_1 == 1 ~ "nav", #eth no co-applicant
                                    applicant_ethnicity == 5 & applicant_race_1 == 2 ~ "asn",
                                    applicant_ethnicity == 5 & applicant_race_1 == 3 ~ "baa",
                                    applicant_ethnicity == 5 & applicant_race_1 == 4 ~ "nhp",
                                    applicant_ethnicity == 5 & applicant_race_1 == 5 ~ "whi",
                                    applicant_ethnicity == 5 & applicant_race_1 == 6 ~ "na",
                                    applicant_ethnicity == 5 & applicant_race_1 == 7 ~ "na",
                                    applicant_ethnicity == 5 & applicant_race_1 == 8 ~ "na"),                                      
         
         income = applicant_income_000s*1000 
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


rm(get_oth_mfi)
# 4.2 compare ACS B19113
get_oth_mfi <- get_acs(geography = 'tract', 
                   table = 'B19113', 
                   year = hmda_yr,
                   state = 25,
                   survey = 'acs5',
                   cache = TRUE) |> 
  select(!c("moe")) %>% 
  pivot_wider(
    names_from = variable,
    names_glue = "{variable}",
    values_from = c(estimate)
  ) %>% 
  mutate(amfi = B19113_001) %>% 
  mutate(mfi_geoid = as.numeric(GEOID))


mfi_import <- clean_import %>%
  left_join(.,
            inc_lim %>% select(hud_muni_id,hud_muni_nm,hud_median),
            by = c('muni_id' = 'hud_muni_id')) %>% 
            select(-c(hud_muni_nm))        

# to compare the tract level AMFI used in original code by Aseem Deodhar add this join to B19113
# mfi_import <- mfi_import %>%
#   left_join(.,
#             get_oth_mfi %>% select(amfi,mfi_geoid),
#             by = c('census_tract' = 'mfi_geoid'))
# 
# # note 145 missing rows in amfi col from B19113
# missing_amfi <- mfi_import %>% 
#   filter(is.na(amfi))

#rm(inc_lim,keys_import,raw_import,keys,join_keys)



## 5 analysis functions

##  note:  amfi [adjusted median family income] is joined from ACS B19113 for the data year
##  high_inc is based on (income/amfi * 100) where > 120 = 120% above the amfi

## replacing b19113.amfi  with sdvm.hous_section8_income_limits_by_year_m.median = hud_median


hmda_mu_func_1821 <- function(hmda_yr){
  mfi_import %>% 
    select(as_of_year, #after 2018 = activity_year
           muni_id,
           muni_name,
           race_ethnicity,
           action_taken,
           income,
#           amfi,  # uncomment this if adding B19113 AMFI
           hud_median) %>%
    
    # removed left_join to tidycensus table B19113, using joined mfi from HUD Income Limits table

    mutate(
      #calculate median income percent value
      med_inc_pc = (income/hud_median)*100,  # change hud_median to amfi if using B19113 tract level 
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
    select(as_of_year, #after 2018 = activity_year
           muni_id,
           muni_name,
           race_ethnicity,
           action_taken,
           income,
#           amfi,  # uncomment this if adding B19113 AMFI
           hud_median,
           cosub_id
           ) %>% 
  
    mutate(
      #calculate median income percent value
      med_inc_pc = (income/hud_median)*100,  # change hud_median to amfi if using B19113 tract level 
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
  
## 10. export to csv (change output filename if using TractAMFI column)
write_csv(df, paste0(exp_path,"hmda_",hmda_yr,"_mortgage_denials_by_race_120pct.csv"))

#ck output at exp_path = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/2007-2017/redo/"