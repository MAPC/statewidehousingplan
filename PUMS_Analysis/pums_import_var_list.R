#Set up libraries for the analysis
library(tidycensus)
library(tidyverse)
library(survey)
library(srvyr)
library(data.table)
library(mapcdatakeys)
library(janitor)


# lberman 2024-08-06
# https://walker-data.com/tidycensus/articles/pums-data.html
# https://walker-data.com/tidycensus/reference/get_pums.html

#Remove scientific notation
options(scipen = 999)

# set output path
exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS"
exp_path ="C:/Users/lberman/Downloads"

# Housing units are uniquely identified by the SERIALNO variable
# Persons are uniquely identified by the combination of SERIALNO and SPORDER.

# 1. get pums vars
pums_vars_2021 <- pums_variables %>% 
  filter(year == 2021, survey == "acs5")

# 1.1 see person vars
pums_vars_2021 %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "person")

# 1.2 see housing vars
pums_vars_2021 %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "housing")


# 2  set up vars list to retrieve
# var names: https://mapc365.sharepoint.com/:x:/s/DataServicesSP/EWFFCY8nSd9PpCQJh5XQlUYBfmT4WRxqWX4kR489r2uo3Q?rtime=GFzJ_jC23Eg

# note:  do not explicitly call the defaults
# default will always return SERIALNO, SPORDER, WGTP, PWGTP, and ST.

var.list  <- c('RT',
               'PUMA',
               'ADJHSG',
               'ADJINC',
               'NP',
               'TYPEHUGQ',
               'BDSP',
               'BLD',
               'RMSP',
               'TEN',
               'VACS',
               'VEH',
               'YRBLT',
               'CPLT',
               'GRNTP',
               'GRPIP',
               'HHLDRAGEP',
               'HHLDRRAC1P',
               'HHT',
               'HHT2',
               'HINCP',
               'HUGCL',
               'HUPAC',
               'HUPAOC',
               'HUPARC',
               'LNGI',
               'MULTG',
               'MV',
               'NPF',
               'NPP',
               'NR',
               'NRC',
               'OCPIP',
               'PARTNER')

var.list.b  <- c('PSF',
               'R18',
               'R60',
               'R65',
               'SMOCP',
               'SRNT',
               'SVAL',
               'AGEP',
               'DDRS',
               'DEAR',
               'DEYE',
               'DOUT',
               'DPHY',
               'DRAT',
               'DRATX',
               'DREM',
               'ENG',
               'LANX',
               'RELSHIPP',
               'SCH',
               'SCHG',
               'SCHL',
               'SEX',
               'YOEP',
               'DIS',
               'HISP',
               'NATIVITY',
               'OC',
               'POBP',
               'RAC2P',
               'SFR',
               'WAOB')


# 2.1 filter to the requested var.lists

ck_vars <- pums_vars_2021 %>% 
   filter(var_code %in% var.list)
 
ck_vars_b <- pums_vars_2021 %>% 
   filter(var_code %in% var.list.b)

pums_vars <- rbind(ck_vars,ck_vars_b)

# 2.2  save full Data Dictionary of var.list

write_csv(pums_vars, paste0(exp_path,"/pums_vars_DataDict.csv"))

# 2.3 trim to uniq variables (not split to hous / person, or sub-sets)
# # count uniq vars
uniq_vars <- pums_vars %>%
  distinct(var_code,var_label,level) %>%
  arrange(var_code)
  
# 2.4  save version of uniq variables in var.list
write_csv(uniq_vars, paste0(exp_path,"/pums_vars_uniq.csv"))

######
# 3 retrieve data function
# WGTP = housing-unit weight; PWGTP person weight
# add "recode = TRUE" to retrieve non-coded readable values


pums_2021_raw <- get_pums(
  variables = var.list,
  state = "MA",
  survey = "acs5",
  year = 2021
)

pums_2021_raw_b <- get_pums(
  variables = var.list.b,
  state = "MA",
  survey = "acs5",
  year = 2021
)


# 4 create uniq id for each hh + person [SERIALNO + SPORDER]
pums_2021_raw <- pums_2021_raw %>% 
  mutate(uniq_obs = paste0(SERIALNO,"_",SPORDER))

pums_2021_raw_b <- pums_2021_raw_b %>% 
  mutate(uniq_obs = paste0(SERIALNO,"_",SPORDER))

# 5 merge all the columns on the uniq_id keep all

pums_2021 <- merge(pums_2021_raw,pums_2021_raw_b,by='uniq_obs')


# 6 export
write_csv(pums_2021, paste0(exp_path,"/pums_raw_data_2021.csv"))
