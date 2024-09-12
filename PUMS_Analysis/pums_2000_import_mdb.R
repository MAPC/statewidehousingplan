#Set up libraries for the analysis

library(RODBC)
library(dplyr)
library(readr)

# lberman 2024-08-20

# from PUMS access.mdb located here:
#  K:\DataServices\Datasets\U.S. Census and Demographics\PUMS\Archive\PUMS_5%_2000\2000_PUMS_MA.mdb 
#  tables from .mdb 
#  households:  PUMS_5%_MA_2000_HousingUnits
#  persons:   PUMS_5%_MA_2000_Persons

# see crosswalk from 2000 variables list to 2021
# K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/pums_overcrowding_vars_2021_2000_xwalk.csv


#Remove scientific notation
options(scipen = 999)


# set output path
exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS"
exp_path ="C:/Users/lberman/Downloads"


## 1 read hh and person tables directly from PUMS 2000 access db 

# this file: K:/DataServices/Datasets/U.S. Census and Demographics/PUMS/Archive/PUMS_5%_2000/2000_PUMS_MA.mdb
# moved to C drive for speed

require(RODBC)
conn <- odbcConnectAccess2007('C:/Users/lberman/Desktop/SWAP/other_projects/statewide_housing_plan/2000_PUMS_MA.mdb')
subset(sqlTables(conn), TABLE_TYPE == "TABLE")

pums_2000_hh <- sqlFetch(conn, "PUMS_5%_MA_2000_HousingUnits")
pums_2000_per <- sqlFetch(conn, "PUMS_5%_MA_2000_Persons")

close(conn)

# notes on datasets

# Households are uniquely identified by the SERIALNO variable
# Persons are uniquely identified by the combination of SERIALNO and SPORDER.

# hh table contains a (115 cols) the person table (165 cols) 

# these two tables have three vars in common: ID, RECTYPE, SERIALNO

# person table column PNUM also contains line for hh (where PNUM = 52)  
# person table order number of persons begins with 2 (not 1) cf SPORDER 2021 (begins with 1)
# person table age for hh default is = 99


# 2 compare variables in two tables
#rm(hh_list,hh_vars)
# 2.1  hh vars
hh_list <- names(pums_2000_hh)
hh_vars <- as.data.frame(hh_list)

hh_vars <- hh_vars  %>% 
  mutate(join_vars = hh_list)

# 2.2 pers vars
pers_list <- names(pums_2000_per)
pers_vars <- as.data.frame(pers_list)

# 2.3 join the two lists
join_hh_pers_vars <- pers_vars %>% 
  left_join(.,
            hh_vars,
            by = c('pers_list' = 'join_vars')) 

# 2.4 filter the vars to show which are missing from hh 
missing_vars <- join_hh_pers_vars %>% 
  filter(is.na(hh_list))


# 3  join on SERIALNO

# 3.1  filter to hh rows only
hh_only <- pums_2000_hh %>% 
  filter(RECTYPE %in% "H") %>% 
  mutate(RECTYPEH = RECTYPE) %>% 
  select(-c(RECTYPE)) 
  

# 3.2
join_pums_2000 <- pums_2000_per %>% 
  left_join(.,
            hh_only,
            by = c('SERIALNO' = 'SERIALNO'))

# 3.3  get the names for comparison with other years
rm(vars_00)
list_00 <- names(join_pums_2000)
vars_00 <- as.data.frame(list_00)

vars_00 <- vars_00  %>% 
  mutate(vars_2000 = list_00)



# 4 import existing vars for 2009 to cross-check

get_2009 <- read_csv(paste0(exp_path,"/pums_overcrowding_2009_V_2024-09-12.csv")) 


# 4.1 get names of vars as df
list_09 <- names(get_2009)
vars_09 <- as.data.frame(list_09)

vars_09 <- vars_09  %>% 
  mutate(join_vars = list_09)



# 5  join with 2000 full set of vars to see what matches and is missing

compare_00_09_vars <- vars_09 %>% 
  left_join(.,
            vars_00,
            by = c('join_vars' = 'vars_2000'))

# 5.1  the values in list_00 = NA are the ones to look up on IPUMS form

vars_to_look_up <- compare_00_09_vars %>% 
  filter(is.na(list_00))

# 5.2  known matches:
# SPORDER = PERNUM


# 4 import additional Sub-Family variables from IPUMS

# set import path
imp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/"

file.exists("K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/ipums_2000_subf_ma_2024-08-29.csv")

file.exists("K:/DataServices/tmp/b25041_bedrooms_per_unit_m.csv")

getwd()
subf <- read_csv(paste0(imp_path,"ipums_2000_subf_ma_2024-08-29.csv"))
