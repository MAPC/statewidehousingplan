#Set up libraries for the analysis

library(RODBC)
library(dplyr)

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
#exp_path ="C:/Users/lberman/Downloads"


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
rm(hh_list,hh_vars)
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
  filter(RECTYPE %in% "H")

# 3.2
join_pums_2000 <- pums_2000_per %>% 
  left_join(.,
            hh_only,
            by = c('SERIALNO' = 'SERIALNO'))
