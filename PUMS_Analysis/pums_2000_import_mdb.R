#Set up libraries for the analysis

library(RODBC)
library(dplyr)
library(readr)
library(ipumsr)

# lberman 2024-08-20, 8-29

# from PUMS access.mdb located here:
#  K:\DataServices\Datasets\U.S. Census and Demographics\PUMS\Archive\PUMS_5%_2000\2000_PUMS_MA.mdb 
#  tables from .mdb 
#  households:  PUMS_5%_MA_2000_HousingUnits
#  persons:   PUMS_5%_MA_2000_Persons

# see also crosswalk from 2000 variables list to 2021
# K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/pums_overcrowding_vars_2021_2000_xwalk.csv

#------------------#
# 0. set paths
work_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/"

# 0.1 set date
currentDate <- Sys.Date()
dateAsText <- format(currentDate, "%Y-%m-%d")

#------------------#
## 1 read hh and person tables directly from PUMS 2000 access db 
# src file: K:/DataServices/Datasets/U.S. Census and Demographics/PUMS/Archive/PUMS_5%_2000/2000_PUMS_MA.mdb

require(RODBC)

# 1.1 connect explicit path to MDB
conn <- odbcConnectAccess2007('K:/DataServices/Datasets/U.S. Census and Demographics/PUMS/Archive/PUMS_5%_2000/2000_PUMS_MA.mdb')

# 1.2 list all mdb contents
subset(sqlTables(conn), TABLE_TYPE == "TABLE")

# 1.3 get hh and person tables
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

#------------------#
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

# note only three vars in common: ID, RECTYPE, SERIALNO

#------------------#
# 3  join hh to person on SERIALNO

# 3.1  filter to hh rows only
hh_only <- pums_2000_hh %>% 
  filter(RECTYPE %in% "H")

# 3.2 prepare join by renaming ID and RECTYPE for hh

hh_only <- hh_only %>% 
  mutate(hh_ID = ID) %>% 
  mutate(hh_RECTYPE = RECTYPE) %>% 
  select(-c(ID,RECTYPE)) %>% 
  select(hh_ID,hh_RECTYPE,everything())

# 3.3
join_pums_2000 <- pums_2000_per %>% 
  left_join(.,
            hh_only,
            by = c('SERIALNO' = 'SERIALNO'))

# note: join has redundant values for hh filled in for each person in hh

# 3.4 export the joined dataset hh + person
write_csv(join_pums_2000, paste0(work_path,"pums_2000_draft_",dateAsText,".csv"))


#------------------#
# 4 import IPUMS csv download SubFamily vars

# file obtained by downloading 2000 vars as csv, filtered on characteristic STATEFIPS = 025
#file.exists("K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/ipums_2000_subf_ma_2024-08-29.csv")

# 4.2 import
subf <- read_csv(paste0(imp_path,"ipums_2000_subf_ma_2024-08-29.csv"))

# cannot join this tabular data to mdb output, the SERIAL != SERIALNO



#------------------#
# 5. attempt to use full "hierarchical" download available in .dat only

# 5.1 rqmt R snippet from ipums and ipumsr library
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

require(ipumsr)
#if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# 5.2 path to data
imp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/"

# 5.3 ingest the ddi metadata
ddi <- read_ipums_ddi(paste0(imp_path,"usa_00004.xml"))

# 5.4 ingest the full hierarchical .dat format dataset
data <- read_ipums_micro(ddi)

# same issue, the SERIAL != SERIALNO   
# identical number of rows in .csv vs .dat versions 460748
