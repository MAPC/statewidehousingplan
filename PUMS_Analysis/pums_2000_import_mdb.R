#Set up libraries for the analysis

library(RODBC)

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

# hh table contains a subset (115 cols) of the person table (165 cols) ?

# person table column PNUM also contains line for hh (where PNUM = 52)  
# person table order number of persons begins with 2 (not 1) cf SPORDER 2021 (begins with 1)
# person table age for hh default is = 99
