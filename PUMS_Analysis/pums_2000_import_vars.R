#Set up libraries for the analysis
library(tidycensus)
library(tidyverse)
library(mapcdatakeys)
library(dplyr)

# lberman 2024-08-20  
# revised 2024-9-18 to use IPUMS downloads (not mdb database found on K drive)

#  note: PUMS access.mdb SERIALNO does NOT match IPUMS SERIAL, and cannot be joined
#  cf -> K:\DataServices\Datasets\U.S. Census and Demographics\PUMS\Archive\PUMS_5%_2000\2000_PUMS_MA.mdb 
#  households:  PUMS_5%_MA_2000_HousingUnits
#  persons:   PUMS_5%_MA_2000_Persons

#Remove scientific notation
options(scipen = 999)

# set output path
exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS/ipums_2000/"
#exp_path ="C:/Users/lberman/Downloads"


# Households are uniquely identified by the SERIALNO variable
# Persons are uniquely identified by SERIALNO and PERNUM(SPORDER).

# downloaded from IPUMS 2000 5%
# https://usa.ipums.org/

# 1.  import PERSON records directly downloaded from IPUMS

get_2000_pers <- read_csv(paste0(exp_path,"all_2000_ipums/2000_ipums_PERSON_overc.csv")) 

# 2.  import HH records directly downloaded from IPUMS

get_2000_hh <- read_csv(paste0(exp_path,"all_2000_ipums/2000_ipums_HH_overc.csv")) 

# 3. get list of vars for hh to compare

hh_list <- names(get_2000_hh)
hh_vars <- as.data.frame(hh_list)

hh_vars <- hh_vars  %>% 
  mutate(join_vars = hh_list)

# 4. get list of vars for person to compare

pers_list <- names(get_2000_pers)
pers_vars <- as.data.frame(pers_list)

pers_vars <- pers_vars  %>% 
  mutate(join_vars = pers_list)

# 5. compare duplicate vars & uniq vars in lists

ck_vars_2000 <- hh_vars %>%
  full_join(.,
            pers_vars,
            by = c('join_vars' = 'join_vars'))

# 6. drop cols from HH already existing in PERSON

trim_2000_hh <- get_2000_hh %>% 
  select(-c(YEAR,SAMPLE,HHWT,CLUSTER,STRATA,STATEFIP,GQ))



# 7. join hh to person df

join_ipums_2000 <-  merge(x = get_2000_pers, y = trim_2000_hh, by = "SERIAL", all.x = TRUE)


ipums_2000_overcrowding <- join_ipums_2000 %>% 
  select(c(YEAR,STATEFIP,SERIAL,PERNUM),everything())

# 8.  write compiled sample for 2000

# export OVERCROWDING
currentDate <- Sys.Date()
dateAsText <- format(currentDate, "%Y-%m-%d")
write_csv(ipums_2000_overcrowding, paste0(exp_path,"pums_overcrowding_2000_V_",dateAsText,".csv"))
