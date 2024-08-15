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


# hh from PUMS ftp https://www.census.gov/programs-surveys/acs/microdata/access.html

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
# use sheet 1

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
               'HHLDRHISP',
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
                 'ENG',
                 'LANX',
                 'RELSHIPP',
                 'SCH',
                 'SCHG',
                 'SCHL',
                 'SEX',
                 'YOEP',
                 'DIS',
                 'RAC1P',
                 'HISP',
                 'NATIVITY',
                 'OC',
                 'POBP',
                 'RAC2P',
                 'SFN',
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



pums_2021_raw <- get_pums(
  variables = var.list,
  state = "MA",
  survey = "acs5",
  year = 2021,
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

# 6 fix some duplicate cols from the join
pums_2021 <- pums_2021 %>% 
  mutate(SPORDER = SPORDER.x) %>% 
  mutate(SERIALNO = SERIALNO.x) %>% 
  mutate(ST = ST.x) %>% 
  mutate(WGTP = WGTP.x) %>% 
  mutate(PWGTP = PWGTP.x) %>% 
  select(-c(SPORDER.x,SPORDER.y,SERIALNO.x,SERIALNO.y,ST.x,ST.y,WGTP.x,WGTP.y,PWGTP.x,PWGTP.y))


# 7 re-order the person df to match column order in sheet 1
pums_2021_reorder <- pums_2021 [c('uniq_obs',
                                 'SERIALNO',
                                 'SPORDER',
                                 'ST',
                                 'WGTP',
                                 'PWGTP',
                                 'RT',
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
                                 'HHLDRHISP',
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
                                 'PARTNER',
                                 'PSF',
                                 'R18',
                                 'R60',
                                 'R65',
                                 'SMOCP',
                                 'SRNT',
                                 'SVAL',
                                 'AGEP',
                                 'ENG',
                                 'LANX',
                                 'RELSHIPP',
                                 'SCH',
                                 'SCHG',
                                 'SCHL',
                                 'SEX',
                                 'YOEP',
                                 'DIS',
                                 'RAC1P',
                                 'HISP',
                                 'NATIVITY',
                                 'OC',
                                 'POBP',
                                 'RAC2P',
                                 'SFN',
                                 'SFR',
                                 'WAOB')]
  


# 8 read csv of hh data downloaded from pums ftp
hh_pums <- read.csv("C:/Users/lberman/Downloads/csv_hma/psam_h25.csv", header = TRUE, stringsAsFactors = FALSE)

# 8.1 add SPORDER column = 0
hh_pums['SPORDER'] = '0'

# 8.2 create the uniq_obs id
hh_pums_2021 <- hh_pums %>% 
  mutate(uniq_obs = paste0(SERIALNO,"_",SPORDER))


# 9 compare the cols of pums_reorder and hh data (need to add empty cols to hh)

# 9.1 get the names of cols as df
data <- names(pums_2021_reorder)
data_raw <- data.frame(data)

reord <- names(hh_pums_2021)
reorder_list <- data.frame(reord)

reorder_list <- reorder_list %>%
  mutate(data = reord)

# 9.2 join the two lists of cols
join_list <- data_raw %>%
  left_join(.,
            reorder_list,
            by = c('data' = 'data'))

# 9.3 filter to the missing cols
missing_from_hh <- join_list %>% 
  filter(is.na(reord)) %>% 
  select(-c(reord))

# 9.4 convert col to list
new_cols <- list(missing_from_hh$data)  # edited the list items for next step

# 9.4 add the missing cols to hh
hh_pums_2021_newcols <- hh_pums_2021
hh_pums_2021_newcols[c("PWGTP", "AGEP", "ENG", "LANX", "RELSHIPP", "SCH", "SCHG", "SCHL", "SEX", "YOEP", "DIS", "RAC1P", "HISP", "NATIVITY", "OC", "POBP", "RAC2P", "SFN", "SFR", "WAOB")] <- NA

# 10 check cols again 
data <- names(pums_2021_reorder)
data_raw <- data.frame(data)

# 10.1
newcol_ck <- names(hh_pums_2021_newcols)
newcol_ck <- data.frame(newcol_ck)

newcol_list <- newcol_ck %>%
  mutate(ck = newcol_ck)

# 10.2 join the two lists of cols
join_list_2 <- data_raw %>%
  left_join(.,
            newcol_list,
            by = c('data' = 'ck'))

# 10.3 filter to the missing cols
still_missing <- join_list_2 %>% 
  filter(is.na(newcol_ck)) %>% 
  select(-c(newcol_ck))


# 11 reorder the hh df
hh_pums_2021_reorder <- hh_pums_2021_newcols [c('uniq_obs',
                                  'SERIALNO',
                                  'SPORDER',
                                  'ST',
                                  'WGTP',
                                  'PWGTP',
                                  'RT',
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
                                  'HHLDRHISP',
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
                                  'PARTNER',
                                  'PSF',
                                  'R18',
                                  'R60',
                                  'R65',
                                  'SMOCP',
                                  'SRNT',
                                  'SVAL',
                                  'AGEP',
                                  'ENG',
                                  'LANX',
                                  'RELSHIPP',
                                  'SCH',
                                  'SCHG',
                                  'SCHL',
                                  'SEX',
                                  'YOEP',
                                  'DIS',
                                  'RAC1P',
                                  'HISP',
                                  'NATIVITY',
                                  'OC',
                                  'POBP',
                                  'RAC2P',
                                  'SFN',
                                  'SFR',
                                  'WAOB')]


# 12 bind the trimmed and reordered person df with hh df

pums_all_2021 <- rbind(pums_2021_reorder, hh_pums_2021_reorder)


# 13 export  version = V_yyyy-mm-dd
currentDate <- Sys.Date()
dateAsText <- format(currentDate, "%Y-%m-%d")
write_csv(pums_all_2021, paste0(exp_path,"/pums_raw_data_2021_V_",dateAsText,".csv"))



#14 SUBSET OVERCROWDING 
pums_overcrowding_2021 <- pums_2021 %>% 
  select(c(uniq_obs, SPORDER, SERIALNO, ST, WGTP, PWGTP, RT, PUMA, ADJHSG, ADJINC, NP, BDSP, RMSP, CPLT, HHLDRAGEP, HHLDRRAC1P, HHLDRHISP, HHT, HHT2, HINCP, HUGCL, HUPAC, HUPAOC, HUPARC, LNGI, MULTG, NPF, NPP, NR, NRC, PARTNER, PSF, R18, AGEP, RELSHIPP, RAC1P, OC, SFN, SFR))

# 14.1 export OVERCROWDING
write_csv(pums_overcrowding_2021, paste0(exp_path,"/pums_overcrowding_2021_V_",dateAsText,".csv"))

# 14.2 export VARS for OVERCROWDING
over_list <- c('RT',
               'PUMA',
               'ADJHSG',
               'ADJINC',
               'NP',
               'BDSP',
               'RMSP',
               'CPLT',
               'HHLDRAGEP',
               'HHLDRRAC1P',
               'HHLDRHISP',
               'HHT',
               'HHT2',
               'HINCP',
               'HUGCL',
               'HUPAC',
               'HUPAOC',
               'HUPARC',
               'LNGI',
               'MULTG',
               'NPF',
               'NPP',
               'NR',
               'NRC',
               'PARTNER',
               'PSF',
               'R18',
               'AGEP',
               'RELSHIPP',
               'RAC1P',
               'OC',
               'SFN',
               'SFR')
  
over_vars <- uniq_vars %>%
  filter(var_code %in% over_list)

write_csv(over_vars, paste0(exp_path,"/pums_overcrowding_variables_2021.csv"))
