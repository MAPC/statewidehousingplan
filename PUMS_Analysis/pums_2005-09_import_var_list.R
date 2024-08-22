#Set up libraries for the analysis
library(tidycensus)
library(tidyverse)
library(survey)
library(srvyr)
library(data.table)
library(mapcdatakeys)
library(janitor)


# lberman 2024-08-22

# PUMS data from tidycensus
# https://walker-data.com/tidycensus/articles/pums-data.html
# https://walker-data.com/tidycensus/reference/get_pums.html

# Housing units are uniquely identified by the SERIALNO variable
# Persons are uniquely identified by the SPORDER

# reconstructed full list of fields directly from tidycensus data for housing and persons
# crosswalk from all 2009 variables to 2021 (with column for subset of Overcrowding vars):
# pums_vars_from_data_2005-09.ods

# set output path
exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS"
#exp_path ="C:/Users/lberman/Downloads"



# PUMS metadata from tidycensus (only from 2017-2021)
# 1. get pums vars
# pums_vars_2009 <- pums_variables %>% 
#   filter(year == 2009, survey == "acs5")

# 2  set up vars list to retrieve
# var names: https://mapc365.sharepoint.com/:x:/s/DataServicesSP/EWFFCY8nSd9PpCQJh5XQlUYBfmT4WRxqWX4kR489r2uo3Q?rtime=GFzJ_jC23Eg
# use sheet 1


# note:  do not explicitly call the defaults
# default will always return SERIALNO, SPORDER, WGTP, PWGTP, and ST.

# 2.1  begin with all possible hh vars
var.list.hh  <- c('SERIALNO',
                  'ACR',
                  'ADJHSG',
                  'ADJINC',
                  'AGS',
                  'BDS',
                  'BLD',
                  'BUS',
                  'CONP',
                  'DIVISION',
                  'ELEP',
                  'FACRP',
                  'FAGSP',
                  'FBDSP',
                  'FBLDP',
                  'FBUSP',
                  'FCONP',
                  'FELEP',
                  'FES',
                  'FFSP',
                  'FFULP',
                  'FGASP',
                  'FHFLP',
                  'FINCP',
                  'FINSP',
                  'FKITP',
                  'FMHP',
                  'FMRGIP',
                  'FMRGP',
                  'FMRGTP',
                  'FMRGXP',
                  'FMVP',
                  'FPARC',
                  'FPLMP',
                  'FRMSP',
                  'FRNTMP',
                  'FRNTP',
                  'FS',
                  'FSMP',
                  'FSMXHP',
                  'FSMXSP',
                  'FTAXP',
                  'FTELP',
                  'FTENP',
                  'FULP',
                  'FVACSP',
                  'FVALP',
                  'FVEHP',
                  'FWATP',
                  'FYBLP',
                  'GASP',
                  'GRNTP',
                  'GRPIP',
                  'HFL',
                  'HHL',
                  'HHT',
                  'HINCP',
                  'HUGCL',
                  'HUPAC',
                  'HUPAOC',
                  'HUPARC',
                  'INSP',
                  'KIT',
                  'LNGI',
                  'MHP',
                  'MRGI',
                  'MRGP',
                  'MRGT',
                  'MRGX',
                  'MV',
                  'NOC',
                  'NP',
                  'NPF',
                  'NPP',
                  'NR',
                  'NRC',
                  'OCPIP',
                  'PARTNER',
                  'PLM',
                  'PSF',
                  'PUMA',
                  'R18',
                  'R60',
                  'R65',
                  'REGION',
                  'RESMODE',
                  'RMS',
                  'RNTM',
                  'RNTP',
                  'SMOCP',
                  'SMP',
                  'SMX',
                  'SRNT',
                  'ST',
                  'SVAL',
                  'TAXP',
                  'TEL',
                  'TEN',
                  'TYPE',
                  'VACS',
                  'VAL',
                  'VEH',
                  'WATP',
                  'WGTP',
                  'WGTP1',
                  'WGTP10',
                  'WGTP11',
                  'WGTP12',
                  'WGTP13',
                  'WGTP14',
                  'WGTP15',
                  'WGTP16',
                  'WGTP17',
                  'WGTP18',
                  'WGTP19',
                  'WGTP2',
                  'WGTP20',
                  'WGTP21',
                  'WGTP22',
                  'WGTP23',
                  'WGTP24',
                  'WGTP25',
                  'WGTP26',
                  'WGTP27',
                  'WGTP28',
                  'WGTP29',
                  'WGTP3',
                  'WGTP30',
                  'WGTP31',
                  'WGTP32',
                  'WGTP33',
                  'WGTP34',
                  'WGTP35',
                  'WGTP36',
                  'WGTP37',
                  'WGTP38',
                  'WGTP39',
                  'WGTP4',
                  'WGTP40',
                  'WGTP41',
                  'WGTP42',
                  'WGTP43',
                  'WGTP44',
                  'WGTP45',
                  'WGTP46',
                  'WGTP47',
                  'WGTP48',
                  'WGTP49',
                  'WGTP5',
                  'WGTP50',
                  'WGTP51',
                  'WGTP52',
                  'WGTP53',
                  'WGTP54',
                  'WGTP55',
                  'WGTP56',
                  'WGTP57',
                  'WGTP58',
                  'WGTP59',
                  'WGTP6',
                  'WGTP60',
                  'WGTP61',
                  'WGTP62',
                  'WGTP63',
                  'WGTP64',
                  'WGTP65',
                  'WGTP66',
                  'WGTP67',
                  'WGTP68',
                  'WGTP69',
                  'WGTP7',
                  'WGTP70',
                  'WGTP71',
                  'WGTP72',
                  'WGTP73',
                  'WGTP74',
                  'WGTP75',
                  'WGTP76',
                  'WGTP77',
                  'WGTP78',
                  'WGTP79',
                  'WGTP8',
                  'WGTP80',
                  'WGTP9',
                  'WIF',
                  'WKEXREL',
                  'WORKSTAT',
                  'YBL')

var.list.per  <- c('SERIALNO',
                 'SPORDER',
                 'ADJINC',
                 'AGEP',
                 'ANC',
                 'ANC1P',
                 'ANC2P',
                 'CIT',
                 'COW',
                 'DECADE',
                 'DRIVESP',
                 'ENG',
                 'ESP',
                 'ESR',
                 'FAGEP',
                 'FANCP',
                 'FCITP',
                 'FCOWP',
                 'FENGP',
                 'FER',
                 'FESRP',
                 'FFERP',
                 'FGCLP',
                 'FGCMP',
                 'FGCRP',
                 'FHISP',
                 'FINDP',
                 'FINTP',
                 'FJWDP',
                 'FJWMNP',
                 'FJWRIP',
                 'FJWTRP',
                 'FLANP',
                 'FLANXP',
                 'FMARP',
                 'FMIGP',
                 'FMIGSP',
                 'FMILPP',
                 'FMILSP',
                 'FOCCP',
                 'FOIP',
                 'FPAP',
                 'FPOBP',
                 'FPOWSP',
                 'FRACP',
                 'FRELP',
                 'FRETP',
                 'FSCHGP',
                 'FSCHLP',
                 'FSCHP',
                 'FSEMP',
                 'FSEXP',
                 'FSSIP',
                 'FSSP',
                 'FWAGP',
                 'FWKHP',
                 'FWKLP',
                 'FWKWP',
                 'FYOEP',
                 'GCL',
                 'GCM',
                 'GCR',
                 'HISP',
                 'INDP',
                 'INTP',
                 'JWAP',
                 'JWDP',
                 'JWMNP',
                 'JWRIP',
                 'JWTR',
                 'LANP',
                 'LANX',
                 'MAR',
                 'MIG',
                 'MIGPUMA',
                 'MIGSP',
                 'MIL',
                 'MLPA',
                 'MLPB',
                 'MLPC',
                 'MLPD',
                 'MLPE',
                 'MLPF',
                 'MLPG',
                 'MLPH',
                 'MLPI',
                 'MLPJ',
                 'MLPK',
                 'MSP',
                 'NAICSP',
                 'NATIVITY',
                 'NWAB',
                 'NWAV',
                 'NWLA',
                 'NWLK',
                 'NWRE',
                 'OC',
                 'OCCP',
                 'OIP',
                 'PAOC',
                 'PAP',
                 'PERNP',
                 'PINCP',
                 'POBP',
                 'POVPIP',
                 'POWPUMA',
                 'POWSP',
                 'PUMA',
                 'PWGTP',
                 'PWGTP1',
                 'PWGTP10',
                 'PWGTP11',
                 'PWGTP12',
                 'PWGTP13',
                 'PWGTP14',
                 'PWGTP15',
                 'PWGTP16',
                 'PWGTP17',
                 'PWGTP18',
                 'PWGTP19',
                 'PWGTP2',
                 'PWGTP20',
                 'PWGTP21',
                 'PWGTP22',
                 'PWGTP23',
                 'PWGTP24',
                 'PWGTP25',
                 'PWGTP26',
                 'PWGTP27',
                 'PWGTP28',
                 'PWGTP29',
                 'PWGTP3',
                 'PWGTP30',
                 'PWGTP31',
                 'PWGTP32',
                 'PWGTP33',
                 'PWGTP34',
                 'PWGTP35',
                 'PWGTP36',
                 'PWGTP37',
                 'PWGTP38',
                 'PWGTP39',
                 'PWGTP4',
                 'PWGTP40',
                 'PWGTP41',
                 'PWGTP42',
                 'PWGTP43',
                 'PWGTP44',
                 'PWGTP45',
                 'PWGTP46',
                 'PWGTP47',
                 'PWGTP48',
                 'PWGTP49',
                 'PWGTP5',
                 'PWGTP50',
                 'PWGTP51',
                 'PWGTP52',
                 'PWGTP53',
                 'PWGTP54',
                 'PWGTP55',
                 'PWGTP56',
                 'PWGTP57',
                 'PWGTP58',
                 'PWGTP59',
                 'PWGTP6',
                 'PWGTP60',
                 'PWGTP61',
                 'PWGTP62',
                 'PWGTP63',
                 'PWGTP64',
                 'PWGTP65',
                 'PWGTP66',
                 'PWGTP67',
                 'PWGTP68',
                 'PWGTP69',
                 'PWGTP7',
                 'PWGTP70',
                 'PWGTP71',
                 'PWGTP72',
                 'PWGTP73',
                 'PWGTP74',
                 'PWGTP75',
                 'PWGTP76',
                 'PWGTP77',
                 'PWGTP78',
                 'PWGTP79',
                 'PWGTP8',
                 'PWGTP80',
                 'PWGTP9',
                 'QTRBIR',
                 'RAC1P',
                 'RAC2P',
                 'RAC3P',
                 'RACAIAN',
                 'RACASN',
                 'RACBLK',
                 'RACNHPI',
                 'RACNUM',
                 'RACSOR',
                 'RACWHT',
                 'RC',
                 'REL',
                 'RETP',
                 'SCH',
                 'SCHG',
                 'SCHL',
                 'SEMP',
                 'SEX',
                 'SFN',
                 'SFR',
                 'SOCP',
                 'SSIP',
                 'SSP',
                 'ST',
                 'VPS',
                 'WAGP',
                 'WAOB',
                 'WKHP',
                 'WKL',
                 'WKW',
                 'YOEP')


######
# 3 retrieve data function
# WGTP = housing-unit weight; PWGTP person weight
#  variables available from 2017-

pums_2009_hh_raw <- get_pums(
  variables = var.list.hh,
  state = "MA",
  survey = "acs5",
  year = 2009,
)

pums_2009_per_raw <- get_pums(
  variables = var.list.per,
  state = "MA",
  survey = "acs5",
  year = 2009
)


# 4 find housing cols to drop from person df 

# 4.1 get the names of cols as df
pers_list <- names(pums_2009_per_raw)
person <- data.frame(pers_list)
person <- person %>%
  mutate(pers_join = pers_list)

hou_list <- names(pums_2009_hh_raw)
housing <- data.frame(hou_list)
housing <- housing %>%
  mutate(join = hou_list)

# 4.2 join the two lists of cols
join_list <- person %>%
  full_join(.,
            housing,
            by = c('pers_list' = 'join'))

# 4.3  filter out the missing hh cols not found in person df
cols_uniq_to_pers <- join_list %>% 
  filter(is.na(hou_list)) 

# 4.4 make sure the list has SERIALNO, SPORDER
cols_uniq_to_pers[nrow(cols_uniq_to_pers) + 1,] = c("SERIALNO","SERIALNO","")
cols_uniq_to_pers[nrow(cols_uniq_to_pers) + 1,] = c("SPORDER","SPORDER","")

# 4.4  trim down to columns uniq to person df
trim_pums_2009_per <- pums_2009_per_raw[as.character(cols_uniq_to_pers$pers_list)]


# 5 find person cols to remove from housing df 
# 5.1 join the two lists of cols
join_hh_list <- housing %>%
  full_join(.,
            person,
            by = c('join' = 'pers_join'))

# 5.2  trim down to columns uniq to hh df
cols_uniq_to_hh <- join_hh_list %>% 
  filter(!is.na(hou_list)) %>% 
  select(c(hou_list))

# 5.3  remove all of the person specific cols from hh df
trim_pums_2009_hh <- pums_2009_hh_raw[as.character(cols_uniq_to_hh$hou_list)]


# 6 create uniq id for each hh + person [SERIALNO + SPORDER]
pums_2009_person <- trim_pums_2009_per %>% 
  mutate(uniq_obs = paste0(SERIALNO,"_",SPORDER))

pums_2009_hh <- trim_pums_2009_hh %>% 
  mutate(uniq_obs = paste0(SERIALNO,"_",SPORDER))


# 7 before join, trim off the serialno and sporder from person to avoid .x .y after join
pums_2009_person <- pums_2009_person %>% 
  select(-c(SERIALNO,SPORDER))


# 8 full join of all columns on the uniq_obs
pums_2009_join <- pums_2009_hh %>%
  full_join(.,
            pums_2009_person,
            by = c('uniq_obs' = 'uniq_obs'))


# 9 WRITE TO CSV
# export  version = V_yyyy-mm-dd
currentDate <- Sys.Date()
dateAsText <- format(currentDate, "%Y-%m-%d")
write_csv(pums_2009_join, paste0(exp_path,"/pums_2009_all_vars_",dateAsText,".csv"))


#10 SUBSET OVERCROWDING including only directly mappable variables between 2009 to 2021

# could not be mapped:  CPLT,HHLDRAGEP,HHLDRRAC1P,HHT2,MULTG,RT
# altered spelling:
# BDSP = BDS
# HHLDRHISP = HISP
# RELSHIPP = REL
# RMSP = RMS 
# TYPEHUGQ  =  TYPE

pums_overcrowding_2009 <- pums_2009_join %>% 
  select(c(uniq_obs, SPORDER, SERIALNO, ST, WGTP, PWGTP, PUMA, ADJHSG, ADJINC, NP, TYPE, BDS, RMS, HISP, HHT,  HINCP, HUGCL, HUPAC, HUPAOC, HUPARC, LNGI, NPF, NPP, NR, NRC, PARTNER, PSF, R18, AGEP, REL, RAC1P, OC, SFN, SFR))


# 11 export OVERCROWDING
write_csv(pums_overcrowding_2009, paste0(exp_path,"/pums_overcrowding_2009_V_",dateAsText,".csv"))



