#-------------------------------------------------------------------------------
#Set-up
#Load libraries
pacman::p_load(tidyverse,
               tidycensus,
               sf,
               data.table)

# Disabling Scientific Notation
options(scipen = 999)
# root <- 'K:/DataServices/Projects/'
root <- 'S:/Network Shares/K Drive/DataServices/Projects/'

#Set directories
baseDIR = paste0(root,'Current_Projects/Projections/Data/UrbanSim Outputs')
setwd(baseDIR)

mrun <- 'run_131'
srun <- 'state_run_97'

#MAPC Data keys - for each muni
munis <- mapcdatakeys::all_muni_data_keys 


mp <- fread(paste0('MAPC/',mrun,'/',mrun,'_municipal_aggregation_2010_2049_v1.csv'))
swm <- fread(paste0('Statewide/',srun,'/',srun,'_municipal_aggregation_2010_2049_v1.csv'))
swm <- swm[!muni_id %in% sun(mp$muni_id)]

urb <- rbind(mp,swm)
urb[persons==1,hhtype:='single']
urb[persons>1 & children==0, hhtype:='multiple adults no children']
urb[persons>1 & children>0, hhtype:='multiple adults with children']

muni.urb <- urb[,lapply(.SD,sum),.(year,muni_id,muni_name,rpa_acr,cmtyp08,hhtype,income_grp),.SDcols='households']
rpa.urb <- urb[,lapply(.SD,sum),.(year,rpa_acr,cmtyp08,hhtype,income_grp),.SDcols='households']
st.urb <- urb[,lapply(.SD,sum),.(year,cmtyp08,hhtype,income_grp),.SDcols='households']

fwrite(muni.urb,'households_by_income_by_type_by_muni_2010_2050.csv')
fwrite(rpa.urb,'households_by_income_by_type_by_community_type_by_RPA_2010_2050.csv')
fwrite(st.urb,'households_by_income_by_type_by_community_type_state_totals_2010_2050.csv')

##############################################################
# Households by type and income   #############################
################################################################

# 2020 PL-92 households with/without children (under 18)

v20 <- load_variables(2020,'pl') %>% setDT()
v20[grepl('voting',label)]



# ACS B19131
# and HH w/ children under 18

#households by children/no children by income from acs5_geography
#compare to 2010 and 2019 urbansim

years <- c(2019)
dt <- data.table()

# for (yr in years){
v1 <- load_variables(yr,'acs5') %>% setDT()
nf <- v1[concept == paste0("NONFAMILY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ",yr," INFLATION-ADJUSTED DOLLARS)"),unique(name)]
famtype <- v1[concept == paste0("FAMILY TYPE BY PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY INCOME IN THE PAST 12 MONTHS (IN ",yr," INFLATION-ADJUSTED DOLLARS)"),unique(name)]

munis <- mapcdatakeys::all_muni_data_keys %>% 
  select(muni_id,paste0('cosub_5y',substr(yr,3,4))) %>% 
  mutate(GEOID=paste0('cosub_5y',substr(yr,3,4)))

setnames(v1,'name','variable')

# Nonfamily household income 
nf.inc <- get_acs(state='ma',
                  geography = 'county subdivision',
                  year=yr,
                  survey='acs5',
                  variables=nf) %>% 
  left_join(v1,by='variable') %>% 
  left_join(munis,by='GEOID') %>% 
  filter(!is.na(muni_id)) %>% 
  select(muni_id,variable,label,estimate) %>%
  filter(label!="Estimate!!Total") %>% 
  # mutate(label = gsub('Estimate!!Total!!','',label),
  #        HHT2 = 3) %>%
  mutate(label = gsub('Estimate!!Total!!','',label),        # ALTERNATE CONFIG - combine all no-child HHs
         HHT2 = 2,
         year = yr) %>%
  setDT()

# Family household income by family type and presence of own children
fam.inc <- get_acs(state='ma',
                   geography = 'county subdivision',
                   year=yr,
                   survey='acs5',
                   variables=famtype) %>% 
  left_join(v1,by='variable') %>% 
  left_join(munis,by='GEOID') %>% 
  filter(!is.na(muni_id)) %>% 
  select(muni_id,variable,label,estimate) %>% 
  filter(label!="Estimate!!Total") %>% 
  mutate(label = gsub('Estimate!!Total!!','',label),
         year = yr) %>% 
  mutate(HHT2 = case_when(
    grepl('With own children',label) ~ 1,
    grepl('No own children',label) ~ 1,
  )) %>% 
  filter(!is.na(HHT2)) %>% 
  tidyr::separate_wider_delim(names_sep = '_', label,delim="!!", too_few = 'align_end') %>% 
  setDT()


# ACS total children in households

ch <- v1[concept == paste0("RELATIONSHIP TO HOUSEHOLDER FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS"),unique(variable)]
hh.ch <- get_acs(state='ma',
                   geography = 'county subdivision',
                   year=yr,
                   survey='acs5',
                   variables=ch) %>% 
  left_join(v1,by='variable') %>% 
  left_join(munis,by='GEOID') %>% 
  filter(!is.na(muni_id)) %>% 
  select(muni_id,variable,label,estimate)


# Recode and summarize to income groups closest to UrbanSim / Control total groups

nf.inc[variable %in% c('B19201_002','B19201_003','B19201_004','B19201_005','B19201_006','B19201_007'), HHINC2013:= 1]
nf.inc[variable %in% c('B19201_008','B19201_009','B19201_010','B19201_011','B19201_012'), HHINC2013:= 2]
nf.inc[variable %in% c('B19201_013','B19201_014'), HHINC2013:= 3]
nf.inc[variable %in% c('B19201_015','B19201_016'), HHINC2013:= 4]
nf.inc[variable=='B19201_017', HHINC2013:=5]
nf.inc <- nf.inc %>% select(year,muni_id,HHT2,HHINC2013,estimate)

fam.inc <- fam.inc %>% 
  mutate(HHINC2013 = case_when(
    label_4 %in% c("Less than $10,000","$10,000 to $14,999","$15,000 to $19,999","$20,000 to $24,999","$25,000 to $29,999","$30,000 to $34,999") ~ 1,
    label_4 %in% c("$35,000 to $39,999","$40,000 to $44,999","$45,000 to $49,999","$50,000 to $59,999","$60,000 to $74,999") ~ 2,
    label_4 %in% c("$75,000 to $99,999","$100,000 to $124,999") ~ 3,
    label_4 %in% c("$125,000 to $149,999","$150,000 to $199,999") ~ 4,
    label_4 == "$200,000 or more" ~ 5)
  ) %>% 
  filter(!is.na(HHINC2013)) %>% 
  select(year, muni_id,HHT2,HHINC2013,estimate)


comb <- rbind(fam.inc,nf.inc)
dt <- rbind(dt,comb)
}

# Combine, sum by HHT2 and income group, and export by muni_id
hhinc.sum <-
  dt[, lapply(.SD, sum), .(year, muni_id, HHT2, HHINC2013), .SDcols = 'estimate']

tot.hh <- fread('HHsize_2010_all_munis.csv')
tot.hh <- tot.hh[,lapply(.SD,sum),muni_id,.SDcols='TARGET']
setkey(tot.hh,muni_id)
setkey(hhinc.sum,muni_id)

hhinc.sum <- tot.hh[hhinc.sum]
hhinc.sum[,acs_sum:=sum(estimate),muni_id]
hhinc.sum[,frac:=estimate/acs_sum]
hhinc.sum[,adj:=round(frac*TARGET)]
hhinc.sum[,newsum:=sum(adj),muni_id]
hhinc.sum[,diff:=TARGET-newsum]
hhinc.sum[HHT2==1 & HHINC2013==3,adj:=adj+diff]
hhinc.sum[,newsum:=sum(adj),muni_id]
hhinc.sum[,diff:=TARGET-newsum]
hhinc.sum[,unique(diff)]
hhinc.sum <- hhinc.sum[,.(muni_id,HHT2,HHINC2013,adj)]
setnames(hhinc.sum,c('muni_id','HHT2','HHINC2013','TARGET'))

hhinc.sum[,HHT2:=paste0('x == ',HHT2)]
hhinc.sum[,HHINC2013:=paste0('x == ',HHINC2013)]


