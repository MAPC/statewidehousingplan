library(data.table)
library(tidyverse)
# root <- 'S:/Network Shares/DS Projects/'
root <- 'K:/DataServices/Projects/'
# root <- '//data-001/public/DataServices/Projects'
setwd(paste0(root,'Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs2/muni_',mid))

options(scipen = 999)

pums <- fread(paste0(root,'Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs2/muni_',mid,'/PUMS2019_',mid,'.csv'))

#Sets targeting files
#population by Age
#pa <- fread('../Pop_Age.csv')
tpa <- fread(paste0('Pop_Age_2010_muni_',mid,'.csv'))
pums.pa.baseline <- pums %>% 
  group_by(ageCAT6) %>% 
  summarise(BASELINE = sum(PWGTP)) %>% 
  mutate(ageCAT6 = case_when(
    ageCAT6 == 1 ~ "x == 1",
    ageCAT6 == 2 ~ "x == 2",
    ageCAT6 == 3 ~ "x == 3",
    ageCAT6 == 4 ~ "x == 4",
    ageCAT6 == 5 ~ "x == 5",
    ageCAT6 == 6 ~ "x == 6",
    ageCAT6 == 7 ~ "x == 7",
    ageCAT6 == 8 ~ "x == 8",
    ageCAT6 == 9 ~ "x == 9",
    ageCAT6 == 10 ~ "x == 10",
    ageCAT6 == 11 ~ "x == 11",
    ageCAT6 == 12 ~ "x == 12",
    ageCAT6 == 13 ~ "x == 13",
    ageCAT6 == 14 ~ "x == 14",
    ageCAT6 == 15 ~ "x == 15",
    ageCAT6 == 16 ~ "x == 16",
    ageCAT6 == 17 ~ "x == 17",
    ageCAT6 == 18 ~ "x == 18"
  ),
  INTER = BASELINE
  ) %>% 
  left_join(
    tpa,
    by = c("ageCAT6")
  ) %>% 
  select(
    -c(
      muni_id
    )
  ) %>% 
  relocate(
    TARGET,
    .before = INTER
  )
#setorder(pa,ageCAT6)
#setorder(tpa,ageCAT6)
#pa[,`:=`(TARGET=tpa$TARGET)]
#pa[,INTER:=BASELINE]
fwrite(pums.pa.baseline,'Pop_Age.csv')

#Age of Householder
#as <- fread('../AgeHHder.csv')
tas <- fread(paste0('AgeHHder_2010_muni_',mid,'.csv'))
pums.as.baseline <- pums %>% 
  filter(SPORDER == 1) %>% 
  group_by(ageCAT4) %>% 
  summarise(BASELINE = sum(WGTP)) %>% 
  mutate(
    ageCAT4 = case_when(
      ageCAT4 == 1 ~ "x == 1",
      ageCAT4 == 2 ~ "x == 2",
      ageCAT4 == 3 ~ "x == 3",
      ageCAT4 == 4 ~ "x == 4",
      ageCAT4 == 5 ~ "x == 5",
      ageCAT4 == 6 ~ "x == 6",
      ageCAT4 == 7 ~ "x == 7",
      ageCAT4 == 8 ~ "x == 8",
      ageCAT4 == 9 ~ "x == 9",
    ),
  INTER = BASELINE
  ) %>% 
  left_join(
    tas,
    by = c("ageCAT4")
  ) %>% 
  select(
    -c(
      muni_id
    )
  ) %>% 
  relocate(
    TARGET,
    .before = INTER
  ) %>% 
  na.omit()
#setorder(as,ageCAT4)
#setorder(tas,ageCAT4)
#as[,`:=`(TARGET=tas$TARGET)]
#as[,INTER:=BASELINE]
#print(sum(pums.as.baseline$TARGET))
fwrite(pums.as.baseline,'AgeHHder.csv')

#Household Type
#hh <- fread('../HHtype.csv')
thh <- fread(paste0('HHtype_2010_muni_',mid,'.csv'))
pums.hh.baseline <- pums %>% 
  filter(SPORDER == 1) %>% 
  group_by(ageCAT5, HHtype) %>% 
  summarise(BASELINE = sum(WGTP)) %>% 
  mutate(
    ageCAT5 = case_when(
      ageCAT5 == 1 ~ "x == 1",
      ageCAT5 == 2 ~ "x == 2",
    ),
    HHtype = case_when(
      HHtype == 1 ~ "x == 1",
      HHtype == 3 ~ "x == 3",
      HHtype == 5 ~ "x == 5"
    ),
    INTER = BASELINE
  ) %>% 
  left_join(
    thh,
    by = c("ageCAT5","HHtype")
  ) %>% 
  select(
    -c(
      muni_id
    )
  ) %>% 
  relocate(
    TARGET,
    .before = INTER
  ) %>% 
  na.omit()
#setorder(hh,ageCAT5,HHtype)
#setorder(thh,ageCAT5,HHtype)
#hh[,`:=`(TARGET=thh$TARGET)]
#hh[,INTER:=BASELINE]
fwrite(pums.hh.baseline,'HHtype.csv')

#Household Size
#hhs <- fread('../HHsize.csv')
thhs <- fread(paste0('HHsize_2010_muni_',mid,'.csv'))
pums.hhs.baseline <- pums %>% 
  filter(SPORDER == 1) %>% 
  group_by(HHsize) %>% 
  summarise(BASELINE = sum(WGTP)) %>% 
  mutate(
    HHsize = case_when(
      HHsize == 1 ~ "x == 1",
      HHsize == 2 ~ "x == 2",
      HHsize == 3 ~ "x == 3",
      HHsize == 4 ~ "x == 4",
      HHsize == 5 ~ "x == 5",
      HHsize == 6 ~ "x == 6",
      HHsize == 7 ~ "x == 7"
    ),
    INTER = BASELINE
  ) %>% 
  left_join(
    thhs,
    by = c("HHsize")
  ) %>% 
  select(
    -c(
      muni_id
    )
  ) %>% 
  relocate(
    TARGET,
    .before = INTER
  ) %>% 
  na.omit()
#setorder(hhs,HHsize)
#setorder(thhs,HHsize)
#hhs[,`:=`(TARGET=thhs$TARGET)]
#hhs[,INTER:=BASELINE]
fwrite(pums.hhs.baseline,'HHsize.csv')
