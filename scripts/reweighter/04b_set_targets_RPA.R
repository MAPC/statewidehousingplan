library(data.table)
library(readxl)
library(tidyverse)
library(magrittr)

# root <- 'S:/Network Shares/DS Projects/Current_Projects/'
# root <- '//data-001/public/DataServices/Projects/Current_Projects/'
setwd(paste0(
  root,
  "Current_Projects/Projections/Reweighter/Input_Files/"
))
setwd(paste0('G:/Input_Files/'))
# Target File

tmp <- fread('AgeHHder_HHtype_template.csv')

mtp <- tmp[RPA == mid]
pums <- fread(paste0(mid, '/PUMS2019_', mid, '.csv'))
target <- pums %>%
  filter(SPORDER == 1) %>%
  group_by(ageCAT3, HHtype) %>%
  summarise(BASELINE = sum(WGTP)) %>%
  mutate(
    ageCAT3 = case_when(
      ageCAT3 == 1 ~ "x == 1",
      ageCAT3 == 2 ~ "x == 2",
      ageCAT3 == 3 ~ "x == 3",
      ageCAT3 == 4 ~ "x == 4",
      ageCAT3 == 5 ~ "x == 5",
      ageCAT3 == 6 ~ "x == 6",
      ageCAT3 == 7 ~ "x == 7",
      ageCAT3 == 8 ~ "x == 8",
      ageCAT3 == 9 ~ "x == 9",
      ageCAT3 == 10 ~ "x == 10",
      ageCAT3 == 11 ~ "x == 11",
      ageCAT3 == 12 ~ "x == 12",
      ageCAT3 == 13 ~ "x == 13",
      ageCAT3 == 14 ~ "x == 14",
      ageCAT3 == 15 ~ "x == 15",
      ageCAT3 == 16 ~ "x == 16",
      ageCAT3 == 17 ~ "x == 17",
      ageCAT3 == 18 ~ "x == 18"
    ),
    HHtype = case_when(HHtype == 1 ~ "x == 1",
                       HHtype == 3 ~ "x == 3",
                       HHtype == 5 ~ "x == 5"),
    RPA = mid
  ) %>%
  right_join(mtp, by = c('RPA', 'ageCAT3', 'HHtype')) %>%
  left_join(thh, by = c('RPA', 'ageCAT3', 'HHtype')) %>%
  setDT()

target[is.na(BASELINE), BASELINE := 0]
target[is.na(TARGET), TARGET := 0]
target[, INTER := BASELINE]
target[, RPA := NULL]
setorder(target,ageCAT3,HHtype)
write.csv(target, paste0(mid, '/AgeHHder_HHtype.csv'))
