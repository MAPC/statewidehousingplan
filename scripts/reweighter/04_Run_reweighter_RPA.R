library(tidyverse)
library(data.table)
##get rid of scientific notation
options(scipen=999)

root <- 'S:/Network Shares/DS Projects/'
# root <- 'K:/DataServices/Projects/'
# root <- '//data-001/public/DataServices/Projects/'

runDIR <- paste0(root,'Current_Projects/Projections/Reweighter/Scripts/')

source(paste0(runDIR,'algo_hh.R'))

ms <-
  mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()

mpos <- sort(ms$mpo)

# Configuration Loop
for (mid in mpos){
  source(paste0(runDIR,'04a_read_config_RPA.R'))
  message(paste0('muni ',mid,' configuration complete'))
}


for (yr in c(2010,2020)){
  # Set Targets and run reweighter loop
  for (mid in mpos) {
    # thh <- read.csv(paste0(root,'Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_', yr, '_adj_V03.22.23.csv')) %>% select(-BASELINE, -INTER)
    thh <- read.csv(paste0('G:/Input_Files/AgeHHder_HHtype_', yr, '_adj_V03.22.23.csv')) %>% select(-BASELINE, -INTER)
    source(paste0(runDIR, '04b_set_targets_RPA.R'))
    message(paste0('muni ', mid, ' targets set'))
    source(paste0(runDIR, '04c_Manage_Scenario_RPA.R'))
    message(paste0('muni ', mid, ' reweighter complete'))
    mess <- data.table(txt = paste0(mid, ' reweighter complete'))
    fwrite(mess,
           paste0(runDIR, 'progress_logs/', mid, '_', yr, '_complete.txt'))
  }
}

for (yr in c(2030, 2040, 2050)) {
  # Set Targets and run reweighter loop
  for (mid in mpos) {
    # thh <- read.csv(paste0(root,'Current_Projects/Projections/Reweighter/Input_Files/AgeHHder_HHtype_', yr, '_V03.22.23.csv')) %>% select(-BASELINE, -INTER)
    thh <- read.csv(paste0('G:/Input_Files/AgeHHder_HHtype_', yr, '_V03.22.23.csv')) %>% select(-BASELINE, -INTER)
    source(paste0(runDIR, '04b_set_targets_RPA.R'))
    message(paste0('muni ', mid, ' targets set'))
    source(paste0(runDIR, '04c_Manage_Scenario_RPA.R'))
    message(paste0('muni ', mid, ' reweighter complete'))
    mess <- data.table(txt = paste0(mid, ' reweighter complete'))
    fwrite(mess,
           paste0(runDIR, 'progress_logs/', mid, '_', yr, '_complete.txt'))
  }
}
