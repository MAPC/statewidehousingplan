library(tidyverse)
library(data.table)
##get rid of scientific notation
options(scipen=999)

root <- 'S:/Network Shares/K Drive/DataServices/Projects/'
# root <- 'K:/DataServices/Projects/'
# root <- '//data-001/public/DataServices/Projects/'

setwd(paste0(root,'Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/reweighter_scripts/'))

source('algo_hh.R')

ms <-
  mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()
mpos <- sort(ms$mpo)

scen <- 'Baseline'

# Configuration Loop
for (mid in mpos){
  source(paste0('04a_read_config_RPA.R'))
  message(paste0(mid,' configuration complete'))
}


for (yr in c(2020,2030,2035,2040,2045,2050)){
  # Set Targets and run reweighter loop
  for (mid in mpos) {
    thh <- read.csv(paste0('Input_Files/',scen,'/AgeHHder_HHtype_', yr, '_', scen,'.csv')) %>% select(-BASELINE, -INTER)
    source(paste0(runDIR, '04b_set_targets_RPA.R'))
    message(paste0('muni ', mid, ' targets set'))
    source(paste0(runDIR, '04c_Manage_Scenario_RPA.R'))
    message(paste0('muni ', mid, ' reweighter complete'))
    mess <- data.table(txt = paste0(mid, ' reweighter complete'))
    fwrite(mess,
           paste0(runDIR, 'progress_logs/', mid, '_', yr, '_complete.txt'))
  }
}
