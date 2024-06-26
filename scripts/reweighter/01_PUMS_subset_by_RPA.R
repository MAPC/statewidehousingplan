library(tidyverse)
library(tidycensus)
library(data.table)

##get rid of scientific notation
options(scipen = 999)

root <- 'S:/Network Shares/DS Projects/'
# root <- 'K:/DataServices/Projects/'
# root <- '//data-001/public/DataServices/Projects/'

runDIR <-
  paste0(root,
         'Current_Projects/Projections/Reweighter/Input_Files/')
setwd(runDIR)

# List of RPA names
mpos <-
  unlist(c(mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()))

# PUMA crosswalk
xw <- fread('ma_muni_puma10_join.csv')
xw <- xw[, .(TOWN_ID, PUMACE10)]
setnames(xw, c('muni_id', 'PUMA'))
xw <- xw[, .(muni_id, PUMA)]
mxw <-
  mapcdatakeys::all_muni_data_keys %>% select(muni_id, rpa_acr, mpo) %>% left_join(xw, by =
                                                                                     'muni_id') %>% select(-muni_id) %>% unique() %>% setDT()

mapc.pumas <- mxw[rpa_acr == 'MAPC', unique(PUMA)]
swm.pumas <- mxw[mpo != 'MAPC', .(mpo, PUMA)] %>% unique()


# PUMS 2019 data
pums <- fread('PUMS2019_formatted_20230322.csv')


# MAPC 101 subset
fwrite(pums[PUMA %in% mapc.pumas], paste0('MAPC/PUMS2019_MAPC.csv'))
message(paste0('MAPC 101 PUMS export complete'))


# SWM join PUMA and export subsets
for (m in unique(swm.pumas$mpo)) {
  pums %>%
    left_join(swm.pumas, by = 'PUMA') %>%
    filter(mpo == m) %>%
    fwrite(paste0(m, '/PUMS2019_', m, '.csv'))
  message(paste0(m, ' PUMS export complete'))
}
