# Section 1: Title, Author, Edit, Input/Output, and Other Info ---- 

# Title:              LaborForceProjections_2050
# Authors:            Sarah Philbrick and Taylor Perez
# Editors:            Sam Rosenberg
# Program:            RStudio R Script

# Start Date:         04-19-2017
# Last Edit Date:     07-21-2021

# Purpose:            Combine rate files and HH population to create household projections for future years

# Input(s):           Rate Script Output
# Output(s):          Labor Force by Education Level, Age, and Sex

# Style Guide:        Refer to "Google's R Style Guide"
#                     Refer to "tidyverse style guide"
#                     Check using lintr()
#                     If something violates Style Guide, change it.
# Legibility:         "Ctrl + Shift + A" to reformat, OR
#                     "Code > Reformat Code"
# Documentation:      See Section 9: Documentation for Example
#                     Add new documentation at end of every session

# Section 2: Package Requirements and Global Options ----

# NOTE: DO NOT ADD RETIRED PACKAGES (e.g., plyr).

# data.table
library(data.table)

# tidyverse
library(tidyverse)

# Section 3: Data Inputs ----

# Setting paths
# in_path_rates <-
#   "K:/DS_Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/"
# in_path_census <-
#   "K:/DS_Projects/Current_Projects/Projections_2050/Data/Tabular/Output/Households_2050Update_unadj/"
# out_path <-
#   "K:/DS_Projects/Current_Projects/Projections_2050/Data/Tabular/Output/Households_2050Update_adj/"
# 
# in_path_rates <-
#   "S:/Network Shares/DS_Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/"
# in_path_census <-
#   "S:/Network Shares/Current_Projects/Projections_2050/Data/Tabular/Output/Households_2050Update_unadj/"
# out_path <-
#   "S:/Network Shares/Current_Projects/Projections_2050/Data/Tabular/Output/Households_2050Update_adj/"

setwd('K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data')

in_path_rates <- 'K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/'
in_path_census <- "K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Output/Households_2050Update_unadj/"
out_path <- 'K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/output_data/'

# Creating list of file names
# NOTE: To include more scenarios, add "_V7" after the "*" in "*_HHPop_101"
hhdpop_files_dir <-
  list.files(
    path = out_path,
    pattern = "*_HHPop_101",
    full.names = TRUE,
    recursive = FALSE
  )

# Creating list of data frames
hhdpop_files_raw <- lapply(hhdpop_files_dir, function(x)
  read.csv(x))

# Setting names for list of files
names(hhdpop_files_raw) <- basename(hhdpop_files_dir)

# Creating list of HHder file names 2008-2012
HHder_files_dir <-
  list.files(
    path = in_path_rates,
    pattern = "^HHder_HHtype_.*\\.*_0812.csv$",
    full.names = TRUE,
    recursive = FALSE
  )

# Creating list of HHder data frames 2008-2012
HHder_files_raw <-
  lapply(HHder_files_dir, function(x) {
    read.csv(x)
  })

# Setting names for LFPR list elements
names(HHder_files_raw) <- basename(HHder_files_dir)


# Household baseline data (Census 2010 Estimates)
census_2010_raw <-
  read.csv(paste0(in_path_census, "Households 2010/HHds_RPA_2010.csv"))


# Baseline data (Same for every scenario)
# NOTE: Currently uses 2010 data, change when Census 2020 data is available
files_2010_dir <-
  list.files(
    path = in_path_census,
    pattern = "^PopbyHHtype2010_.*\\.*_V2_101.csv$",
    full.names = TRUE,
    recursive = FALSE
  )

files_2010_raw <-
  lapply(files_2010_dir, function(x) {
    read.csv(x)
  })

names(files_2010_raw) <- basename(files_2010_dir)

# Section 4: Cleaning Data ----

# Number of RPAs
nRPA <- 13

# Creating data frame to calculate all RPAs
HHder_files <-
  
  # Removing index value
  lapply(HHder_files_raw, function(x)
    select(x, -c(X, nothhderchild, nothhdernochild))) %>%
  
  # Replacing all NA values with 0
  lapply(function(x) {
    replace(x, is.na(x), 0)
  }) %>%
  
  # Multiplying all values by 0.01
  lapply(function(x)
    transmute(x, across(everything(), function(x)
      x * 0.01))) %>%
  
  # Adding column containing list element ID
  imap(function(x, name) {
    mutate(x, ID = name,
           AgeC = 4:18)
  }) %>%
  
  # Separating column values into RPA name ("ID") and age category
  lapply(function(x)
    separate(
      x,
      col = ID,
      into = c("RATE", "HHType", "ID", "YEAR"),
      sep = "_"
    )) %>%
  
  # Removing column "Type" (e.g., "acs2.csv")
  lapply(function(x)
    select(x,-c("RATE", "HHType", "YEAR"))) %>%
  
  # Binding data into one data frame
  bind_rows() %>%
  
  # Mutating new value to replicate rows
  mutate(z = 4) %>%
  
  # Replicating rows
  uncount(z) %>%
  
  # Creating new column with prediction years
  mutate(PRED_YEAR = rep(seq(2020, 2050, 10), length.out = (60 * nRPA)),
         PRED_YEAR = as.character(PRED_YEAR))

# For each data frame in list, creating column matching element name
hhdpop_files <-
  
  # Filtering out first three age categories
  lapply(hhdpop_files_raw, function(x)
    x[x$AgeC %in% c("4",
                    "5",
                    "6",
                    "7",
                    "8",
                    "9",
                    "10",
                    "11",
                    "12",
                    "13",
                    "14",
                    "15",
                    "16",
                    "17",
                    "18"),]) %>%
  
  # Adding column containing list element ID
  imap(function(x, name) {
    mutate(x, ID = name)
  }) %>%
  
  # Separating column values into RPA name ("ID") and age category
  lapply(function(x)
    separate(
      x,
      col = ID,
      into = c("ID", "PRED_YEAR", "SCENARIO", "Type1", "Type2"),
      sep = "_"
    )) %>%
  
  # Keeping certain columns
  lapply(function(x)
    select(x, c(
      "AgeC", "ID", "PRED_YEAR", "SCENARIO", "HH"
    ))) %>%
  
  # Binding all data frames
  bind_rows()

# Cleaning Census 2010 estimates for each RPA
census_2010 <-
  
  # Mutating age categories
  mutate(census_2010_raw, AgeCat = 4:18) %>%
  
  # Reformatting data
  gather(census_2010_raw, "HH_baseline",-AgeCat) %>% #Should switch to pivot_wider.
  
  # Renaming columns to match
  rename("AgeC" = "AgeCat", "ID" = "census_2010_raw")

# Cleaning Scenario Predictions from 2010
files_2010 <- files_2010_raw %>%
  
  # Adding column containing list element ID
  imap(function(x, name) {
    mutate(x, ID = name,
           AgeC = 4:18)
  }) %>%
  
  # Separating column values into RPA name ("ID") and age category
  lapply(function(x)
    separate(
      x,
      col = ID,
      into = c("Type_YR", "ID", "SCENARIO", "Type"),
      sep = "_"
    )) %>%
  
  # Removing extra columns
  lapply(function(x)
    select(x, c(
      "AgeC", "ID", "hhderchild", "hhdernochild", "single"
    ))) %>%
  
  # Binding all data frames
  bind_rows()


# Section 5: Calculating Adjustment Factor ----

# Joining Census 2010 Estimates with Scenario Predictions
HH_factors <- left_join(files_2010, census_2010) %>%
  
  # Mutating new calculated columns
  mutate(
    # Calculating sum of households from scenario predictions
    hh_sum = hhderchild + hhdernochild + single,
    
    # Calculating baseline by multiplying scenario predictions by Census 2010
    child_baseline = (hhderchild / hh_sum) * HH_baseline,
    nochild_baseline = (hhdernochild / hh_sum) * HH_baseline,
    single_baseline = (single / hh_sum) * HH_baseline,
    
    # Calculating difference between scenario predictions and baseline
    child_diff = hhderchild - child_baseline,
    nochild_diff = hhdernochild - nochild_baseline,
    single_diff = single - single_baseline
  ) %>%
  
  # Selecting columns used for adjusted projections
  select(
    AgeC,
    ID,
    child_diff,
    nochild_diff,
    single_diff,
    child_baseline,
    nochild_baseline,
    single_baseline
  )



# Section 6: Unadjusted Projections ----
HH_unadj <-
  
  # Joining household data frames
  left_join(hhdpop_files, HHder_files) %>%
  
  # Mutating new values
  mutate(
    hhderchild_unadj = HH * hhderchild,
    hhdernochild_unadj = HH * hhdernochild,
    single_unadj = HH * single
  ) %>%
  
  # Selecting columns for output
  select(AgeC,
         ID,
         SCENARIO,
         PRED_YEAR,
         hhderchild_unadj,
         hhdernochild_unadj,
         single_unadj)





# Section 7: Adjusted Projections ----

# HH_adj <-
#   
#   # Joining all data frames
#   left_join(HH_unadj, HH_factors) %>%
#   
#   # Calculating adjusted values
#   mutate(
#     hhderchild_adj = hhderchild_unadj - child_diff,
#     
#     hhdernochild_adj = hhdernochild_unadj - nochild_diff,
#     
#     single_adj = single_unadj - single_diff
#   ) %>%
#   
#   # Selecting columns for output
#   select(AgeC,
#          ID,
#          PRED_YEAR,
#          SCENARIO,
#          hhderchild_adj,
#          hhdernochild_adj,
#          single_adj)


# Section 7b: Adjusted Projections ---- adjust households to match total occupied housing units by RPA in Census 2020 PL94

input_path_Census2020 <-
  "K:/DataServices/Datasets/U.S. Census and Demographics/Census 2020/Data/Processed/"
census_2020_raw <-
  fread(paste0(input_path_Census2020, "hu_occ_vac_muni.csv"))
census_2020 <- census_2020_raw %>%
  select(muni_id, tot_hu, hu_occ)
setnames(census_2020, c('muni_id', 'units_2020_C', 'hh_2020_C'))
setkey(census_2020, muni_id)

muni_keys <-
  fread(
    "K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/LODES/tabular_datakeys_muni351.csv"
  )
muni_keys <- muni_keys[, .(muni_id, rpa_name)]
setkey(muni_keys, rpa_name)
rpa_key <- fread('20190821_rpa-key.csv')
setnames(rpa_key, 'rpa', 'rpa_name')
setkey(rpa_key, rpa_name)
muni_keys <- rpa_key[muni_keys]
muni_keys <- muni_keys[, .(muni_id, rpa_abr)]
setkey(muni_keys, muni_id)

census_2020 <- muni_keys[census_2020]

# Adjust rpa name for rebel towns
census_2020[is.na(rpa_abr), rpa_abr := 'MAPC']
census_2020 <-
  census_2020[, lapply(.SD, sum), rpa_abr, .SDcols = c('hh_2020_C')]
setnames(census_2020, 'rpa_abr', 'ID')
setkey(census_2020, ID)

HH20 <- HH_unadj[HH_unadj$PRED_YEAR == 2020, ]
setDT(HH20)
setkey(HH20, ID)

HH20_adj <- census_2020[HH20]
HH20_adj[, totHH := sum(hhderchild_unadj) + sum(hhdernochild_unadj) + sum(single_unadj), .(ID, SCENARIO, PRED_YEAR)]

# Calculating adjusted values for 2020 data
HH20_adj <- HH20_adj %>%
  mutate(
    hhderchild_adj = hhderchild_unadj / totHH * hh_2020_C,
    
    hhdernochild_adj = hhdernochild_unadj / totHH * hh_2020_C,
    
    single_adj = single_unadj / totHH * hh_2020_C
  ) %>%
  select(AgeC,
         ID,
         PRED_YEAR,
         SCENARIO,
         hhderchild_adj,
         hhdernochild_adj,
         single_adj)

# Joining all data frames

HH_adj <-
  left_join(HH_unadj, HH_factors) %>%
  filter(PRED_YEAR != 2020) %>%
  # Calculating adjusted values for non-2020 data
  mutate(
    hhderchild_adj = hhderchild_unadj - child_diff,
    
    hhdernochild_adj = hhdernochild_unadj - nochild_diff,
    
    single_adj = single_unadj - single_diff
  ) %>%
  
  # Selecting columns for output
  select(AgeC,
         ID,
         PRED_YEAR,
         SCENARIO,
         hhderchild_adj,
         hhdernochild_adj,
         single_adj)


HH_adj <- rbind(HH20_adj, HH_adj)

# Section 8: Saving Files ----

# Adjusted data (change for unadj data)
write.csv(HH_adj,
          file = paste0(
            out_path,
            "Adjusted_HouseholdProjections_LongForm_UMDI_2020.csv"
          ))

write.csv(
  HH_unadj,
  file = paste0(
    out_path,
    "Unadjusted_HouseholdProjections_LongForm_UMDI_2020.csv"
  )
)


