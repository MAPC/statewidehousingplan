# Section 1: Title, Author, Edit, Input/Output, and Other Info ---- 

# Title:              LaborForceProjections_UMDI_2020
# Authors:            Sarah Philbrick
# Editors:            Sam Rosenberg, Conor Gately
# Program:            RStudio R Script

# Start Date:         04-19-2017
# Last Edit Date:     11-22-2021

# Purpose:            Combine rate files and HH Civilian Population to create Labor Force Projections for 2020 interim update to regional control totals using UMDI-provided reprocessed Census data
#                     This goes through the left side of the model until the end of rate four where output is households by age and wage of householder and number of workers in household.

# Input(s):           Regional HH populations by Age and Sex for 2020
# Output(s):          Labor Force by Education Level, Age, and Sex

# Style Guide:        Refer to "Google's R Style Guide"
#                     Refer to "tidyverse style guide"
#                     Check using lintr()
#                     If something violates Style Guide, change it.
# Legibility:         "Ctrl + Shift + A" to reformat, OR
#                     "Code > Reformat Code"
# Documentation:      See Section 7: Documentation for Example
#                     Add new documentation at end of every session

# Section 2: Package Requirements and Global Options ----

# NOTE: DO NOT ADD RETIRED PACKAGES (e.g., plyr).

# data.table
library(data.table)

# tidyverse
library(tidyverse)

# Section 3: Data Inputs ----

# PERSONAL NOTE: Why is this here?
# hhdpop<- read.csv("K:\\DataServices\\Projects\\Current_Projects\\Projections_2050\\Data\\Tabular\\ReferenceData\\HouseholdPop_MAPC\\MAPC_HHPop_101.csv")

# Setting paths


setwd('K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data')

in_path_rates <- 'K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/'

out_path <- 'K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/output_data/'

# in_path_rates <-
#   "S:/Network Shares/DS Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/"
# out_path <-
#   "S:/Network Shares/DS Projects/Current_Projects/Projections_2050/Data/Tabular/Output/Labor Force 2050Update/Long Form"

# Creating list of file names
# NOTE: To change scenario, add "_V7" after "*" in "*CivHHPop_101"
civhhdpop_files_dir <-
  list.files(
    path = out_path,
    pattern = "*CivHHPop_101",
    full.names = TRUE,
    recursive = FALSE
  )

# Creating list of data frames
civhhdpop_files_raw <- lapply(civhhdpop_files_dir, function(x)
  read.csv(x))

# Setting names for list of files
names(civhhdpop_files_raw) <- basename(civhhdpop_files_dir)

# Edu rates for all RPAs

# Creating list of edu file names 2012-2016
Edu_files_dir <-
  list.files(
    path = in_path_rates,
    pattern = "^Edu_.*\\.*_1216_ac2.csv$",
    full.names = TRUE,
    recursive = FALSE
  )

# Creating list of edu data frames 2012-2016
Edu_files_raw <-
  lapply(Edu_files_dir, function(x) {
    read.csv(x)
  })

# Setting names for LFPR list elements
names(Edu_files_raw) <- basename(Edu_files_dir)

# LF rates of all RPAs

# Creating list of LFPR file names 2012-2016
LFPR_files_dir <-
  list.files(
    path = in_path_rates,
    pattern = "^LFPR_1216_.*\\.*_a_s_edu_ac2.csv$",
    full.names = TRUE,
    recursive = FALSE
  )

# Creating list of LFPR data frames 2012-2016
LFPR_files_raw <-
  lapply(LFPR_files_dir, function(x) {
    read.csv(x)
  })

# Setting names for LFPR list elements
names(LFPR_files_raw) <- basename(LFPR_files_dir)

# Section 4: Civilian Data ----

# Number of RPAs
# NOTE: Has to be changed once MVC and NPEDC data are added to folder
nRPA <- 11

# Creating new names vector for edu
edu_names <-
  c("edu1",
    "edu2",
    "edu3",
    "edu4")

# Creating new names vector for LFPR
lfpr_names <-
  c(
    "lfpr1_m",
    "lfpr2_m",
    "lfpr3_m",
    "lfpr4_m",
    "lfpr1_f",
    "lfpr2_f",
    "lfpr3_f",
    "lfpr4_f"
  )






# Creating data frame to calculate all RPAs (11 RPAs in edu list)
Edu_files <-
  
  # Removing index value
  lapply(Edu_files_raw, function(x)
    select(x, -X)) %>%
  
  # Replacing all NA values with 0
  lapply(function(x) {
    replace(x, is.na(x), 0)
  }) %>%
  
  # Setting names according to input vector
  lapply(setNames, edu_names) %>%
  
  # Multiplying all values by 0.01
  lapply(function(x)
    transmute(x, across(starts_with("edu"), function(x)
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
      into = c("RATE", "ID", "YEAR", "Type"),
      sep = "_"
    )) %>%
  
  # Removing column "Type" (e.g., "acs2.csv")
  lapply(function(x)
    select(x,-c("RATE", "YEAR", "Type"))) %>%
  
  # Binding data into one data frame
  bind_rows() %>%
  
  # Mutating new value to replicate rows
  mutate(z = 4) %>%
  
  # Replicating rows
  uncount(z) %>%
  
  # Creating new column with prediction years
  mutate(PRED_YEAR = rep(seq(2020, 2050, 10), length.out = (60 * nRPA)),
         PRED_YEAR = as.character(PRED_YEAR))







# Labor Force rate table for RPAs (11 elements in list)
LFPR_files <-
  
  # Removing index and "not" columns
  lapply(LFPR_files_raw, function(x)
    select(x,-c(X, contains("not")))) %>%
  
  
  # Replacing all NA values with 0
  lapply(function(x) {
    replace(x, is.na(x), 0)
  }) %>%
  
  # Setting names according to input vector
  lapply(setNames, lfpr_names) %>%
  
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
      into = c("RATE", "YEAR", "ID", "Type1", "Type2", "Type3", "Type4"),
      sep = "_"
    )) %>%
  
  # Removing column "Type" (e.g., "acs2.csv")
  lapply(function(x)
    select(x,-c(
      "RATE", "YEAR", "Type1", "Type2", "Type3", "Type4"
    ))) %>%
  
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
civhhdpop_files <-
  
  # Filtering out first three age categories
  lapply(civhhdpop_files_raw, function(x)
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
      into = c("ID", "PRED_YEAR", "SCENARIO", "Civ", "Type"),
      sep = "_"
    )) %>%
  
  # Removing extra columns
  lapply(function(x)
    select(x, -c("Civ", "Type"))) %>%
  
  # Binding all data frames
  bind_rows() %>%
  
  # Removing RPAs that don't appear in edu files or lfpr files (temporary)
  # NOTE: Remove this once the other RPA files are created/found
  filter(!ID %in% c("MVC", "NPEDC"))






# Section 5: Calculating Long Form Tables ----
LF_proj <-
  
  # Joining all data frames
  left_join(civhhdpop_files, Edu_files) %>%
  left_join(LFPR_files) %>%
  
  # Mutating new values
  mutate(
    "High School or less Male in LF" = Male * edu1 * lfpr1_m,
    "Some College or Associate's Degree Male in LF" = Male * edu2 * lfpr2_m,
    "Bachelor's Male in LF" = Male * edu3 * lfpr3_m,
    "Master's or Higher Male in LF" = Male * edu4 * lfpr4_m,
    "High School or less Female in LF" = Female * edu1 * lfpr1_f,
    "Some College or Associate's Degree Female in LF" = Female * edu2 * lfpr2_f,
    "Bachelor's Female in LF" = Female * edu3 * lfpr3_f,
    "Master's or Higher Female in LF" = Female * edu4 * lfpr4_f
  ) %>%
  
  # Removing extra columns
  select(-c(all_of(lfpr_names), all_of(edu_names), "Male", "Female"))



# Section 6: Saving Files ----
write.csv(LF_proj,
          file = paste0(out_path, "LaborForceProjections_LongForm_UMDI_2020.csv"))



# Section 7: Documentation ----

# 06-30-2021  Sam Rosenberg
#             Copied Sarah's "LaborForceProjections_2050update2" (09-09-2020) to
#             current script, moved current script to "LF_Projections_2050_Efficient"
#             folder, developing function to create list of dataframes.
#             
# 07-06-2021  Sam Rosenberg
#             Reworked df_split function into loop, cleaned education and LFPR data,
#             just needs search and replace.

# 07-16-2021  Sam Rosenberg
#             Finished adding long form. Currently only does V7 scenario, but this
#             is an easy fix.