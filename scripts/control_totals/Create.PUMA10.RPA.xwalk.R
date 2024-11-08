#===============================================================================
# Creating a 2010 PUMA to RPA Crosswalk
# Author(s): Brandon Stanaway and Conor Gately
# Last Updated: 11/8/2024

# Purpose: ACS PUMS data is grouped by Public Use Microdata Areas (PUMAs) which do not
# match commonly used geographies like Regional Planning Agency (RPA) regions. Municipalities
# have both a PUMA and a RPA to which they belong. This script creates a crosswalk between
# PUMAs and RPAs. Whena single PUMA is crosswalked to multiple RPAs (as is the case 
# for PUMA 04800 and CCC, MVC, and NPEDC) duplicate PUMA to RPA relationships are 
# created.

#===============================================================================
# 0.0 Environment Set-up
library(tidyverse)
library(data.table)
library(mapcdatakeys)

# Get rid of scientific notation
options(scipen = 999)
# Set random seed
set.seed(351)

# Base root. Toggle between S: and K: drive depedning on mapped paths on your computer
base <- "K:/DataServices/Projects/Current_Projects/"
# base <- "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/"

#General K drive filepath
root <- paste0(base, "Housing/StatewideHousingPlan/04_Analysis/Data/Working/Regional_Control_Totals/")

#Set output filepath
output_path <- paste0(base, "Housing/StatewideHousingPlan/04_Analysis/Data/Working/Regional_Control_Totals/", scen, "/")

# Reweighter files path
rwt_path <- paste0(base, "Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/")

#===============================================================================
# 1.0 Generate Crosswalk

# Set up datakeys for future joins.
mkeys <- mapcdatakeys::all_muni_data_keys |> 
  select(muni_id, rpa_acr, mpo)

# Load in the municipality to PUMA crosswalk.
psf <- fread(paste0(root, 'pums_muni_inter.csv')) |> 
  select(TOWN_ID, PUMACE10, Shape_Area) %>%
  dplyr::rename(muni_id = TOWN_ID, PUMA = PUMACE10) |> 
  # Join datakeys by muni_id to attach RPA designation of municipalities to the PUMA designation.
  left_join(mkeys, by = c('muni_id')) |> 
  setDT()

# Generate RPA to PUMA crosswalk.
lsf <- psf[, lapply(.SD, sum, na.rm = T), .(rpa_acr, PUMA), .SDcols = 'Shape_Area']
lsf[, mx := max(Shape_Area), PUMA]
xw <- lsf[Shape_Area == mx, .(rpa_acr, PUMA)]
xw[, PUMA := sprintf("%05d", PUMA)]

# Manufacturing MVC and NPEDC RPAs manually

cc <- data.table(rpa_acr = c('MVC', 'NPEDC'),
                 PUMA = c('04800', '04800'))

#Append MVC and NPEDC data to PUMA to RPA/MPO crosswalk
xw <- rbind(xw, cc)

#===============================================================================
#2.0 Output Crosswalk

fwrite(xw, paste0(output_path, 'PUMA10_RPA_crosswalk.csv'))
fwrite(xw, paste0(rwt_path, 'PUMS_data/PUMA10_RPA_crosswalk.csv'))

#Remove intermediate dataframes.
gc()