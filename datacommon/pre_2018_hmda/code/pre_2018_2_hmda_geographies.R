###### THIS CODE ADDS RELEVANT GEOGRAPHIES
###### TO THE HMDA DATASET FOR YEARS 2007-2017
### Author: Taylor Perez

library(dplyr)
library(foreign)
library(tidyverse)

# edited by lberman 2024-08-05
# changed to processing for single years (removing steps for binding)
# changed joins to use (ct00_id, ct10_id,ct20_id) from mapcdatakeys based on input year value 

#####
## 0. SET PATHS and YEAR

#set for K
#wd <- "K:/DataServices/Datasets/Housing/HMDA/Code/datacommon/pre_2018_hmda/"
#data_path <- "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/2007_2017/"

## 0.1 set local paths
wd <- "C:/Users/lberman/Desktop/SWAP/other_projects/hmda/pre_2018/"
data_path <- "C:/Users/lberman/Desktop/SWAP/other_projects/hmda/pre_2018/raw/"

setwd(wd)


## 0.2 set year 
hmda_yr <- 2017


## 0.3 derive year for keys
cosub_yr <- str_sub(hmda_yr, -2)

# note census tract IDs for 2020 were not applied until 2022
cosub_col <- if(hmda_yr < 2013) {
  "ct00_id"
} else if (hmda_yr < 2022) {
  "ct10_id"  
} else {
  "ct20_id"  
}

#####
## 1. load in data ##   use the final coded version created by [pre_2018_1_hmda_cleaning.R]

HMDA.input <- read.csv(paste0("output/hmda_codes_cleaned_",hmda_yr,".csv"),colClasses=c(geoid="character"),header=TRUE)
#HMDA.all <- read.csv(paste0("output/hmda_all_cleaned_",year,".csv"),colClasses=c(geoid="character"),header=TRUE)

## 1.1 rm blank rows
HMDA.input <- HMDA.input[!(is.na(HMDA.input$geoid) | HMDA.input$geoid == ""),] # remove rows with no associated geography


# 1.2 fix tract IDs
HMDA.input$tract_id <- gsub("\\.","",HMDA.input$geoid) # remove decimal
HMDA.input$county_id <- as.character(HMDA.input$county_id) # redefine county column
HMDA.input$countyid <- ifelse(nchar(HMDA.input$county_id)==1,paste("00",HMDA.input$county_id,sep=""),paste("0",HMDA.input$county_id,sep="")) # add leading zeros
HMDA.input$geoid2 <- paste(as.character(HMDA.input$state_id),HMDA.input$countyid,HMDA.input$tract_id,sep="") # concatenate for final GEOID (tracts)


# 1.3 reset geoid2 to num
HMDA.input <- HMDA.input %>% 
  mutate(geoid2 = as.numeric(geoid2))


## 2.1 get data keys xw for the year
# see: https://github.com/MAPC/mapcdatakeys/blob/main/census_geog_keys.md

keys <- if(hmda_yr < 2013) {
  read_csv(paste0(test_path,'geog_xw_2000_DRAFT.csv'))  
} else if(hmda_yr < 2022) { #hmda data uses 2010 census tracts through 2021
  mapcdatakeys::geog_xw_2010  
} else {
  mapcdatakeys::geog_xw_2020  
} 

# 2.1 trim to distinct ct id

dist_keys <- if(hmda_yr < 2013) {
  distinct(keys, ct00_id, .keep_all = TRUE) 
} else if(hmda_yr < 2022) {
  distinct(keys, ct10_id, .keep_all = TRUE)
} else {
  distinct(keys, ct20_id, .keep_all = TRUE)
} 

# 2.3 trim to needed cols
trim_keys <- if(hmda_yr < 2013) {
  dist_keys %>% select(c(ct00_id,county,muni_id,muni_name))
} else if(hmda_yr < 2022) {
  dist_keys %>% select(c(ct10_id,county,muni_id,muni_name)) 
} else {
  dist_keys %>% select(c(ct20_id,county,muni_id,muni_name))
} 

rm(keys,dist_keys)

##### add geographies #####

# 3 join muni keys
HMDA_munis <- left_join(HMDA.input,trim_keys,by=c("geoid2"="ct10_id")) # join municipality info by CT ID


# 3.1 trim to needed cols
HMDA_geog <- if(hmda_yr < 2013) {
  HMDA_munis %>% 
    mutate(ct00_id = as.character(geoid2)) %>% 
    mutate(msamd = as.character(msamd)) %>% 
    mutate(county_id = as.integer(county_id)) %>% 
    select(-c(geoid2,seqnum,countyid))
} else if(hmda_yr < 2022) {
  HMDA_munis %>% 
    mutate(ct10_id = as.character(geoid2)) %>% 
    mutate(msamd = as.character(msamd)) %>% 
    mutate(county_id = as.integer(county_id)) %>% 
    select(-c(geoid2,seqnum,county_id))
} else {
  HMDA_munis %>% 
    mutate(ct20_id = as.character(geoid2)) %>% 
    mutate(msamd = as.character(msamd)) %>% 
    mutate(county_id = as.integer(county_id)) %>% 
    select(-c(geoid2,seqnum,county_id))
} 


# 3.2 re-order to get all geographies to left of table
HMDA_geog <- HMDA_geog[,c(49,44,46,45,48,47,1:43)] 


## 4  export data 
write.csv(HMDA_geog,paste0("output/hmda_geog_",hmda_yr,".csv"),row.names=FALSE)
