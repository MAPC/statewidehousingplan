###### THIS CODE ADDS RELEVANT GEOGRAPHIES
###### TO THE HMDA DATASET FOR 2007-2017

#.libPaths("H:/Data/RLibrary")
# original
#wd <- "K:/DataServices/Datasets/Housing/HMDA/2007_2017/"
#wd <- "C:/Users/lberman/Desktop/SWAP/other_projects/hmda/pre_2018/"


wd <- "K:/DataServices/Datasets/Housing/HMDA/Code/datacommon/pre_2018_hmda/"
setwd(wd)

data_path <- "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/2007_2017/"



setwd(wd)

library(dplyr)
library(foreign)
library(tidyverse)

#install.packages("pacman")
#pacman::p_load("dplyr","foreign","tidyverse")

### set year ###
year <- 2016

######### load in data #########

HMDA.all <- read.csv(paste0("output/hmda_all_cleaned_",year,".csv"),colClasses=c(geoid="character"),header=TRUE)
HMDA.all <- HMDA.all[!(is.na(HMDA.all$geoid) | HMDA.all$geoid == ""),] # remove rows with no associated geography

######### add geographies #########
### NOTE: joins based on information from https://www.ffiec.gov/hmda/glossary.htm

# fix tract IDs
HMDA.all$tract_id <- gsub("\\.","",HMDA.all$geoid) # remove decimal
HMDA.all$county_id <- as.character(HMDA.all$county_id) # redefine county column
HMDA.all$countyid <- ifelse(nchar(HMDA.all$county_id)==1,paste("00",HMDA.all$county_id,sep=""),paste("0",HMDA.all$county_id,sep="")) # add leading zeros
HMDA.all$geoid2 <- paste(as.character(HMDA.all$state_id),HMDA.all$countyid,HMDA.all$tract_id,sep="") # concatenate for final GEOID (tracts)

######### add municipality information #########

# years 2007-2011 (2000 Census IDs)
tracts_2000 <- read.dbf("data_keys/Census00_Tract_MuniJoin.dbf") # load in data key

# save full index copy for reference
write.csv(tracts_2000,"output/census_tracts_2000_munijoin_trim.csv")

tracts_2000 <- tracts_2000[,c(6:9,20:21)] # remove unnecessary columns
tracts_2000$ct_id <- as.character(tracts_2000$ct_id)

# save minimal cols copy for reference
write.csv(tracts_2000,"output/census_tracts_2000_munijoin_trim.csv")


HMDA_2007_2011 <- subset(HMDA.all,asofyear <= 2011)
HMDA_2007_2011_munis <- left_join(HMDA_2007_2011,tracts_2000,by=c("geoid2"="ct_id")) # join municipality info by CT ID
HMDA_2007_2011_munis <- HMDA_2007_2011_munis[,-c(68:70,79,82:84)]
HMDA_2007_2011_munis$town <- str_to_title(HMDA_2007_2011_munis$town) # get muni names in title case
colnames(HMDA_2007_2011_munis)[78] <- "municipal" # rename columns for later rbind
colnames(HMDA_2007_2011_munis)[79] <- "muni_id"

# years 2012-2017 (2010 Census IDs)
tracts_2010 <- read.csv("data_keys/census_2010_munis.csv",header=TRUE,colClasses=c(ct10_id="character")) # load in data key
tracts_2010.muni_all <- aggregate(as.character(municipal) ~ ct10_id,tracts_2010,toString) # aggregate ct10_ids with all associated municipalities as one column
tracts_2010.muni_id <- aggregate(muni_id ~ ct10_id,tracts_2010,toString) # aggregate ct10_ids with all associated municipality IDs as one column
tracts_2010.agg <- cbind(tracts_2010.muni_all,tracts_2010.muni_id)
tracts_2010.agg <- tracts_2010.agg[,-3] # remove additional ct10_id
colnames(tracts_2010.agg)[2] <- "municipal" # rename muni column

HMDA_2012_2017 <- subset(HMDA.all,asofyear > 2011)
HMDA_2012_2017_munis <- left_join(HMDA_2012_2017,tracts_2010.agg,by=c("geoid2"="ct10_id")) # join municipality info by CT ID
HMDA_2012_2017_munis <- HMDA_2012_2017_munis[,-c(68:70,79,84)] # remove unnecessary columns

# bind rows
HMDA.all.munis <- rbind(HMDA_2007_2011_munis,HMDA_2012_2017_munis)

# reorder columns & final cleaning
HMDA.all.munis <- HMDA.all.munis[,c(77,79,78,76,39,56:57,1:38,40:55,58:75)] # reorder to get all geographies to left of table
colnames(HMDA.all.munis)[1] <- "ct_id" # rename to standard
HMDA.all.munis$countyname <- as.character(HMDA.all.munis$countyname)
HMDA.all.munis$countyname <- substring(HMDA.all.munis$countyname,1,nchar(HMDA.all.munis$countyname)-7) # remove "County" from county names 
HMDA.all.munis$msamd <- as.character(HMDA.all.munis$msamd) # get msamd IDs as character
HMDA.all.munis <- HMDA.all.munis[,-c(30,45)] # remove leftover geography columns 

######### export data #########

write.csv(HMDA.all.munis,file="HMDA_2007_2017_final.csv",row.names=FALSE)
