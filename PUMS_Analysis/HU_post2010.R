library(plyr)
library(dplyr)
library(tidyverse)


D2021h <- read.csv ("C:\\Users\\sphilbrick\\Downloads\\csv_hma\\psam_h25.csv")
D2021p <- read.csv ("C:\\Users\\sphilbrick\\Downloads\\csv_pma\\psam_p25.csv")
PUMAtype <- read.csv ("C:\\Users\\sphilbrick\\Downloads\\new_puma_types_w_rural_2024.csv")
RPAtype <- read.csv ("C:\\Users\\sphilbrick\\Downloads\\PUMA10_RPA_crosswalk.csv")

#Remove scientific notation
options(scipen = 999)

#Combine person and household files
PHa21 <- join(D2021h,D2021p, by = c("SERIALNO"), type = "left", match = "all")

#make PUMA character string with leading 0s
PHa21$PUMAc <- as.character(str_pad(PHa21$PUMA, width = 5, side = "left", pad = 0))
PUMAtype$PUMAc <- as.character(str_pad(PUMAtype$puma_name, width = 5, side = "left", pad = 0))
RPAtype$PUMAc <- as.character(str_pad(RPAtype$PUMA, width = 5, side = "left", pad = 0))

#ADD Community type GEOGRAPHIES and RPA geographies 
PHa21 <- join(PUMAtype, PHa21, by = c("PUMAc"), type = "left", match = "all")
#PHa21 <- join(RPAtype, PHa21, by = c("PUMAc"), type = "left", match = "all")

#Add serialnosp, combine hhd and person id
PHa21$serialnosp <- paste(PHa21$SERIALNO, PHa21$SPORDER, sep= "_")

#Redone Individual Race Category
#NOTE: Categories are (1) White, non hispanic/latino (2)Black, non hispanic/latino (3) hispanic/latino (4)Asian/ Pacific Islander non hispanic/latino (5) other, non hispanic/latino
PHa21$prace <- ifelse(PHa21$RAC1P==1 & PHa21$HISP==1,"White, Non-Hispanic/Latino",
                      ifelse(PHa21$RAC1P==2 & PHa21$HISP==1,"Black, Non-Hispanic/Latino",
                             ifelse(PHa21$HISP>1,"Hispanic/Latino",
                                    ifelse((PHa21$RAC1P==7 | PHa21$RAC1P==6) & PHa21$HISP==1,"Asian/Pacific Islander Non-Hispanic/Latino",
                                           "Other, Non-Hispanic/Latino"))))

# Individual Age Category (both as 1-18 categories and text)
# NOTE: Levels are (1) 0 to 4; (2) 5 to 9; (3) 10 to 14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 29; (7) 30 to 34; (8) 35 to 39; (9) 40 to 44 (10) 45 to 49 (11) 50 to 54 (12) 55 to 59 (13) 60 to 64 (14) 65 to 69 (15) 70 to 74 (16) 75 to 79 (17) 80-84 (18)85p 
PHa21$pagec <- cut(PHa21$AGEP, breaks=c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,Inf), labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"))
PHa21$pagect <- cut(PHa21$AGEP, breaks=c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,Inf), labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+"))


#Create person categories-- 1 person, 2 person, 3 person, 4 plus person
PHa21$HHSize[PHa21$NP==1] = "1 person"
PHa21$HHSize[PHa21$NP==2] = "2 people" 
PHa21$HHSize[PHa21$NP==3] = "3 people" 
PHa21$HHSize[PHa21$NP>=4] = "4+ people"

#Tenure
PHa21$tenure [PHa21$TEN==1 | PHa21$TEN==2] = "Owner"
PHa21$tenure [PHa21$TEN==3 | PHa21$TEN==4] = "Renter"

#Built post-2010
PHa21$hu2010 [PHa21$YRBLT== 2010 | PHa21$YRBLT== 2020 | PHa21$YRBLT == 2021] = "After 2010"
PHa21$hu2010 [PHa21$YRBLT < 2010] = "Before 2010"

#Building Type
PHa21$buildtype [PHa21$BLD==2 | PHa21$BLD==3] = "single family"
PHa21$buildtype [PHa21$BLD>3 & PHa21$BLD<=9] = "multi family"
PHa21$buildtype [PHa21$BLD>9] = "other"

#adjust income
PHa21$adj <- (PHa21$ADJINC/1000000)
PHa21$HINCP_adj = PHa21$HINCP*PHa21$adj
PHa21$incgrp [PHa21$HINCP_adj <= 35000] = "<=$35,000"
PHa21$incgrp [PHa21$HINCP_adj > 35000 & PHa21$HINCP_adj <= 75000] = "$35,001-$75,000"
PHa21$incgrp [PHa21$HINCP_adj > 75000 & PHa21$HINCP_adj <= 125000] = "$75,001-$125,000"
PHa21$incgrp [PHa21$HINCP_adj > 125000 & PHa21$HINCP_adj <= 225000] = "$125,001-$225,000"
PHa21$incgrp [PHa21$HINCP_adj > 225000] = ">$225,000"

#Children category
PHa21$children [PHa21$HUPAC == 1 | PHa21$HUPAC == 2 | PHa21$HUPAC == 3] = "Children"
PHa21$children [PHa21$HUPAC == 4] = "No Children"

#SF vs MF
PHa21$bldtype [PHa21$BLD == 1] = 'Mobile Home'
PHa21$bldtype [PHa21$BLD == 2 | PHa21$BLD == 3] = 'Single Family'
PHa21$bldtype [PHa21$BLD >= 4] = 'Multi-Family'
PHa21$buildtype [PHa21$BLD == 10] = 'Boat, RV, Van, Etc'


#Remove any records in GQ
PHH21 <- PHa21[PHa21$TYPEHUGQ==1,]
HH21 <- PHH21[PHH21$SPORDER==1 | is.na(PHH21$SPORDER),]
gc()

#write only necessary columns 
output21 <- select(HH21, c( 'SERIALNO', 'PUMAc', 'SPORDER','tenure', 'bldtype', 'HHSize', 'children','pagect', 'pagec', 'incgrp', 'RAC1P', 'HISP', 'prace', 'WGTP', 'puma_type', "NP", "hu2010" ))
write.csv(output21, file= "C:\\Users\\sphilbrick\\Downloads\\HH2010output.csv")
