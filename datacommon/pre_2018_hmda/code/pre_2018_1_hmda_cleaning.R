###### THIS CODE CLEANS THE HMDA DATASET
###### FOR 2007-2017
### Author: Taylor Perez

library(dplyr)

# edited by lberman 2024-08-01
# purpose:  prevent citrix VM from crashing, filter the raw file (3.3GB) to single years
# re-order the functions to take advantage of processing only one year at a time
 

#####
## 0. SET PATHS

#set for K
#wd <- "K:/DataServices/Datasets/Housing/HMDA/Code/datacommon/pre_2018_hmda/"
#data_path <- "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/2007_2017/"

## 0.1 set local paths
wd <- "C:/Users/lberman/Desktop/SWAP/other_projects/hmda/pre_2018/"
data_path <- "C:/Users/lberman/Desktop/SWAP/other_projects/hmda/pre_2018/raw/"

setwd(wd)



### 1, load in data

HMDA <- read.csv("hmda_lar.csv",header=TRUE,colClasses=c(census_tract_number="character"))
# you only want to read this 3.3 GB .csv once.    
# reset year below this line and re-run to end to cycle through years.

## filter to single year (to make filesizes manageable)

## 2 set year 
year <- 2017

HMDA_year <- HMDA %>% 
  filter(as_of_year == year)


## 3.  rename columns
colnames(HMDA_year)[1] <- "action"
colnames(HMDA_year)[2] <- "action_nm"
colnames(HMDA_year)[3] <- "agency"
colnames(HMDA_year)[5] <- "agency_nm"
colnames(HMDA_year)[6] <- "ethn"
colnames(HMDA_year)[7] <- "ethn_nm"
colnames(HMDA_year)[8] <- "inc000s"
colnames(HMDA_year)[9] <- "race1"
colnames(HMDA_year)[10] <- "race2"
colnames(HMDA_year)[11] <- "race3"
colnames(HMDA_year)[12] <- "race4"
colnames(HMDA_year)[13] <- "race5"
colnames(HMDA_year)[14] <- "race1_nm"
colnames(HMDA_year)[15] <- "race2_nm"
colnames(HMDA_year)[16] <- "race3_nm"
colnames(HMDA_year)[17] <- "race4_nm"
colnames(HMDA_year)[18] <- "race5_nm"
colnames(HMDA_year)[19] <- "sex"
colnames(HMDA_year)[20] <- "sex_nm"
colnames(HMDA_year)[21] <- "dateind"
colnames(HMDA_year)[22] <- "asofyear"
colnames(HMDA_year)[23] <- "geoid"
colnames(HMDA_year)[24] <- "coethn"
colnames(HMDA_year)[25] <- "coethn_nm"
colnames(HMDA_year)[26] <- "corace1"
colnames(HMDA_year)[27] <- "corace2"
colnames(HMDA_year)[28] <- "corace3"
colnames(HMDA_year)[29] <- "corace4"
colnames(HMDA_year)[30] <- "corace5"
colnames(HMDA_year)[31] <- "corace1_nm"
colnames(HMDA_year)[32] <- "corace2_nm"
colnames(HMDA_year)[33] <- "corace3_nm"
colnames(HMDA_year)[34] <- "corace4_nm"
colnames(HMDA_year)[35] <- "corace5_nm"
colnames(HMDA_year)[36] <- "cosex"
colnames(HMDA_year)[37] <- "cosex_nm"
colnames(HMDA_year)[38] <- "county_id"
colnames(HMDA_year)[39] <- "countyname"
colnames(HMDA_year)[40] <- "denial1"
colnames(HMDA_year)[41] <- "denial2"
colnames(HMDA_year)[42] <- "denial3"
colnames(HMDA_year)[43] <- "denial1_nm"
colnames(HMDA_year)[44] <- "denial2_nm"
colnames(HMDA_year)[45] <- "denial3_nm"
colnames(HMDA_year)[46] <- "editst"
colnames(HMDA_year)[47] <- "editst_nm"
colnames(HMDA_year)[48] <- "hoepast"
colnames(HMDA_year)[49] <- "hoepast_nm"
colnames(HMDA_year)[50] <- "lienst"
colnames(HMDA_year)[51] <- "lienst_nm"
colnames(HMDA_year)[52] <- "loanpur"
colnames(HMDA_year)[53] <- "loanpur_nm"
colnames(HMDA_year)[54] <- "loanty"
colnames(HMDA_year)[55] <- "loanty_nm"
colnames(HMDA_year)[56] <- "msamd"
colnames(HMDA_year)[57] <- "msamd_nm"
colnames(HMDA_year)[58] <- "ownocc"
colnames(HMDA_year)[59] <- "ownocc_nm"
colnames(HMDA_year)[60] <- "preapp"
colnames(HMDA_year)[61] <- "preapp_nm"
colnames(HMDA_year)[62] <- "propty"
colnames(HMDA_year)[63] <- "propty_nm"
colnames(HMDA_year)[64] <- "purchty"
colnames(HMDA_year)[65] <- "purchty_nm"
colnames(HMDA_year)[66] <- "respondid"
colnames(HMDA_year)[67] <- "seqnum"
colnames(HMDA_year)[68] <- "state_id"
colnames(HMDA_year)[69] <- "state_abbr"
colnames(HMDA_year)[70] <- "state_name"
colnames(HMDA_year)[71] <- "hudmedinc"
colnames(HMDA_year)[72] <- "loan000s"
colnames(HMDA_year)[73] <- "tot14units"
colnames(HMDA_year)[74] <- "totownocc"
colnames(HMDA_year)[75] <- "totmin_p"
colnames(HMDA_year)[76] <- "totpop"
colnames(HMDA_year)[77] <- "ratespread"
colnames(HMDA_year)[78] <- "tractmsamd"
colnames(HMDA_year)

### 4. export data - with strings
write.csv(HMDA_year,paste0("output/hmda_all_cleaned_",year,".csv"),row.names=FALSE)


# 4.1 altered for single year
HMDA.codes <- HMDA_year[,-grep("nm$",colnames(HMDA_year))]
HMDA.codes <- HMDA.codes[,-grep("name$",colnames(HMDA.codes))]
HMDA.codes <- HMDA.codes[,-grep("abbr$",colnames(HMDA.codes))]
head(HMDA.codes)

# 4.2 export final coded version
write.csv(HMDA.codes,paste0("output/hmda_codes_cleaned_",year,".csv"),row.names=FALSE)



