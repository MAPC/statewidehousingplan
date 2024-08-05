###### THIS CODE DIVIDES THE HMDA DATASET
###### FOR 2007-2017 INTO RELEVANT SUBSETS
###### BASED ON RACE AND ETHNICITY
### Author: Taylor Perez

library(dplyr)
library(foreign)
library(reshape)
library(tidyverse)

# edited by lberman 2024-08-05
# changed to processing for single years (removing steps for binding)

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

# ## 0.3 derive year for keys
# cosub_yr <- str_sub(hmda_yr, -2)

# # note census tract IDs for 2020 were not applied until 2022
# cosub_col <- if(hmda_yr < 2013) {
#   "ct00_id"
# } else if (hmda_yr < 2022) {
#   "ct10_id"  
# } else {
#   "ct20_id"  
# }


## 1. import data. use data with geographic keys created by [pre_2018_2_hmda_geographies.R]
hmda_input <- read.csv(paste0("output/hmda_geog_",hmda_yr,".csv"),stringsAsFactors=FALSE,header=TRUE)


## 2.  subset based on race 

# 2.1 remove unused cols
hmda <- subset(hmda_input[-which(hmda_input$ethn== 3 | hmda_input$ethn == 4 | hmda_input$race1 == 6),])

# 2.2 refactor race eth value
hmda$re_factor <- ifelse(hmda$ethn == 2 & (hmda$race1 == 5 & is.na(hmda$race2) & is.na(hmda$race3) & is.na(hmda$race4) & is.na(hmda$race5)),
                         "Non-Hispanic White Alone",
                         ifelse(hmda$ethn == 2 & (hmda$race1 == 3 & is.na(hmda$race2) & is.na(hmda$race3) & is.na(hmda$race4) & is.na(hmda$race5)),
                                "Non-Hispanic Black or African American Alone",
                                ifelse(hmda$ethn == 2 & (hmda$race1 == 1 & is.na(hmda$race2) & is.na(hmda$race3) & is.na(hmda$race4) & is.na(hmda$race5)),
                                       "Non-Hispanic Native American or Alaska Native Alone",
                                       ifelse(hmda$ethn == 2 & (hmda$race1 == 2 & is.na(hmda$race2) & is.na(hmda$race3) & is.na(hmda$race4) & is.na(hmda$race5)),
                                              "Non-Hispanic Asian Alone",
                                              ifelse(hmda$ethn == 2 & (hmda$race1 == 4 & is.na(hmda$race2) & is.na(hmda$race3) & is.na(hmda$race4) & is.na(hmda$race5)),
                                                     "Non-Hispanic Native Hawaiian or Pacific Islander Alone",
                                                     ifelse((hmda$ethn == 1 | hmda$ethn == 2) & hmda$race1 == 7 & (is.na(hmda$race2) == TRUE),
                                                            "Some Other Race Alone",
                                                            ifelse((hmda$ethn == 1 | hmda$ethn == 2) & (is.na(hmda$race2) == FALSE),
                                                                   "Two or More Races",
                                                                   "Hispanic/Latinx"
                                                     )))))))
hmda$re_factor <- as.factor(hmda$re_factor)
hmda <- subset(hmda,(loanpur == 1) & (action == 1 | action == 3)) # subset grand data by action and home purchases only


# set up col names
re.t <- c("lattot","nhastot","nhaatot","nhnatot","nhpitot","nhwhitot","mlttot") # col names by total, originated, denied based off of ACS nomenclature
re.o <- c("latori","nhasori","nhaaori","nhnaori","nhpiori","nhwhiori","mltori") 
re.d <- c("latden","nhasden","nhaaden","nhnaden","nhpiden","nhwhiden","mltden")


## 3. set ct_id col = correct year
hmda.sub <- if(hmda_yr < 2013) {
  hmda %>% 
    mutate(ct_id = as.character(ct00_id))
} else if (hmda_yr < 2022) {
  hmda %>% 
    mutate(ct_id = as.character(ct10_id))
} else {
  hmda %>% 
    mutate(ct_id = as.character(ct20_id))
}


## 4 process groups and sums
######### subset data by year and get percents ########

# for(i in min(hmda$asofyear):max(hmda$asofyear)){  
# original loop based on year not needed for single year

for(i in (hmda.sub)){
      
#  hmda.sub <- subset(hmda,asofyear == i)  
  
  # group data
  hmda.ct_id <- hmda.sub %>% # group by ct ids
    group_by(ct_id) 
  hmda.inc <- hmda.sub %>% # group by ct ids and race/ethn factors
    group_by(ct_id,re_factor)
  hmda.action <- hmda.sub %>% # group by ct ids by action
    group_by(ct_id,action)
  hmda.all <- hmda.sub %>%
    group_by(ct_id,action,re_factor) # group by ct id by action by race/ethn 
  
  # total loans by ct id 
  tot_loans <- tally(hmda.ct_id) # tally counts of total loans by ct
  colnames(tot_loans)[2] <- "total"
  
  # total loans by ct id by race/ethn
  tot_loans_re <- tally(hmda.inc) # tally counts of total loans by ct & inc factor
  tot_loans_re <- cast(tot_loans_re,ct_id~re_factor) # cast dataframe so race factors are columns
  colnames(tot_loans_re)[2:8] <- re.t
  
  # number of originated/denied loans by ct id
  action_loans <- tally(hmda.action) # tally counts of actions by type
  action_loans <- cast(action_loans,ct_id~action) # cast dataframe so actions are columns
  colnames(action_loans)[2:3] <- c("origin","denied")
  
  # number of originated/denied loans by ct id by race/ethn
  action_loans_re <- tally(hmda.all) # tally counts of total loans by ct by action by race/ethn
  action_loans_re <- cast(action_loans_re,ct_id~action + re_factor) # cast dataframe so action/race factors are columns
  colnames(action_loans_re)[2:8] <- re.o
  colnames(action_loans_re)[9:15] <- re.d
  
  # join to table 
  hmda.re <- tot_loans %>% left_join(tot_loans_re,"ct_id") %>%
    left_join(action_loans,"ct_id") %>%
    left_join(action_loans_re,"ct_id")
  
  # reorder columns 
  hmda.re$asofyear <- hmda_yr # add asofyear column # change i to specific year = hmda_yr
  hmda.re <- hmda.re %>% select(ct_id,asofyear,total,re.t,origin,re.o,denied,re.d)
  
  # calculate percents
  pct.total <- list()
  pct.origin <- list()
  pct.denied <- list()
  
  for(i in re.t){
    pct.total[[i]] <- hmda.re[[i]]/hmda.re$total # total
  }
  names(pct.total) <- paste(re.t,"p",sep="")
  hmda.re <- cbind(hmda.re,pct.total)
  
  hmda.re$originp <- hmda.re$origin/hmda.re$total # originated
  for(i in re.o){
    pct.origin[[i]] <- hmda.re[[i]]/hmda.re$origin
  }
  names(pct.origin) <- paste(re.o,"p",sep="")
  hmda.re <- cbind(hmda.re,pct.origin)
  
  hmda.re$deniedp <- hmda.re$denied/hmda.re$total # denied
  for(i in re.d){
    pct.denied[[i]] <- hmda.re[[i]]/hmda.re$denied 
  }
  names(pct.denied) <- paste(re.d,"p",sep="")
  hmda.re <- cbind(hmda.re,pct.denied)
  hmda.re[is.na(hmda.re)] <- 0

}

## 5  export data 
write.csv(hmda.re,paste0("output/hmda_re_",hmda_yr,".csv"),row.names=FALSE)
  
    

# section to bind and write by years or group of years removed 
