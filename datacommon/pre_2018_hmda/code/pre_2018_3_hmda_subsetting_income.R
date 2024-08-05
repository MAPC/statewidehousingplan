### HMDA Loan Outcomes By Income Bracket
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


## 1. import data. use data with geographic keys created by [pre_2018_2_hmda_geographies.R]
hmda <- read.csv(paste0("output/hmda_geog_",hmda_yr,".csv"),header=TRUE)


## 2. subset based on income

# 2.1 assign factors
hmda$inc_factor <- ifelse(hmda$inc000s >= 0 & hmda$inc000s < 15, # income 0-15k
                          1,
                          ifelse(hmda$inc000s >= 15 & hmda$inc000s < 35, # income 15-35K
                                 2,
                                 ifelse(hmda$inc000s >= 35 & hmda$inc000s < 75, # income 35-75k
                                 3,
                                 ifelse(hmda$inc000s >= 75 & hmda$inc000s < 125, # income 75-125k
                                        4,
                                        5)))) # income > 125k
hmda$inc_factor <- as.factor(hmda$inc_factor)
hmda <- subset(hmda,(loanpur == 1) & (action == 1 | action == 3) & (is.na(hmda$inc_factor) == FALSE))# subset grand data by action and home purchases only, omit NAs

# 2.2  subset data by year and get percents 

income.t <- c("i015tot","i1535tot","i3575tot","i75125tot","i125otot") # col names by total, originated, denied
income.o <- c("i015ori","i1535ori","i3575ori","i75125ori","i125ooori") 
income.d <- c("i015den","i1535den","i3575den","i75125den","i125oden")


  
## 3. set ct_id <- correct year for specific years
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

for(i in (hmda)){

  # 4 group data
  hmda.ct_id <- hmda.sub %>% # group by ct ids
    group_by(ct_id) 
  hmda.inc <- hmda.sub %>% # group by ct ids and income factors
    group_by(ct_id,inc_factor)
  hmda.action <- hmda.sub %>% # group by ct ids by action
    group_by(ct_id,action)
  hmda.all <- hmda.sub %>%
    group_by(ct_id,action,inc_factor) # group by ct id by action by income 
  
  # 4.1 total loans by ct id 
  tot_loans <- tally(hmda.ct_id) # tally counts of total loans by ct
  colnames(tot_loans)[2] <- "total"
  
  # 4.2 total loans by ct id by income
  tot_loans_inc <- tally(hmda.inc) # tally counts of total loans by ct & inc factor
  tot_loans_inc <- cast(tot_loans_inc,ct_id~inc_factor) # cast dataframe so inc factors are columns
  colnames(tot_loans_inc)[2:6] <- income.t
  
  # 4.3 number of originated/denied loans by ct id
  action_loans <- tally(hmda.action) # tally counts of actions by type
  action_loans <- cast(action_loans,ct_id~action) # cast dataframe so actions are columns
  colnames(action_loans)[2:3] <- c("origin","denied")
  
  # 4.4 number of originated/denied loans by ct id by income
  action_loans_inc <- tally(hmda.all) # tally counts of total loans by ct by action by income
  action_loans_inc <- cast(action_loans_inc,ct_id~action + inc_factor) # cast dataframe so action/inc factors are columns
  colnames(action_loans_inc)[2:6] <- income.o
  colnames(action_loans_inc)[7:11] <- income.d
  
  # 4.5 join to table 
  hmda.income <- tot_loans %>% left_join(tot_loans_inc,"ct_id") %>%
    left_join(action_loans,"ct_id") %>%
    left_join(action_loans_inc,"ct_id")
  
  # 4.6 reorder columns 
  hmda.income$asofyear <- hmda_yr # add asofyear column  [changed from i in the loop to specific year]
  hmda.income <- hmda.income %>% select(ct_id,asofyear,total,income.t,origin,income.o,denied,income.d)
  
  # 4.7 calculate percents
  pct.total <- list()
  pct.origin <- list()
  pct.denied <- list()
  
  for(i in income.t){
    pct.total[[i]] <- hmda.income[[i]]/hmda.income$total # total
  }
  names(pct.total) <- paste(income.t,"p",sep="")
  hmda.income <- cbind(hmda.income,pct.total)
  
  hmda.income$originp <- hmda.income$origin/hmda.income$total # originated
  for(i in income.o){
    pct.origin[[i]] <- hmda.income[[i]]/hmda.income$origin
  }
  names(pct.origin) <- paste(income.o,"p",sep="")
  hmda.income <- cbind(hmda.income,pct.origin)
  
  hmda.income$deniedp <- hmda.income$denied/hmda.income$total # denied
  for(i in income.d){
    pct.denied[[i]] <- hmda.income[[i]]/hmda.income$denied 
  }
  names(pct.denied) <- paste(income.d,"p",sep="")
  hmda.income <- cbind(hmda.income,pct.denied)
  hmda.income[is.na(hmda.income)] <- 0

}

## 5  export data 
write.csv(hmda.income,paste0("output/hmda_income_",hmda_yr,".csv"),row.names=FALSE)

# section to bind and write by years or group of years removed