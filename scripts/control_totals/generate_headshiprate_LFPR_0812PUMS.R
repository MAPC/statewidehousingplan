################################################################################
################################################################################
################################################################################
#
#
#
#
#
#
#This script is designed to streamline and replace 01 - 03 of the Projections
#script sequence. The purpose is to load and clean PUMS and UMDI population projection
#data, generate HH and LF rates with various crosstabs, validate that data,
#create summary statsitics and data visualizations, create LF and HH projections
#based on the rate data and population projections.
#
#The scripts that generate the baseline LF and HH projections scenario are located
#in section XX
#Alternative scenarios have yet to be determined; they will be located in sub-sections
# YY ZZ etc.

#This script will eventually be migrated to an RMarkdown file for ease of use.
################################################################################
################################################################################
################################################################################
### 0. Environment Setup



#0.1 Load in requisite libraries
library(tidyverse)
library(tidycensus)
library(srvyr)
library(janitor)
library(data.table)
library(easycensus)
#library(mapcdatakeys)
library(readxl)
library(gt)
library(tsibble)
library(feasts)
library(magrittr)

#0.2 Setting Filepaths
#default_dir <- "K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/"

#puma_inputs_dir <- "K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/"

#output_dir <- ""

#Remote  Filepaths
default_dir <- "S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs"

puma_inputs_dir <- "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs"

output_dir <- "S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs"

#0.3 Setting Tidycensus API key
census_api_key("d590f657a113f2f78b5a422462ea00745e79111c")

###############################################################################
### 1. Reading in Datasets

#1.1 Reading in Census Data

#List of ACS PUMS data iterations
pums_list <- c(2012)

#List of census variables
variable_list <- c("PUMA00", "PUMA10", "TYPE", "SEX", "AGEP", "RAC1P", "HISP", "ESR",
                     "WKHP", "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER")

#2008-2012 ACS PUMS
PUMS_data_2012 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
})

#1.2 Reading in PUMA Geographies
setwd(puma_inputs_dir)

puma_00 <- read.csv("PUMA_2000Geog.csv")

puma_10 <- read.csv("PUMA_2010Geog.csv")

puma_xwalk <- read_excel("PUMA2000_PUMA2010_crosswalk.xlsx")

#puma_20_raw <- fread("PUMA 2020Geog.csv)

setwd(default_dir)

puma_muni_xwalk <- read.csv("S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs/ma_muni_puma10_crosswalk.csv")

#PUMS 2008-2012 ACS PUMS
PUMS_data_2012 <- PUMS_data_2012 %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA00 = as.character(PUMA00),
    PUMA00 = str_pad(PUMA00, 5, side = "left", pad = "0"),
    PUMA10 = as.character(PUMA10),
    PUMA10 = str_pad(PUMA10, 5, side = "left", pad = "0"),
    
    # Individual Age Category
    # NOTE: Levels are (1) 0 to 4; (2) 5 to 9; (3) 10 to 14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 29; (7) 30 to 34; (8) 35 to 39; (9) 40 to 44 (10) 45 to 49 (11) 50 to 54 (12) 55 to 59 (13) 60 to 64 (14) 65 to 69 (15) 70 to 74 (16) 75 to 79 (17) 80-84 (18)85p
    AGEP = as.numeric(AGEP), #Change datatype of 
    PAGEC =
      cut(AGEP,
          breaks = c(-Inf, seq(4, 84, 5), Inf),
          labels = 1:18),
    
    # Consolidated Age Category
    # NOTE: Levels are (0) 0-14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 34; (7) 35 to 44; (8) 45 to 54; (9) 55 to 64 (10) 65 to 74 (11) 75+
    PAGEC2 =
      cut(
        AGEP,
        breaks = c(-Inf, 14, 19, 24, 34, 44, 54, 64, 74, 79, 84, Inf),
        labels = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
      ),
    
    # Individual Race Category (Analysis may need more specificity than this)
    # NOTE: Categories are (1) White, non hispanic/latino (2)Black, non hispanic/latino (3) hispanic/latino (4)Asian/ Pacific Islander non hispanic/latino (5) other, non hispanic/latino
    PRACE = case_when(
      RAC1P == 1 & HISP == 1 ~ 1,
      RAC1P == 2 & HISP == 1 ~ 2,
      (RAC1P == 6 | RAC1P == 7) & HISP == 1 ~ 4,
      HISP != 1 ~ 3,
      TRUE ~ 5
    ),
    
    # Age, Sex, Race Cat in order of Age Cat, Sex Cat, Race Cat
    #ASRC = paste(PAGEC, SEX, PRACE, sep = "_"),
    # Age, Sex Cat in order of Age Cat, Sex Cat
    #ASC = paste(PAGEC, SEX, sep = "_"),
    # Age, Race Cat in the order of Age Cat, Race Cat
    #ARC = paste(PAGEC, PRACE, sep = "_"),
    
    # Categorizing employment status
    empstat = case_when(
      ESR == 0 ~ "Under 16 not eligible for work",
      ESR == 1 ~ "Employed",
      ESR == 2 ~ "Employed but not at work",
      ESR == 3 ~ "Unemployed",
      ESR == 4 ~ "Armed forces",
      ESR == 5 ~ "Armed forces but not at work",
      ESR == 6 ~ "Not in labor force"
    ),
    
    # Categorizing Civilian Labor Force or not
    # NOTE: Categories are (0) under 16 or non-civilian, (1) in labor force, (2) not in labor force
    lf = case_when(
      ESR == 0 | ESR == 4 | ESR == 5  ~ 0,
      ESR == 1 | ESR == 2 | ESR == 3  ~ 1,
      ESR == 6 ~ 2,
      is.na(ESR) ~ 0
    ),
    
    # Counts as Worker
    # NOTE: Categories are (0) under 16 or not in labor force, or (1) in labor force or non-civilian worker
    worker = case_when(
      (ESR == 0 | ESR == 6) ~ 0,
      (ESR == 1 | ESR == 2 | ESR == 3 | ESR == 4 | ESR == 5) ~ 1,
      is.na(ESR) ~ 0
    ),
    
    # Categorizing full or part-time work
    wrkfull = case_when(
      WKHP < 35 ~ "Part time (less than 35 hrs)",
      WKHP > 34 ~ "Full time (35 hrs or more)",
      is.na(WKHP) ~ "Under 16, unemployed, or not in labor force"
    ),
    
    # Categorizing educational attainment
    # NOTE: Categories are (1) High school degree or less, (2) Associate's degree or some college, (3) Bachelor's, and (4) MA or higher
    SCHL = as.numeric(SCHL),
    eduattn = case_when(
      SCHL <= 17 ~ 1,
      SCHL %in% 18:20 ~ 2,
      SCHL == 21 ~ 3,
      SCHL >= 22 ~ 4,
      is.na(SCHL) ~ 0
    ),
    
    # Adjust wages to constant dollars (2012)
    WAGPA = round(as.numeric(ADJINC) * as.numeric(WAGP), 0),
    
    # Adjust total self employment income to constant dollars (2012)
    SWAGPA = round(as.numeric(ADJINC) * as.numeric(SEMP), 0),
    
    # Combine wages and self employment
    WAGPALL = WAGPA + SWAGPA,
    
    # Break wages into wage groups
    WAGPALLC =
      cut(
        WAGPALL,
        breaks = c(-Inf, 0, 10000, 30000, 50000, 75000, 125000, Inf),
        labels = c(seq(0, 6))
      ),
    
    # Determine if person is head of household
    HousHder = case_when(SPORDER == 1 ~ 1,
                         SPORDER > 1 ~ 2),
    
    # Determine if case counts as person
    person = case_when(SPORDER != 0 ~ 1,
                       SPORDER == 0 ~ 0),
    
    # Determine if case counts as child
    child = case_when(AGEP < 18 ~ 1,
                      AGEP >= 18 ~ 0)
  ) %>%
  
  #Generating Household Variables
  group_by(SERIALNO) %>%
  mutate(
    Worker_Total = sum(worker),
    Person_Total = sum(person),
    Child_Total = sum(child)
  ) %>%
  ungroup() %>%
  # Categorizing household sizes by number of persons (1) one person, (2) two persons, (3) three persons, (4) four or more persons
  mutate(
    HHSize = case_when(
      Person_Total == 1 ~ 1,
      Person_Total == 2 ~ 2,
      Person_Total == 3 ~ 3,
      Person_Total >= 4 ~ 4
    ),
    
    # Categorizing worker categories (0) no workers, (1) one worker, (2) two workers, (3) three or more workers
    WRKHH = case_when(Worker_Total == 0 ~ 0,
                      Worker_Total == 1 ~ 1,
                      Worker_Total == 2 ~ 2,
                      Worker_Total >= 3 ~ 3),
    
    # Determine if person is not hhder vs hhder of 0- wrk, 1wrk, 2 wrk, 3+wrk
    HHder = case_when(
      SPORDER == 1 & WRKHH == 0 ~ 0,
      SPORDER == 1 & WRKHH == 1 ~ 1,
      SPORDER == 1 & WRKHH == 2 ~ 2,
      SPORDER == 1 & WRKHH == 3 ~ 3,
      SPORDER > 1 ~ 99
    ),
    # Determine what type of household the person is heading or not heading (1) head child, (2) child, (3) head not child, (4) not head no child, (5) head single
    HHtype = case_when(
      Child_Total >= 1 & Person_Total > 1 & HHder != 99 ~ 1,
      Child_Total >= 1 & Person_Total > 1 & HHder == 99 ~ 2,
      Child_Total == 0 & Person_Total > 1 & HHder != 99 ~ 3,
      Child_Total == 0 & Person_Total > 1 & HHder == 99 ~ 4,
      Person_Total == 1 & HHder != 99 ~ 5
    )
  ) %>%
  
  #Selecting for variables used in further analysis
  select(TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA00, PUMA10, SEX, eduattn, PAGEC, PRACE, lf, WAGPALLC, HHtype, HHder, WRKHH, HHSize, AGEP)

#Test for NAs in new variables
sapply(PUMS_data_2012, function(x) sum(is.na(x)))

# 2.2 Cleaning PUMA files
#Creates a list of all RPAs
RPA <- c("MAPC", "OCPC", "SRPEDD", "MVC", "NPEDC", "NMCOG", "MVPC", "MRPC", 
         "CMRPC", "FRCOG", "PVPC", "BRPC", "CCC")

puma_00 <- puma_00 %>%
  select(name, all_of(RPA)) %>% #Selects names of relevant variables
  rename(PUMA = name) %>% #Renames variable representing distinct PUMAs
  mutate(PUMA = as.character(PUMA), #Changes the datatype to character
         PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% #Adds leading 0s to match strings with xwalk data
  pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA") %>%
  filter(in_RPA == 1) %>%
  select(-in_RPA)

puma_10 <- puma_10 %>%
  select(name_t, all_of(RPA)) %>% #Selects names of relevant variables
  rename(PUMA = name_t) %>% #Renames variable representing distinct PUMAs
  mutate(PUMA = as.character(PUMA), #Changes the datatype to character
         PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% #Adds leading 0s to match strings with xwalk data
  pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA") %>%
  filter(in_RPA == 1) %>%
  select(-in_RPA) #Assigns a RPA to each PUMA

puma_xwalk <- puma_xwalk %>%
filter(State00 == 25 & State10 == 25) %>% #Filters for MA PUMAs
select(PUMA00, PUMA10) %>% #Selects the PUMA 2000 and 2010 variables
mutate(PUMA00 = as.character(PUMA00),
       PUMA10 = as.character(PUMA10)) %>% #Changes the datatype to character
dplyr::rename(
  PUMA10_new = PUMA10
)

#puma_xwalk_10 <- left_join(puma_xwalk, puma_10, by = c("PUMA10" = "PUMA")) #%>% #Convert all data to 2010 PUMA RPA distributions
#pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA" ) %>% 
#filter(in_RPA == 1) %>%
#select(-in_RPA) #Assigns a RPA to each PUMA

#RPA test - demonstrate differences in PUMA RPA affiliation by 2000 and 2010 PUMAs
#puma_xwalk_00 <- puma_xwalk_00 %>%
#pivot_longer(all_of(RPA), names_to = "RPA", values_to = "count") %>%
#group_by(RPA) %>%
#summarise(sum(count))

#puma_xwalk_10 <- puma_xwalk_10 %>%
#pivot_longer(all_of(RPA), names_to = "RPA", values_to = "count") %>%
#group_by(RPA) %>%
#summarise(sum(count))

################################################################################
# 3. Joining PUMs Vintages + PUMA Crosswalks

#PUMS_data_1 <- left_join(PUMS_data_1, puma_xwalk_10, by = c("PUMA" = "PUMA00")) %>%
#select(-PUMA)
#PUMS_data_2_00 <- PUMS_data_2 %>%
#  filter(!PUMA00 == "000-9") %>%
#  left_join(puma_00, by = c("PUMA00" = "PUMA"))

#PUMS_data_2_10 <- PUMS_data_2 %>%
#  filter(!PUMA10 == "000-9") %>%
#  left_join(puma_10, by = c("PUMA10" = "PUMA"))

###Assigns an RPA to each obs given their PUMA
PUMS_data_2012_puma <- PUMS_data_2012 %>% 
  #Joins the PUMA - RPA xwalk
  left_join(puma_xwalk, by = c("PUMA00")) %>%
  mutate(
    PUMA10_new = coalesce(PUMA10_new, PUMA10)
  ) %>% 
  #Removes depricated 2000 PUMA variable
  select(-PUMA00, -PUMA10) 

PUMS_data_2012_puma_muni <- PUMS_data_2012_puma %>% 
  left_join(
    puma_muni_xwalk, by = c("PUMA10_new" = "PUMACE10")
  )


# 4. Generating Rate data
#
#Rates are generated at the state and RPA level for each topic.
#Needs the rate calc


hhpop_baseline <- PUMS_data %>% 
  filter(
    year == "2019" & TYPE == "1"
  ) %>%  
  mutate(
    PAGEC = as.factor(PAGEC),
    RPA = as.factor(RPA)
  ) %>% 
  group_by(
    RPA
  ) %>% 
  dplyr::count(
    var = PAGEC,
    wt = PWGTP
  ) %>% 
  rename(
    AgeCat = var,
    BASELINE = n
  ) %>% 
  mutate(
    AgeCat = case_when(
      AgeCat == 1 ~ "x == 1",
      AgeCat == 2 ~ "x == 2",
      AgeCat == 3 ~ "x == 3",
      AgeCat == 4 ~ "x == 4",
      AgeCat == 5 ~ "x == 5",
      AgeCat == 6 ~ "x == 6",
      AgeCat == 7 ~ "x == 7",
      AgeCat == 8 ~ "x == 8",
      AgeCat == 9 ~ "x == 9",
      AgeCat == 10 ~ "x == 10",
      AgeCat == 11 ~ "x == 11",
      AgeCat == 12 ~ "x == 12",
      AgeCat == 13 ~ "x == 13",
      AgeCat == 14 ~ "x == 14",
      AgeCat == 15 ~ "x == 15",
      AgeCat == 16 ~ "x == 16",
      AgeCat == 17 ~ "x == 17",
      AgeCat == 18 ~ "x == 18"
    )
  )

hhtype_baseline <- PUMS_data %>% 
  filter(
    year == "2019" & TYPE == "1"
  ) %>%  
  mutate(
    PAGEC = as.factor(PAGEC),
    RPA = as.factor(RPA),
    HHtype = as.factor(HHtype)
  ) %>% 
  group_by(
    RPA,
    PAGEC
  ) %>% 
  dplyr::count(
    var = HHtype,
    wt = PWGTP
  ) %>% 
  rename(
    HHtype = var,
    BASELINE = n,
    AgeCat = PAGEC
  ) %>% 
  mutate(
    HHtype = case_when(
      HHtype == 1 ~ "hhderchild",
      HHtype == 2 ~ "nothhderchild",
      HHtype == 3 ~ "hhdernochild",
      HHtype == 4 ~ "nothhdernochild",
      HHtype == 5 ~ "single"
    ),
    AgeCat = case_when(
      AgeCat == 1 ~ "x == 1",
      AgeCat == 2 ~ "x == 2",
      AgeCat == 3 ~ "x == 3",
      AgeCat == 4 ~ "x == 4",
      AgeCat == 5 ~ "x == 5",
      AgeCat == 6 ~ "x == 6",
      AgeCat == 7 ~ "x == 7",
      AgeCat == 8 ~ "x == 8",
      AgeCat == 9 ~ "x == 9",
      AgeCat == 10 ~ "x == 10",
      AgeCat == 11 ~ "x == 11",
      AgeCat == 12 ~ "x == 12",
      AgeCat == 13 ~ "x == 13",
      AgeCat == 14 ~ "x == 14",
      AgeCat == 15 ~ "x == 15",
      AgeCat == 16 ~ "x == 16",
      AgeCat == 17 ~ "x == 17",
      AgeCat == 18 ~ "x == 18"
    )
  ) %>% 
  dplyr::filter(
    HHtype == "hhderchild" | HHtype == "hhdernochild" | HHtype == "single"
  ) %>% 
  mutate(
    HHtype = case_when(
      HHtype == "hhderchild" ~ "x == 1",
      HHtype == "hhdernochild" ~ "x == 3",
      HHtype == "single" ~ "x == 5"
    )
  )


lf_baseline <- PUMS_data %>% 
  filter(
    year == "2019" & TYPE == "1" & lf == "1"
  ) %>%  
  mutate(
    PAGEC = as.factor(PAGEC),
    RPA = as.factor(RPA),
    eduattn = as.factor(eduattn)
  ) %>% 
  group_by(
    RPA,
    PAGEC
  ) %>% 
  dplyr::count(
    var = eduattn,
    wt = PWGTP
  ) %>% 
  rename(
    eduattn = var,
    BASELINE = n,
    AgeCat = PAGEC
  ) %>% 
  mutate(
    eduattn = case_when(
      eduattn == 1 ~ "High School or less",
      eduattn == 2 ~ "Some college or Associate's degree",
      eduattn == 3 ~ "Bachelor's",
      eduattn == 4 ~ "Master's or higher"
    ),
    AgeCat = case_when(
      AgeCat == 1 ~ "x == 1",
      AgeCat == 2 ~ "x == 2",
      AgeCat == 3 ~ "x == 3",
      AgeCat == 4 ~ "x == 4",
      AgeCat == 5 ~ "x == 5",
      AgeCat == 6 ~ "x == 6",
      AgeCat == 7 ~ "x == 7",
      AgeCat == 8 ~ "x == 8",
      AgeCat == 9 ~ "x == 9",
      AgeCat == 10 ~ "x == 10",
      AgeCat == 11 ~ "x == 11",
      AgeCat == 12 ~ "x == 12",
      AgeCat == 13 ~ "x == 13",
      AgeCat == 14 ~ "x == 14",
      AgeCat == 15 ~ "x == 15",
      AgeCat == 16 ~ "x == 16",
      AgeCat == 17 ~ "x == 17",
      AgeCat == 18 ~ "x == 18"
    )
  ) %>% 
  mutate(
    eduattn = case_when(
      eduattn == "High School or less" ~ "x == 1",
      eduattn == "Some college or Associate's degree" ~ "x == 2",
      eduattn == "Bachelor's" ~ "x == 3",
      eduattn == "Master's or higher" ~ "x == 4"
    )
  )











