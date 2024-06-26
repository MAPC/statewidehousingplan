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
library(magrittr)
library(srvyr)
library(janitor)
library(data.table)
library(easycensus)
library(mapcdatakeys)
library(readxl)
library(gt)
library(tsibble)
library(feasts)

#0.2 Setting Filepaths
default_dir <- "K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/"

HH_Pop_Proj_dir <- "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/"

puma_inputs_dir <- "K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/"

output_dir <- ""

#Remote  Filepaths
#default_dir <- "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Scripts/sProjections 2022 v2050"

#puma_inputs_dir <- "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs"

#output_dir <- ""

#0.3 Setting Tidycensus API key
census_api_key("d590f657a113f2f78b5a422462ea00745e79111c")

#0.4 Load in Household Population Projections
umdi_hh_population_proj <- read.csv(paste0(HH_Pop_Proj_dir,"UMDI_HHPopulation_Projections_Age_Sex_MPO_v1.csv"))
umdi_hh_population_proj %<>%
  mutate(
    Year = as.character(Year),
    Age.Group = as.character(Age.Group)
  )
###############################################################################
### 1. Reading in Datasets

#1.1 Reading in Census Data
#List of ACS PUMS data iterations

pums_list <- c(2019)

#List of census variables

variable_list <- c("PUMA", "TYPE", "SEX", "AGEP", "RAC1P", "HISP", "ESR", "WKHP",
                     "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER", "HINCP")

#Reading in PUMS Data

#2019
PUMS_data_2019 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
},  .id = "year")

#1.2 Reading in PUMA Geographies

puma_10 <- read.csv(paste0(puma_inputs_dir,"PUMA_2010Geog.csv"))

puma_xwalk <- read_excel(paste0(puma_inputs_dir,"PUMA2000_PUMA2010_crosswalk.xlsx"))
################################################################################
# Dataset Cleaning and Variable Generation

# 2.1 Cleaning Decennial and ACS PUMS Datasets and Generating New Variables

#####
#PUMS 2019
PUMS_data_2019 <- PUMS_data_2019 %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA = as.character(PUMA),
    #PUMA = str_pad(PUMA, 5, side = "left", pad = "0"),
    
    #Year ID Adjustment
    #Change year ID value to the actual year
    year = case_when(
      year == "1" ~ "2019"
    ),
    year = as.character(year),
    
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
      ESR == "b" ~ "Under 16 not eligible for work",
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
      ESR == "b" | ESR == 4 | ESR == 5  ~ 0,
      ESR == 1 | ESR == 2 | ESR == 3  ~ 1,
      ESR == 6 ~ 2,
      is.na(ESR) ~ 0
    ),
    
    # Counts as Worker
    # NOTE: Categories are (0) under 16 or not in labor force, or (1) in labor force or non-civilian worker
    worker = case_when(
      (ESR == "b" | ESR == 6) ~ 0,
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
  group_by(year, SERIALNO) %>%
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
    ),
    #Change the datatype of SPORDER to character.
    SPORDER = as.character(SPORDER)
  )  %>%
  
  #Selecting for variables used in further analysis
  select(year, TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA, SEX, eduattn, PAGEC, PRACE, lf, WAGPALLC, HHtype, HHder, WRKHH, HHSize, AGEP, HINCP)

#Testing for NAs in new variables
sapply(PUMS_data_2019, function(x) sum(is.na(x)))

################################################################################

# 2.2 Cleaning PUMA files
#Creates a list of all RPAs
RPA <- c("MAPC", "OCPC", "SRPEDD", "MVC", "NPEDC", "NMCOG", "MVPC", "MRPC", 
         "CMRPC", "FRCOG", "PVPC", "BRPC", "CCC")

puma_10 <- puma_10 %>%
           select(name_t, all_of(RPA)) %>% #Selects names of relevant variables
           rename(PUMA = name_t) %>% #Renames variable representing distinct PUMAs
           mutate(PUMA = as.character(PUMA), #Changes the datatype to character
                  PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% #Adds leading 0s to match strings with xwalk data
           pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA") %>%
           filter(in_RPA == 1) %>%
           select(-in_RPA) #Assigns a RPA to each PUMA
################################################################################
# 3. Joining PUMs Vintages + PUMA Crosswalks
PUMS_data_2019 <- left_join(PUMS_data_2019, puma_10, by = c("PUMA"))

#Binds all dataframes together into a single dataframe
PUMS_data <- PUMS_data_2019

################################################################################
# 4. Generate BASELINE data for the reweighter targeting files
#
#Baseline of people in Households
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

#Basline of Households by Household Type
hhtype_baseline <- PUMS_data %>% 
  filter(
    year == "2019" & TYPE == "1" & SPORDER == "1"
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
    wt = WGTP
  ) %>%
  ungroup() %>% 
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
  ungroup() %>% 
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

write.csv(hhpop_baseline, "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hhpop_baseline.csv", row.names = FALSE)
write.csv(hhtype_baseline, "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hhtype_baseline.csv", row.names = FALSE)
write.csv(lf_baseline, "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/lf_baseline.csv", row.names = FALSE)

################################################################################
# 5. Generate BASELINE data for the reweighter targeting files
#
#5.1.1 Labor Force Participation by Sex, Age, Educational Attainment
#STATE LEVEL
PUMS_LF_PAGEC_SEX_EDUATTN_STATE <- PUMS_data %>%
  #Takes only observations that are in housing units and in the labor force.
  filter(TYPE == 1 & lf >= 1) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by age group, by sex, by educational attainment) 
  group_by(year, PAGEC, SEX, eduattn) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.
  count(var = lf, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"),
    EDUATTN = case_when(
      eduattn == 1 ~ "High School or less",
      eduattn == 2 ~ "Some college or Associate's degree",
      eduattn == 3 ~ "Bachelor's",
      eduattn == 4 ~ "Master's or higher"),
    freq = (n/sum(n))) %>%
  filter(var == "in LF" & year == 2019) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  #Drops the "n" variable 
  select(-n)# %>%
  #Rearranges the dataframe to have each column be a level of labor force participation.
  #and each row be an age sex, and educational attainment group.
  #pivot_wider(names_from = var, values_from = freq)
  #pivot_wider(names_from = var, values_from = n)

#RPA LEVEL
PUMS_pop_by_educ <- PUMS_data %>%
  #Takes only observations that are in housing units and in the labor force.
  filter(TYPE == 1 & lf >= 1) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by RPA, by age group, by sex, by educational attainment) 
  group_by(year, RPA, PAGEC, SEX) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.
  count(var = eduattn, wt = PWGTP) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  #Recodes factor variables as the categories they represent.
  mutate(
    var = case_when(
      var == 1 ~ "High School or less",
      var == 2 ~ "Some college or Associate's degree",
      var == 3 ~ "Bachelor's",
      var == 4 ~ "Master's or higher"),
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"),
  ) %>%
  group_by(
    year, RPA, PAGEC, SEX
  ) %>% 
  #Converts numbers into frequencies for those in and not in the laborforce, based
  #on the demographic variables.
  mutate(
    freq = (n/sum(n))
  ) %>% 
  ungroup() %>% 
  filter(year == 2019) %>%
  #Drops the "n" variable 
  select(-n)

PUMS_LF_PAGEC_SEX_EDUATTN_RPA <- PUMS_data %>%
  #Takes only observations that are in housing units and in the labor force.
  filter(TYPE == 1 & lf >= 1) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by RPA, by age group, by sex, by educational attainment) 
  group_by(year, RPA, PAGEC, eduattn, SEX) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.
  count(var = lf, wt = PWGTP) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"),
    EDUATTN = case_when(
      eduattn == 1 ~ "High School or less",
      eduattn == 2 ~ "Some college or Associate's degree",
      eduattn == 3 ~ "Bachelor's",
      eduattn == 4 ~ "Master's or higher")
  ) %>%
  group_by(
    year, RPA, PAGEC, SEX, EDUATTN
  ) %>% 
  #Converts numbers into frequencies for those in and not in the laborforce, based
  #on the demographic variables.
  mutate(
    freq = (n/sum(n))
  ) %>% 
  ungroup() %>% 
  filter(var == "in LF" & year == 2019) %>%
  #Drops the "n" variable 
  select(-n) #%>%
  #Rearranges the dataframe to have each column be a level of labor force participation.
  #and each row be an age sex, and educational attainment group.
  #pivot_wider(names_from = var, values_from = freq)
  #pivot_wider(names_from = var, values_from = n)

#Bring in Labor Force Participation Rate Adjustment 
lfpr_adj <- read_excel(
  paste0(HH_Pop_Proj_dir,"LFPR_adjustment.xlsx"),
  sheet = 1
  ) %>%
  mutate(
    age_cat = as.factor(age_cat)
  )

#Join adjustment to PUMS laborforce participation rate calculations
PUMS_LF_PAGEC_SEX_EDUATTN_RPA <- left_join(PUMS_LF_PAGEC_SEX_EDUATTN_RPA, lfpr_adj, by = c("PAGEC" = "age_cat", "SEX" = "sex"))

PUMS_LF_PAGEC_SEX_EDUATTN_RPA <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>%
  mutate(static_rate = freq,
         LFPR_adj_2020 = freq+adj_2020,
         LFPR_adj_2020 = if_else(LFPR_adj_2020 < 0, 0, LFPR_adj_2020),
         LFPR_adj_2020 = if_else(LFPR_adj_2020 > 1.0, 1.0, LFPR_adj_2020),
         LFPR_adj_2030_2050 = LFPR_adj_2020+adj_2030_2050,
         LFPR_adj_2030_2050 = if_else(LFPR_adj_2030_2050 < 0, 0, LFPR_adj_2030_2050),
         LFPR_adj_2030_2050 = if_else(LFPR_adj_2030_2050 > 1.0, 1.0, LFPR_adj_2030_2050)) %>%
  select(RPA, PAGEC, SEX, EDUATTN, static_rate, LFPR_adj_2020, LFPR_adj_2030_2050)

lfpr_2000 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, static_rate
  ) %>% 
  dplyr::rename(
    lfpr = static_rate
  ) %>% 
  mutate(
    Year = 2000
  )

lfpr_2010 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, static_rate
  )  %>% 
  dplyr::rename(
    lfpr = static_rate
  ) %>%  
  mutate(
    Year = 2010
  )

lfpr_2020 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, LFPR_adj_2020
  ) %>% 
  dplyr::rename(
    lfpr = LFPR_adj_2020
  ) %>% 
  mutate(
    Year = 2020
  )

lfpr_2030 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, LFPR_adj_2030_2050
  ) %>% 
  dplyr::rename(
    lfpr = LFPR_adj_2030_2050
  ) %>%  
  mutate(
    Year = 2030
  )

lfpr_2040 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, LFPR_adj_2030_2050
  ) %>% 
  dplyr::rename(
    lfpr = LFPR_adj_2030_2050
  ) %>% 
  mutate(
    Year = 2040
  )

lfpr_2050 <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>% 
  select(
    RPA, PAGEC, SEX, EDUATTN, LFPR_adj_2030_2050
  ) %>% 
  dplyr::rename(
    lfpr = LFPR_adj_2030_2050
  ) %>%  
  mutate(
    Year = 2050
  )

lfpr <- bind_rows(lfpr_2000,lfpr_2010,lfpr_2020,lfpr_2030,lfpr_2040,lfpr_2050) %>% mutate(Year = as.character(Year))

#Determine how many people are in each educational attainment bracket
umdi_pop_educ <- left_join(umdi_hh_population_proj, PUMS_pop_by_educ, by = c("MPO" = "RPA", "Sex"  = "SEX", "Age.Group" = "PAGEC"))

umdi_pop_educ <- umdi_pop_educ %>% 
  filter(
    Age.Group != 1 & Age.Group != 2 & Age.Group != 3
  ) %>% 
  mutate(
    HH_Population_Educ = HH_Population*freq
  ) %>% 
  select(
    -c(
      HH_Population,
      year,
      freq
    )
  ) %>% 
  dplyr::rename(
    EDUATTN = var
  ) %>% 
  relocate(
    EDUATTN,
    .before = HH_Population_Educ
  )

# Now Join the laborforce participation rate to the household population number by 
umdi_pop_educ_lf <- left_join(umdi_pop_educ, lfpr, by = c("Year", "MPO" = "RPA", "Sex"  = "SEX", "Age.Group" = "PAGEC", "EDUATTN"))

umdi_pop_educ_lf_cleaned <- umdi_pop_educ_lf %>% 
  mutate(
    lf = HH_Population_Educ*lfpr
  ) %>%
  select(
    -c(
      HH_Population_Educ,
      lfpr
    )
  ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = lf
  ) %>% 
  dplyr::rename(
    lf_2000 = `2000`,
    lf_2010 = `2010`,
    lf_2020 = `2020`,
    lf_2030 = `2030`,
    lf_2040 = `2040`,
    lf_2050 = `2050`
  )

write.csv(umdi_pop_educ_lf_cleaned, "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/lf_age_sex_educ_cleaned_v3.22.23.csv",
          row.names = FALSE)

#5.2 Household Type by Age

#RPA LEVEL
PUMS_HHtype_PAGEC_RPA <- PUMS_data %>%
  #Inclues only observations for people in households (omits GQ)                       
  filter(TYPE == 1) %>%
  #Groups by variables in the dataframe necessary to getting the granularity of
  #data we're interested in.
  group_by(year, RPA, PAGEC) %>%
  #Computes the weighted sum of individuals in each HH type category by the groups
  #assigned above.
  count(var = HHtype, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                       
  mutate(
    var = case_when(
      var == 1 ~ "hhderchild",
      var == 2 ~ "nothhderchild",
      var == 3 ~ "hhdernochild",
      var == 4 ~ "nothhdernochild",
      var == 5 ~ "single"),
    freq = (n/sum(n))) %>%
  #Selects the appropriate year of data
  filter(year == 2019) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup()# %>%
  #Drops the variable that represents the number of observations from each category
  #select(-n) #%>%
  #Pivots the dataframe to a wide format 
  #pivot_wider(names_from = var, values_from = freq)
  #pivot_wider(names_from = var, values_from = freq)

#write.csv(PUMS_HHtype_PAGEC_RPA,
          #"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hhtype_rates_v1.csv")
write.csv(PUMS_HHtype_PAGEC_RPA,
          "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Alternative_Scenarios/MAPC_General_HHd_Target/Projected_HeadshipRate_Adjustements/ACSPUMS_15_19_hhtype_rates_v03.22.23.csv",
          row.names = FALSE)
PUMS_HHtype_PAGEC_RPA <- read.csv(
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Alternative_Scenarios/MAPC_General_HHd_Target/Projected_HeadshipRate_Adjustements/ACSPUMS_15_19_hhtype_rates_v03.22.23_ADJUSTED.csv"
) %>% 
  select(
    -c(
      freq,
      adjustment,
      Ignore
    )
  ) %>% 
  dplyr::rename(
    freq = freq_adj
  )


umdi_hh_population_projs_age <- umdi_hh_population_proj %>%
  group_by(Year, MPO, Age.Group) %>%
  summarise(
    HH_Population = sum(HH_Population)
  ) %>% 
  ungroup()

umdi_pop_hhtype_age <- left_join(umdi_hh_population_projs_age, PUMS_HHtype_PAGEC_RPA, by = c("MPO" = "RPA", "Age.Group" = "PAGEC"))

umdi_pop_hhtype_age_cleaned <- umdi_pop_hhtype_age %>% 
  dplyr::rename(
    HH_type = var
  ) %>%
  rowwise() %>% 
  mutate(
    Households = HH_Population*freq
  ) %>% 
  select(
    -c(
      year,
      freq,
      HH_Population
    )
  ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = Households
  ) %>% 
  dplyr::rename(
    proj_hh_2000 = `2000`,
    proj_hh_2010 = `2010`,
    proj_hh_2020 = `2020`,
    proj_hh_2030 = `2030`,
    proj_hh_2040 = `2040`,
    proj_hh_2050 = `2050`
  )


write.csv(umdi_pop_hhtype_age_cleaned,
          "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hhtype_age_PROJECTIONS_v03.22.23.csv",
          row.names = FALSE)

PUMS_HHtype_PAGEC_SEX_RPA <- PUMS_data %>%
  #Inclues only observations for people in households (omits GQ)                       
  filter(TYPE == 1) %>%
  #Groups by variables in the dataframe necessary to getting the granularity of
  #data we're interested in.
  group_by(year, RPA, PAGEC, SEX) %>%
  #Computes the weighted sum of individuals in each HH type category by the groups
  #assigned above.
  count(var = HHtype, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                       
  mutate(
    var = case_when(
      var == 1 ~ "hhderchild",
      var == 2 ~ "nothhderchild",
      var == 3 ~ "hhdernochild",
      var == 4 ~ "nothhdernochild",
      var == 5 ~ "single"
    ),
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"
    ),
    freq = (n/sum(n))) %>%
  #Selects the appropriate year of data
  filter(year == 2019) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  #Drops the variable that represents the number of observations from each category
  select(-n) #%>%
#Pivots the dataframe to a wide format 
#pivot_wider(names_from = var, values_from = freq)
#pivot_wider(names_from = var, values_from = freq)


umdi_pop_hhtype_age_sex <- left_join(umdi_hh_population_proj, PUMS_HHtype_PAGEC_SEX_RPA, by = c("MPO" = "RPA", "Age.Group" = "PAGEC", "Sex" = "SEX"))

umdi_pop_hhtype_age_sex_cleaned <- umdi_pop_hhtype_age_sex %>% 
  dplyr::rename(
    HH_type = var
  ) %>%
  rowwise() %>% 
  mutate(
    Households = HH_Population*freq
  ) %>% 
  select(
    -c(
      year,
      freq,
      HH_Population
    )
  ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = Households
  ) %>% 
  dplyr::rename(
    proj_hh_2000 = `2000`,
    proj_hh_2010 = `2010`,
    proj_hh_2020 = `2020`,
    proj_hh_2030 = `2030`,
    proj_hh_2040 = `2040`,
    proj_hh_2050 = `2050`
  )

write.csv(umdi_pop_hhtype_age_sex_cleaned,
          "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hhtype_age_sex_PROJECTIONS_v03.22.23.csv")
################################################################################
### 6. Descriptive Statistics and Graphical Representation
# Create summary statistics and graphs that demonstrate trends in existing data
# to better understand how it is structured and changes over time. Also, compares
# data collected from decennial survey and ACS survey data from the same years to
# understand and address any outlying differences between the two data sources that
# may be relevant to the projections and forecasting work being undertaken.

### 5.1 ACS Descriptive Statistics & Graphical Representation

# 5.1.1 ACS State-Level Descriptive Statistics & Graphs

PUMS_HHtype_PAGEC_STATE %>%
  ggplot(aes(x = as.Date(year, format = "%Y"), y = single)) +
  geom_line() +
  facet_wrap(~PAGEC)

PUMS_HHtype_PAGEC_RPA %>%
  ggplot(aes(x = as.Date(year, format = "%Y"), y = single)) +
  geom_line() +
  facet_wrap(~PAGEC + RPA)

# 5.1.2 ACS RPA-Level Descriptive Statistics & Graphs

### 5.2 Decennial Descriptive Statistics & Graphical Representation

# 5.1.1 Decennial State-Level Descriptive Statistics & Graphs

# 5.1.2 Decennial RPA-Level Descriptive Statistics & Graphs

### 5.3 Harmonized ACS + Decennial Descriptive Statistics and Graphical Representation

################################################################################
### 6. Processing 

################################################################################
### 8. Forecasting

#General Forecasts (demonstrating state and RPA level dynamics irrespective
#of cross-tabs for final projections)


#4.1 Educational Attainment by Sex and Age
#STATE LEVEL
PUMS_EDUATTN_SEX_STATE <- PUMS_data %>%
  #Takes only observations that are housing units, not group quarters
  #filter(TYPE == 1) %>%
  #filter(eduattn != 0) %>%
  filter(AGEP >= 18) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by age group, by sex)
  group_by(year, SEX) %>%
  #Sums up the number of observations using the person weight.
  count(var = eduattn, wt = WGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.
  mutate("Educational Attainment" = case_when(
    var == 1 ~ "High School or less",
    var == 2 ~ "Some college or Associate's degree",
    var == 3 ~ "Bachelor's",
    var == 4 ~ "Master's or higher"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
#Drops the "n" variable
  select(-n) #%>%
#Rearranges the dataframe to have each column be a level of educational attainment
#and each row be an age and sex group.
#pivot_wider(names_from = var, values_from = freq)


PUMS_EDUATTN_SEX_STATE_forecast <- PUMS_EDUATTN_SEX_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(`Educational Attainment`, SEX),
             index = year) %>%
  na.omit()

PUMS_EDUATTN_SEX_STATE_forecast %>%
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  )) %>%
  ggplot(aes(x=year, y=freq, fill = `Educational Attainment`)) +
  geom_area(position = "stack",
            stat = "identity",
            alpha = 0.5) +
  geom_line(position = "stack") +
  facet_wrap(~SEX)  +
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  scale_fill_discrete(limits = c("Master's or higher", "Bachelor's", "Some college or Associate's degree", "High School or less")) +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))

#RPA LEVEL 
PUMS_EDUATTN_SEX_RPA <- PUMS_data %>%
  #Takes only observations that are housing units, not group quarters.                            
  #filter(TYPE == 1) %>%
  #filter(eduattn != 0) %>%
  filter(AGEP >= 18) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by RPA, by age group, by sex)                            
  group_by(year, RPA, SEX) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.                            
  count(var = eduattn, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                            
  mutate("Educational Attainment" = case_when(
    var == 1 ~ "High School or less",
    var == 2 ~ "Some college or Associate's degree",
    var == 3 ~ "Bachelor's",
    var == 4 ~ "Master's or higher"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()                            
  ungroup() %>%
#Drops the "n" variable                           
  select(-n) #%>%
#Rearranges the dataframe to have each column be a level of educational attainment
#and each row be an age and sex group.                            
#pivot_wider(names_from = var, values_from = freq)

PUMS_EDUATTN_SEX_RPA_forecast <- PUMS_EDUATTN_SEX_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(`Educational Attainment`, SEX, RPA),
             index = year) %>%
  na.omit()

PUMS_EDUATTN_SEX_RPA_forecast %>%
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  )) %>%
  filter(RPA == "FRCOG" | RPA == "NPEDC") %>%
  ggplot(aes(x=year, y=freq, fill = `Educational Attainment`)) +
  geom_area(position = "stack",
            stat = "identity",
            alpha = 0.5) +
  geom_line(position = "stack") +
  facet_grid(vars(RPA), vars(SEX)) +
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  scale_fill_discrete(limits = c("Master's or higher", "Bachelor's", "Some college or Associate's degree", "High School or less")) +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14)) #+
  #guides(guide_legend(reverse = TRUE))

#4.2 Labor Force Participation by Sex, Age, Educational Attainment
#STATE LEVEL
PUMS_LF_STATE <- PUMS_data %>%
  #Takes only observations that are in housing units and in the labor force.
  filter(TYPE == 1 & lf >= 1) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by age group, by sex, by educational attainment) 
  group_by(year) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.
  count(var = lf, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
#Drops the "n" variable 
  select(-n) #%>%
#Rearranges the dataframe to have each column be a level of labor force participation.
#and each row be an age sex, and educational attainment group.
#pivot_wider(names_from = var, values_from = freq)

PUMS_LF_STATE_forecast <- PUMS_LF_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(var),
             index = year) %>%
  na.omit()

PUMS_LF_STATE_forecast %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = var))

#RPA LEVEL
PUMS_LF_RPA <- PUMS_data %>%
  #Takes only observations that are in housing units and in the labor force.
  filter(TYPE == 1 & lf >= 1) %>%
  #Sets the level of aggregation (e.g. we want the number of observations by year, by RPA, by age group, by sex, by educational attainment) 
  group_by(year, RPA) %>%
  #Sums up the number of observations by the level of aggregation set above using the person weight.
  count(var = lf, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
#Drops the "n" variable 
  select(-n) #%>%
#Rearranges the dataframe to have each column be a level of labor force participation.
#and each row be an age sex, and educational attainment group.
#pivot_wider(names_from = var, values_from = freq)

#Converting table into a time series object
PUMS_LF_RPA_forecast <- PUMS_LF_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(RPA, var),
             index = year) %>%
  na.omit()

PUMS_LF_RPA_forecast %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = var)) +
  facet_wrap(~RPA)

#4.3 Wages by Sex, Age
#STATE LEVEL
PUMS_WAGES_STATE <- PUMS_data %>%
  filter(TYPE == 1) %>%
  filter(WAGPALLC != 0) %>%
  group_by(year) %>%
  count(var = WAGPALLC, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                            
  mutate(
     "Individual Income" = case_when(
      var == 0 ~ "0",
      var == 1 ~ "0-10k",
      var == 2 ~ "10k-35k",
      var == 3 ~ "35k-50k",
      var == 4 ~ "50k-75k",
      var == 5 ~ "75k-125k",
      var == 6 ~ "Greater than 125k"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() #%>%
#Drops the "n" variable 
  #select(-n) #%>%
#pivot_wider(names_from = var, values_from = freq)

#Converting table into a time series object
PUMS_WAGES_STATE_forecast <- PUMS_WAGES_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(`Individual Income`),
             index = year) %>%
  na.omit()

PUMS_WAGES_STATE_forecast %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = `Individual Income`), lwd = 1.0) +
  scale_fill_discrete(limits = c("Greater than 125k", "75k-125k", "50k-75k", "35k-50k", "10k-35k", "0-10k")) +
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year")+
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))

#RPA LEVEL
PUMS_WAGES_RPA <- PUMS_data %>%
  filter(TYPE == 1) %>%
  group_by(year, RPA) %>%
  count(var = WAGPALLC, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                          
  mutate(
    "Individual Income" = case_when(
      var == 0 ~ "0",
      var == 1 ~ "0-10k",
      var == 2 ~ "10k-35k",
      var == 3 ~ "35k-50k",
      var == 4 ~ "50k-75k",
      var == 5 ~ "75k-125k",
      var == 6 ~ "Greater than 125k"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
#Drops the "n" variable 
 select(-n) #%>%
#pivot_wider(names_from = var, values_from = freq)

#Converting table into a time series object
PUMS_WAGES_RPA_forecast <- PUMS_WAGES_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(RPA, `Individual Income`),
             index = year) %>%
  na.omit()

PUMS_WAGES_RPA_forecast %>%
  filter(`Individual Income` != "0") %>%
  filter(RPA == "CMRPC" | RPA == "BRPC") %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = `Individual Income`), lwd = 1.0) +
  scale_fill_discrete(limits = c("Greater than 125k", "75k-125k", "50k-75k", "35k-50k", "10k-35k", "0-10k")) +
  facet_wrap(~RPA) +
  ylab("Percent (%)") +
  xlab("Year") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))


#4.4 Household Type by Age
#STATE LEVEL
PUMS_HHtype_STATE <- PUMS_data %>%
  filter(TYPE == 1) %>%
  group_by(year) %>%
  count(var = HHtype, wt = WGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                        
  mutate(
    "Household Type" = case_when(
      var == 1 ~ "Head of household with child < 18",
      var == 2 ~ "Other adult in household with children < 18",
      var == 3 ~ "Head of household with no children",
      var == 4 ~ "Other adult in household with no children",
      var == 5 ~ "Lives Alone"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  filter(year != 2020) #%>%
#Drops the "n" variable 
  #select(-n) #%>%
#pivot_wider(names_from = var, values_from = freq)

#Converting table into a time series object
PUMS_HHtype_STATE_forecast <- PUMS_HHtype_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(`Household Type`),
             index = year) %>%
  na.omit()

PUMS_HHtype_STATE_forecast %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = `Household Type`), lwd = 1.0) + 
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))


PUMS_HHtype_STATE %>%
  ggplot(aes(fill = `Household Type`, x = year, y = n)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))


#RPA LEVEL
PUMS_HHtype_RPA <- PUMS_data %>%
  filter(TYPE == 1) %>%
  group_by(year, RPA) %>%
  count(var = HHtype, wt = PWGTP) %>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                       
  mutate(
    "Household Type" = case_when(
      var == 1 ~ "Head of household with child < 18",
      var == 2 ~ "Other adult in household with children < 18",
      var == 3 ~ "Head of household with no children",
      var == 4 ~ "Other adult in household with no children",
      var == 5 ~ "Lives Alone"),
    freq = (n/sum(n))*100) %>%
  #Removes the level of aggregation set earlier by group_by()
  ungroup() %>%
  filter(year != 2020) %>%
#Drops the "n" variable 
  select(-n) #%>%
#pivot_wider(names_from = var, values_from = freq)

#Converting table into a time series object
PUMS_HHtype_RPA_forecast <- PUMS_HHtype_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(RPA, `Household Type`),
             index = year) %>%
  na.omit()

PUMS_HHtype_RPA_forecast %>% 
  filter(RPA == "CCC" | RPA == "MAPC") %>%
  ggplot() +
  geom_line(aes(x=year, y=freq, color = `Household Type`), lwd = 1.0) +
  facet_wrap(~RPA)  + 
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))

#Specific Forecasts (all cross-tabs needed for final projections)
#Convert rate dataframes into time series objects (tsibbles)
PUMS_EDUATTN_SEX_PAGEC_STATE <- PUMS_EDUATTN_SEX_PAGEC_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(PAGEC, SEX, var),
             index = year)

PUMS_EDUATTN_SEX_PAGEC_RPA <- PUMS_EDUATTN_SEX_PAGEC_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(RPA, PAGEC, SEX, var),
             index = year)

PUMS_HHtype_PAGEC_STATE <- PUMS_HHtype_PAGEC_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(PAGEC, var),
             index = year)

PUMS_HHtype_PAGEC_RPA <- PUMS_HHtype_PAGEC_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(PAGEC, RPA, var),
             index = year)

PUMS_LF_PAGEC_SEX_EDUATTN_STATE <- PUMS_LF_PAGEC_SEX_EDUATTN_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  select(-eduattn) %>%
  as_tsibble(key = c(PAGEC, SEX, EDUATTN, var),
             index = year)

PUMS_LF_PAGEC_SEX_EDUATTN_RPA <- PUMS_LF_PAGEC_SEX_EDUATTN_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  select(-eduattn) %>%
  as_tsibble(key = c(PAGEC, SEX, EDUATTN, RPA, var),
             index = year)

PUMS_WAGES_SEX_PAGEC_STATE <- PUMS_WAGES_SEX_PAGEC_STATE %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(PAGEC, SEX, var),
             index = year)

PUMS_WAGES_SEX_PAGEC_RPA <- PUMS_WAGES_SEX_PAGEC_RPA %>%
  mutate(year = as.Date(year, format = "%Y")) %>%
  as_tsibble(key = c(PAGEC, SEX, RPA, var),
             index = year)

#Cool, now the rate tables are time series objects and we can do some diagnostics.
#Plot Autocorrelation function (ACF) charts.
################################################################################
### 7.



################################################################################
### Supplemental

#ACS 2016-2020 5-YEAR PUMS Testing

#Labor Force Comparison Data (from BLS/FRED)

lfpr_PUMS <- PUMS_data %>%
  filter(TYPE == 1 & lf >= 1) %>%
  group_by(year) %>%
  count(var = lf, wt = PWGTP) %>%
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    freq = (n/sum(n))*100) %>%
  filter(var == "in LF")

fredr::fredr_set_key("8bdd1423b21c9e0bc762d1d259e7fecc")

lfpr_FRED <- fredr::fredr(
  series_id = "LBSSA25",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2022-03-01")
)


#Not too happy with this. I trust the BLS data more so than the PUMS data, especially since the start of the pandemic.
#Not a fan of the rate of change over the 2012-2019 period in PUMS. LFPR was not decreasing. It was growing substantially and levels were commensurate with pre-GR numbers.

#Pull in CPS data to create labor force participation rates for the state by age sex educational attainment to produce adjustment factors for PUMS LFPR data.
library(cpsR)

cps_years_1 <- 1994:2021
cps_years_2 <- 1994:2022

cps_varlist <- c("pwsswgt", "pwcmpwgt", "pemlr", "peeduca", "pesex", "prtage", "gestfips")

CPS_data_Jan <- map_dfr(cps_years_2, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 1
  )
},  .id = "year")

CPS_data_Feb <- map_dfr(cps_years_2, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 2
  )
},  .id = "year")

cps_mar_years_1 <- 1994:2018
cps_mar_years_2 <- 2019:2022

CPS_data_Mar_1 <- map_dfr(cps_mar_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 3
  )
},  .id = "year")

CPS_data_Mar_2 <- map_dfr(cps_mar_years_2, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 3
  )
},  .id = "year")

CPS_data_Apr <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 4
  )
},  .id = "year")

CPS_data_May <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 5
  )
},  .id = "year")

CPS_data_Jun <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 6
  )
},  .id = "year")

CPS_data_Jul <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 7
  )
},  .id = "year")

CPS_data_Aug <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 8
  )
},  .id = "year")

cps_sep_years_1 <- 1994:2015
cps_sep_years_2 <- 2016:2021

CPS_data_Sep_1 <- map_dfr(cps_sep_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 9
  )
},  .id = "year")

CPS_data_Sep_2 <- map_dfr(cps_sep_years_2, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 9
  )
},  .id = "year")

CPS_data_Oct <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 10
  )
},  .id = "year")

CPS_data_Nov <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 11
  )
},  .id = "year")

CPS_data_Dec <- map_dfr(cps_years_1, ~{
  get_basic(
    vars = cps_varlist,
    year = .x,
    month = 12
  )
},  .id = "year")

CPS_data_Jan <- CPS_data_Jan %>%
  mutate(month = "01") %>%
  filter(gestfips == "25")

CPS_data_Feb <- CPS_data_Feb %>%
  mutate(month = "02") %>%
  filter(gestfips == "25")

CPS_data_Mar_1 <- CPS_data_Mar_1 %>%
  mutate(month = "03",
         pemlr = as.character(pemlr)) %>%
  filter(gestfips == "25")

CPS_data_Mar_2 <- CPS_data_Mar_2 %>%
  mutate(month = "03") %>%
  filter(gestfips == "25")

CPS_data_Apr <- CPS_data_Apr %>%
  mutate(month = "04") %>%
  filter(gestfips == "25")

CPS_data_May <- CPS_data_May %>%
  mutate(month = "05") %>%
  filter(gestfips == "25")

CPS_data_Jun <- CPS_data_Jun %>%
  mutate(month = "06") %>%
  filter(gestfips == "25")

CPS_data_Jul <- CPS_data_Jul %>%
  mutate(month = "07") %>%
  filter(gestfips == "25")

CPS_data_Aug <- CPS_data_Aug %>%
  mutate(month = "08") %>%
  filter(gestfips == "25")

CPS_data_Sep_1 <- CPS_data_Sep_1 %>%
  mutate(month = "09",
         pemlr = as.character(pemlr)) %>%
  filter(gestfips == "25")

CPS_data_Sep_2 <- CPS_data_Sep_2 %>%
  mutate(month = "09") %>%
  filter(gestfips == "25")

CPS_data_Oct <- CPS_data_Oct %>%
  mutate(month = "10") %>%
  filter(gestfips == "25")

CPS_data_Nov <- CPS_data_Nov %>%
  mutate(month = "11") %>%
  filter(gestfips == "25")

CPS_data_Dec <- CPS_data_Dec %>%
  mutate(month = "12") %>%
  filter(gestfips == "25")

CPS_data_Mar_2 <- CPS_data_Mar_2 %>%
  mutate(
    year = case_when(
      year == "1" ~ "26",
      year == "2" ~ "27",
      year == "3" ~ "28",
      year == "4" ~ "29"
    )
  )

CPS_data_Sep_2 <- CPS_data_Sep_2 %>%
  mutate(
    year = case_when(
      year == "1" ~ "24",
      year == "2" ~ "25",
      year == "3" ~ "26",
      year == "4" ~ "27",
      year == "5" ~ "28",
      year == "6" ~ "29"
    )
  )

CPS_data <- rbind(CPS_data_Jan, CPS_data_Feb, CPS_data_Mar_1, CPS_data_Mar_2, CPS_data_Apr, CPS_data_May, CPS_data_Jun,
                  CPS_data_Jul, CPS_data_Aug, CPS_data_Sep_1, CPS_data_Sep_2, CPS_data_Oct, CPS_data_Nov, CPS_data_Dec)

CPS_data <- CPS_data %>%
  mutate(
    year = case_when(
      year == "1" ~ 1994,
      year == "2" ~ 1995,
      year == "3" ~ 1996,
      year == "4" ~ 1997,
      year == "5" ~ 1998,
      year == "6" ~ 1999,
      year == "7" ~ 2000,
      year == "8" ~ 2001,
      year == "9" ~ 2002,
      year == "10" ~ 2003,
      year == "11" ~ 2004,
      year == "12" ~ 2005,
      year == "13" ~ 2006,
      year == "14" ~ 2007,
      year == "15" ~ 2008,
      year == "16" ~ 2009,
      year == "17" ~ 2010,
      year == "18" ~ 2011,
      year == "19" ~ 2012,
      year == "20" ~ 2013,
      year == "21" ~ 2014,
      year == "22" ~ 2015,
      year == "23" ~ 2016,
      year == "24" ~ 2017,
      year == "25" ~ 2018,
      year == "26" ~ 2019,
      year == "27" ~ 2020,
      year == "28" ~ 2021,
      year == "29" ~ 2022),
    educ_attn = case_when(
      peeduca <= 39 ~ 1,
      peeduca %in% 40:42 ~ 2,
      peeduca == 43 ~ 3,
      peeduca >= 44 ~ 4,
      peeduca == -1 ~ 1,
      is.na(peeduca) ~ 1),
    PAGEC =
      cut(prtage,
          breaks = c(-Inf, seq(4, 84, 5), Inf),
          labels = 1:18),
    lf = case_when(
      pemlr == 1 | pemlr == 2 | pemlr == 4 ~ 1,
      pemlr == 3 | pemlr == 5 | pemlr == 6 | pemlr == 7 ~ 2,
      pemlr == -1 ~ 2,
      is.na(pemlr) ~ 2
    )
  )

lfpr_CPS <- CPS_data %>%
  filter(lf >= 1 & prtage > 16) %>%
  mutate(date_1 = paste(year, month, "01", sep = "-")) %>%
  group_by(date_1) %>%
  count(var = lf, wt = pwcmpwgt) %>%
  mutate(
    var = case_when(
      var == 1 ~ "in LF",
      var == 2 ~ "not in LF"),
    freq = (n/sum(n))*100) %>%
  filter(var == "in LF")

##
lfpr_FRED_post09 <- lfpr_FRED[186:314,]
##

lfpr_FRED %>%
  ggplot(aes(x=date, y=value, color = "lfpr_MA")) +
  geom_line(lwd = 1.0) +
  geom_line(data = lfpr_PUMS,
            mapping = aes(x=as.Date(year, format = "%Y"), y = freq, color = "lfpr_PUMS"),
            lwd = 1.0) +
  geom_line(data = lfpr_CPS,
            mapping = aes(x = as.Date(date_1, format = "%Y-%m-%d"), y = freq, color = "lfpr_CPS"),
            lwd = 1.0) +
  geom_hline(yintercept = max(lfpr_FRED_post09$value), color = "blue", linetype = 2) +
  geom_hline(yintercept = min(lfpr_FRED_post09$value), color = "blue", linetype = 2) +
  scale_color_manual(name = "Data Series",
                     breaks = c("lfpr_FRED", "lfpr_PUMS", "lfpr_CPS"),
                     values = c("lfpr_FRED" = "black", "lfpr_PUMS" = "red", "lfpr_CPS" = "green")) +
  labs(title = "") +
  ylab("Percent (%)") +
  xlab("Year") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",  fill = NA, size = 1),
        panel.grid = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14))

#Experimenting with loading the PUMS data as a survey object

#2020
PUMS_data_6 <- map_dfr(pums_list_6, ~{
  get_pums(
    variables = variable_list_4,
    state = "MA",
    survey = "acs5",
    year = .x,
    rep_weights = "person"
  )
},  .id = "year")

#PUMS 2020
PUMS_data_6 <- PUMS_data_6 %>%
  rename(TYPE = TYPEHUGQ) %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA = as.character(PUMA),
    PUMA = str_pad(PUMA, 5, side = "left", pad = "0"),
    
    #Year ID Adjustment
    #Change year ID value to the actual year
    year = case_when(
      year == "1" ~ "2020"
    ),
    year = as.character(year),
    
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
  group_by(year, SERIALNO) %>%
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
    ),
    #Change the datatype of SPORDER to character.
    SPORDER = as.character(SPORDER)
  )  #%>%

#Selecting for variables used in further analysis
#select(year, TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA, SEX, eduattn, PAGEC, PRACE, lf, WAGPALLC, HHtype, HHder, WRKHH, HHSize)

PUMS_data_6_survey <- to_survey(PUMS_data_6)

#4.4 Household Type by Age
#STATE LEVEL
PUMS_HHtype_PAGEC_STATE <- PUMS_data_6_survey %>%
  
  filter(TYPE == 1) %>%
  
  group_by(year, PAGEC) %>%
  
  survey_count(HHtype) #%>%
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                        
  dplyr::mutate(
    #HHtype = case_when(
      #HHtype == 1 ~ "hhderchild",
      #HHtype == 2 ~ "nothhderchild",
      #HHtype == 3 ~ "hhdernochild",
      #HHtype == 4 ~ "nothhdernochild",
      #HHtype == 5 ~ "single"),
    upper_bound = n + n_se))
    #lower_bound = n - n_se,
    #point_freq = (n/sum(n))*100,
    #upper_freq = (upper_bound/sum(upper_bound))*100,
    #lower_freq = (lower_bound/sum(lower_bound))*100) #%>%
  #Removes the level of aggregation set earlier by group_by()
  #ungroup() #%>%
#Drops the "n" variable 
#select(-n) #%>%

#pivot_wider(names_from = var, values_from = freq)
################################################################################
### Improvements to make

# 1. Loop the data cleaning code
# 2. Generate survey std. errors
# 3. PUMA vintage harmonization
