### Format Targets for Reweighter from Projections
### Author: Brandon Stanaway
### Date: 08/05/2022
### Purpose:Takes output from projections methodology to create target files for the reweighter
### Same purpose as excel file K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Inputs/HH_Targets/Reweighter_Targets_091020/Format_V2Projections_forControls.xlsx
### Targets are AgeHHder_HHtype, Pop_Age, Age_Edu_LF

rm(list=ls())
#install.packages("pacman")
pacman::p_load(tidyverse, plyr, dplyr,data.table, magrittr, readxl)

options(scipen = 999)

###Set directory paths for working remotely or in the office
#Remote
#inpath <- 'S:/Network Shares/DS Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'

#In-office
inpath <- 'K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'

#Remote
#outpath <- 'S:/Network Shares/DS Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'

#In-office
outpath <- 'K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'

#Remote
#reweighter_path <- 'S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Input Files/'

#In-office
reweighter_path <- 'K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/'

#Read in baseline files created in the "Projections 2050 Mastercode" R script
temp_hhd_proj <- fread(paste0(inpath,'hhtype_baseline.csv'))
temp_lf_proj <-  fread(paste0(inpath,'lf_baseline.csv'))
temp_pop_proj <-  fread(paste0(inpath,'hhpop_baseline.csv'))

#Depricated?
#temp_hhd_proj[HHtype=='x == 1', HHtype:='x == 5']
#temp_hhd_proj[HHtype=='x == 3', HHtype:='x == 1']
#temp_hhd_proj[HHtype=='x == 2', HHtype:='x == 3']


###HHd Projections####
hhd_proj <- read.csv(paste0(outpath,'hhtype_age_PROJECTIONS_v03.22.23.csv'))

hhd_proj %<>%
  filter(
    HH_type != "nothhderchild" & HH_type != "nothhdernochild"
  ) %>% 
  replace(
    is.na(.), 0
  ) %>% 
  dplyr::rename(
    RPA = MPO,
    AgeCat = Age.Group
  ) %>% 
  mutate(
    HH_type = case_when(
      HH_type == "hhderchild" ~ "x == 1",
      HH_type == "hhdernochild" ~ "x == 3",
      HH_type == "single" ~ "x == 5",
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
  )

temp_hhd_proj %<>%
  mutate(
    INTER = BASELINE
  ) %>% 
  select(
    RPA,
    AgeCat,
    HHtype,
    BASELINE,
    INTER
  )

hhd_proj_all <- left_join(hhd_proj, temp_hhd_proj, by = c("RPA", "AgeCat", "HH_type" = "HHtype"))

hhd_proj_all %<>%
  dplyr::rename(
    ageCAT3 = AgeCat
  ) %>% 
  replace(
    is.na(.), 0
  )

V2_hhd_2010f <- hhd_proj_all %>% 
  select(
   RPA,
   ageCAT3,
   HH_type,
   proj_hh_2010,
   BASELINE,
   INTER
  ) %>% 
  dplyr::rename(
    HHtype = HH_type,
    TARGET = proj_hh_2010
  )

write.csv(V2_hhd_2010f,
          paste0(reweighter_path, "AgeHHder_HHtype_2010_V03.22.23.csv"),
          row.names = FALSE)
  
V2_hhd_2020f <- hhd_proj_all %>% 
  select(
    RPA,
    ageCAT3,
    HH_type,
    proj_hh_2020,
    BASELINE,
    INTER
  ) %>% 
  dplyr::rename(
    HHtype = HH_type,
    TARGET = proj_hh_2020
  )

write.csv(V2_hhd_2020f,
          paste0(reweighter_path, "AgeHHder_HHtype_2020_V03.22.23.csv"),
          row.names = FALSE)

V2_hhd_2030f <- hhd_proj_all %>% 
  select(
    RPA,
    ageCAT3,
    HH_type,
    proj_hh_2030,
    BASELINE,
    INTER
  ) %>% 
  dplyr::rename(
    HHtype = HH_type,
    TARGET = proj_hh_2030
  )

write.csv(V2_hhd_2030f,
          paste0(reweighter_path, "AgeHHder_HHtype_2030_V03.22.23.csv"),
          row.names = FALSE)

V2_hhd_2040f <- hhd_proj_all %>% 
  select(
    RPA,
    ageCAT3,
    HH_type,
    proj_hh_2040,
    BASELINE,
    INTER
  ) %>% 
  dplyr::rename(
    HHtype = HH_type,
    TARGET = proj_hh_2040
  )

write.csv(V2_hhd_2040f,
          paste0(reweighter_path, "AgeHHder_HHtype_2040_V03.22.23.csv"),
          row.names = FALSE)

V2_hhd_2050f <- hhd_proj_all %>% 
  select(
    RPA,
    ageCAT3,
    HH_type,
    proj_hh_2050,
    BASELINE,
    INTER
  ) %>% 
  dplyr::rename(
    HHtype = HH_type,
    TARGET = proj_hh_2050
  )

write.csv(V2_hhd_2050f,
          paste0(reweighter_path, "AgeHHder_HHtype_2050_V03.22.23.csv"),
          row.names = FALSE)

####LF Projections####

setwd(outpath)
lf_projections <- read_excel('lf_age_educ_v1.xlsx',
                             sheet = 1)

lf_projections %<>%
  replace(
    is.na(.),
    0
  ) %>% 
  dplyr::rename(
    RPA = MPO,
    Year = PRED_YEAR,
    AgeCat = age_cat
  )

lf_proj1 <- lf_projections %>% 
  select(
    RPA,
    Year,
    AgeCat,
    `High School or less Male in LF`,
    `High School or less Female in LF`
  ) %>% 
  mutate(
    TARGET = `High School or less Male in LF` + `High School or less Female in LF`,
    edu = "x == 1",
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
  select(
    -`High School or less Male in LF`,
    -`High School or less Female in LF`
  )

lf_proj2 <- lf_projections %>% 
  select(
    RPA,
    Year,
    AgeCat,
    `Some college or Associate's degree Male in LF`,
    `Some college or Associate's degree Female in LF`
  ) %>% 
  mutate(
    TARGET = `Some college or Associate's degree Male in LF` + `Some college or Associate's degree Female in LF`,
    edu = "x == 2",
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
  select(
    -`Some college or Associate's degree Male in LF`,
    -`Some college or Associate's degree Female in LF`
  )

lf_proj3 <- lf_projections %>% 
  select(
    RPA,
    Year,
    AgeCat,
    `Bachelor's Male in LF`,
    `Bachelor's Female in LF`
  ) %>% 
  mutate(
    TARGET = `Bachelor's Male in LF` + `Bachelor's Female in LF`,
    edu = "x == 3",
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
  select(
    -`Bachelor's Male in LF`,
    -`Bachelor's Female in LF`
  )


lf_proj4 <- lf_projections %>% 
  select(
    RPA,
    Year,
    AgeCat,
    `Master's or higher Male in LF`,
    `Master's or higher Female in LF`
  ) %>% 
  mutate(
    TARGET = `Master's or higher Male in LF` + `Master's or higher Female in LF`,
    edu = "x == 4",
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
  select(
    -`Master's or higher Male in LF`,
    -`Master's or higher Female in LF`
  )

lf_proj <- rbind(lf_proj1, lf_proj2, lf_proj3, lf_proj4)

lf_proj %<>%
  dplyr::rename(
    ageCAT3 = AgeCat
  )

temp_lf_proj %<>%
  mutate(
    INTER = BASELINE,
    lf = "x == 1"
  ) %>%
  dplyr::rename(
    edu = eduattn
  ) %>% 
  select(
    RPA,
    AgeCat,
    edu,
    BASELINE,
    INTER,
    lf
  )

lf_proj_all <- left_join(lf_proj, temp_lf_proj, by = c("RPA", "ageCAT3" = "AgeCat", "edu"))

V2_lf_2010f <- lf_proj_all %>% 
  filter(
    Year == 2010
  ) %>% 
  select(
    RPA,
    ageCAT3,
    edu,
    lf,
    BASELINE,
    TARGET
  )

write.csv(V2_lf_2010f,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/LF_Age_Edu_2010_V2.csv",
          row.names = FALSE
)

V2_lf_2020f <- lf_proj_all %>% 
  filter(
    Year == 2020
  ) %>% 
  select(
    RPA,
    ageCAT3,
    edu,
    lf,
    BASELINE,
    TARGET
  )

write.csv(V2_lf_2020f,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/LF_Age_Edu_2020_V2.csv",
          row.names = FALSE
)

V2_lf_2030f <- lf_proj_all %>% 
  filter(
    Year == 2030
  ) %>% 
  select(
    RPA,
    ageCAT3,
    edu,
    lf,
    BASELINE,
    TARGET
  )

write.csv(V2_lf_2030f,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/LF_Age_Edu_2030_V2.csv",
          row.names = FALSE
)

V2_lf_2040f <- lf_proj_all %>% 
  filter(
    Year == 2040
  ) %>% 
  select(
    RPA,
    ageCAT3,
    edu,
    lf,
    BASELINE,
    TARGET
  )

write.csv(V2_lf_2040f,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/LF_Age_Edu_2040_V2.csv",
          row.names = FALSE
)

V2_lf_2050f <- lf_proj_all %>% 
  filter(
    Year == 2050
  ) %>% 
  select(
    RPA,
    ageCAT3,
    edu,
    lf,
    BASELINE,
    TARGET
  )

write.csv(V2_lf_2050f,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/LF_Age_Edu_2050_V2.csv",
          row.names = FALSE
  )

####PopbyAge####
####V2####
setwd(outpath)
data <-read_excel('hhpop_age_v1.xlsx',
                  sheet = 1)

data %<>% 
  dplyr::rename(
    AgeCat = age_cat
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

pop_proj_all <- left_join(data, temp_pop_proj, by = c("MPO" = "RPA", "AgeCat"))

pop_proj_all %<>%
  dplyr::rename(
    RPA = MPO,
    ageCAT3 = AgeCat,
    TARGET = proj_hh_pop,
    Year = year
  )
  
V2_hhpop_2020f <- pop_proj_all %>% 
  filter(
    Year == 2020
  ) %>% 
  select(
    RPA,
    ageCAT3,
    TARGET,
    BASELINE
  )

write.csv(
  V2_hhpop_2020f,
  "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/Pop_Age_2020_V2.csv",
  row.names = FALSE
)

V2_hhpop_2030f <- pop_proj_all %>% 
  filter(
    Year == 2030
  ) %>% 
  select(
    RPA,
    ageCAT3,
    TARGET,
    BASELINE
  )

write.csv(
  V2_hhpop_2030f,
  "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/Pop_Age_2030_V2.csv",
  row.names = FALSE
)

V2_hhpop_2040f <- pop_proj_all %>% 
  filter(
    Year == 2040
  ) %>% 
  select(
    RPA,
    ageCAT3,
    TARGET,
    BASELINE
  )

write.csv(
  V2_hhpop_2040f,
  "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/Pop_Age_2040_V2.csv",
  row.names = FALSE
)

V2_hhpop_2050f <- pop_proj_all %>% 
  filter(
    Year == 2050
  ) %>% 
  select(
    RPA,
    ageCAT3,
    TARGET,
    BASELINE
  )

write.csv(
  V2_hhpop_2050f,
  "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/Pop_Age_2050_V2.csv",
  row.names = FALSE
)
