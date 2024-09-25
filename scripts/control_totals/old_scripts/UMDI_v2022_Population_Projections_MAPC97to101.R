#
#Author: Brandon Stanaway
#Date: 3/15/2023
#Purpose: Aggregate population totals to reflect MAPC101 (RPA) instead of MAPC97 (MPO)
#UMDI uses the term RPA and MPO interchangeably

#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(mapcdatakeys)
library(tidycensus)

root <- "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/"
#root <- ""

keys <- mapcdatakeys::all_muni_data_keys %>% 
  select(
    muni_name,
    rpa_acr
  ) %>% 
  mutate(
    muni_name = if_else(muni_name == "Manchester-by-the-Sea", "Manchester", muni_name)
  )

umdi_pop_muni <- read_excel(paste0(root,"UMDI_V2022_Population_Projections_AgeSexMCD_5yr.xlsx"),
                            sheet = 1) %>% left_join(keys, by = c("MCD" = "muni_name")) %>% mutate(rebel_towns_RPA = if_else(MCD %in% c("Stoughton","Hanover","Pembroke","Duxbury"),"RT",rpa_acr))
milpop <- read_excel(paste0(root,"UMDI-MassDOT_V2022_Population_Projections_MPO_10yr.xlsx"),
                           sheet = 1) %>% 
  select(MPO, Sex, `Age Group`, `mil pop 2010`,) %>% 
  mutate(`mil pop 2010` = round(`mil pop 2010`,0))
  
umdi_pop_mpo <- read_excel(paste0(root,"UMDI-MassDOT_V2022_Population_Projections_MPO_10yr.xlsx"),
                           sheet = 1)

umdi_pop_mpo_2 <- umdi_pop_mpo %>% 
  group_by(
    MPO
  ) %>% 
  summarise(
    Pop_2000 = sum(`2000`),
    Pop_2010 = sum(`2010`),
    Pop_2020 = sum(`2020`)
  )

sf1_pop_muni <- read_excel("C:/Users/bstanaway/Desktop/SF1_2010_population.xlsx",sheet=1) %>% 
  select(
    muni_id,
    municipal,
    pop
  )
pl94_pop_muni <- read_excel("C:/Users/bstanaway/Desktop/PL94_2020_Population.xlsx",sheet=1) %>% 
  select(
    muni_id,
    municipal,
    totpop
  ) %>% 
  left_join(
    keys_mpo,
    by = c("muni_id")
  ) %>% 
  group_by(
    mpo
  ) %>% 
  summarise(
    totpop = sum(totpop)
  )

Umdi_pop_sf1_hhpop_2010_MPO <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_MPO_DRAFT.csv")
Umdi_pop_sf1_hhpop_2010_RPA <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_RPA_DRAFT.csv")
Umdi_pop_sf1_hhpop_2010_RPA_RT <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_RPA_RT_DRAFT.csv")
#-------------------------------------------------------------------------------

umdi_pop_MAPC97_RT <- umdi_pop_muni %>% 
  group_by(
    Year,
    rebel_towns_RPA,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population)
  ) %>% 
  ungroup() %>% 
  filter(
    Year %in% c(2000,2010,2020,2030,2040,2050)
  )

umdi_pop_MAPC97_RT_TEST <- umdi_pop_MAPC97_RT  %>% 
  group_by(
    Year,
    rebel_towns_RPA
  ) %>% 
  summarise(
    Population = sum(Population)
  )

umdi_pop_MAPC101 <- umdi_pop_muni %>% 
  group_by(
    Year,
    rpa_acr,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population)
  ) %>% 
  ungroup() %>% 
  filter(
    Year %in% c(2000,2010,2020,2030,2040,2050)
  ) %>% 
  dplyr::rename(
    RPA = rpa_acr
  )

umdi_pop_MAPC101_TEST <- umdi_pop_MAPC101 %>% 
  group_by(
    Year,
    rpa_acr
  ) %>% 
  summarise(
    Population = sum(Population)
  )

umdi_pop_MAPC97 <- umdi_pop_muni %>% 
  group_by(
    Year,
    RPA,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population)
  ) %>% 
  ungroup() %>% 
  filter(
    Year %in% c(2000,2010,2020,2030,2040,2050)
  ) %>% 
  dplyr::rename(
    MPO = RPA
  )

umdi_pop_MAPC97_TEST <- umdi_pop_MAPC97 %>% 
  group_by(
    Year,
    MPO
  ) %>% 
  summarise(
    Population = sum(Population)
  )
#-------------------------------------------------------------------------------
keys_mpo <- mapcdatakeys::all_muni_data_keys %>% 
  select(
    muni_id,
    mpo
  )

umdi_pop_muni_eval <- umdi_pop_muni %>% 
  group_by(
    RPA,
    `MCD Code`,
    Year
  ) %>% 
  summarise(
    Population = sum(Population)
  ) %>% 
  ungroup()

umdi_pop_muni_eval_2010 <- umdi_pop_muni_eval %>% 
  filter(
    Year == 2010
  ) %>% full_join(
    sf1_pop_muni, by = c("MCD Code" = "muni_id")
  ) %>% 
  dplyr::rename(
    MPO = RPA,
    UMDI_Population = Population,
    SF1_Population = pop
  ) %>% 
  mutate(
    diff = UMDI_Population - SF1_Population
  ) %>% full_join(
    keys_mpo, by = c("MCD Code" = "muni_id")
  ) %>% 
  mutate(
    mpo_check = if_else(MPO == mpo, 1, 0)
  ) %>% 
  group_by(
    MPO
  ) %>% 
  summarise(
    SF1_Population = sum(SF1_Population)
  )

print(length(unique(umdi_pop_muni_eval_2010$`MCD Code`)))


umdi_pop_muni_eval_2020 <- umdi_pop_muni_eval %>% 
  filter(
    Year == 2020
  ) %>% inner_join(
    pl94_pop_muni, by = c("MCD Code" = "muni_id")
  ) %>% 
  dplyr::rename(
    MPO = RPA,
    UMDI_Population = Population,
    PL94_Population = totpop
  ) %>% 
  mutate(
    UMDI_Population = round(UMDI_Population,0),
    diff = UMDI_Population - PL94_Population
  )%>% full_join(
    keys_mpo, by = c("MCD Code" = "muni_id")
  ) %>% 
  mutate(
    mpo_check = if_else(MPO == mpo, 1, 0)
  ) %>% 
  group_by(
    MPO
  ) %>% 
  summarise(
    PL94_Population = sum(PL94_Population)
  )

#-------------------------------------------------------------------------------
umdi_pop_MAPC101_HHPop <- umdi_pop_MAPC101 %>% 
  left_join(Umdi_pop_sf1_hhpop_2010_RPA, by = c("RPA","Sex","Age Group"="Age.Group")) %>% 
  dplyr::rename(
    HHPopulation_SF1 = HHPopulation
  ) %>% 
  select(
    -Population.y,
    -gq_population
  ) %>%
  dplyr::rename(
    UMDI_Population = Population.x
  ) %>%
  mutate(
    UMDI_Population = round(UMDI_Population, 0),
    HH_Population = round((UMDI_Population)*hh_formation_r,0)
  ) %>% 
  select(
    -c(
      HHPopulation_SF1
    )
  ) %>% 
  dplyr::rename(
    Household_Formation_Rate = hh_formation_r
  )

write.csv(
  umdi_pop_MAPC101_HHPop,
  paste0(root,"UMDI_v2022_Population_HHPopulation_10yr_MAPC101_v3.csv"),
  row.names = FALSE
)

umdi_pop_MAPC97_HHPop <- umdi_pop_MAPC97 %>% 
  left_join(Umdi_pop_sf1_hhpop_2010_MPO, by = c("MPO","Sex","Age Group"="Age.Group")) %>% 
  dplyr::rename(
    HHPopulation_SF1 = HHPopulation,
    Original_MPO_Rate = hh_pop_r
  ) %>%  
  select(
    -Population.y,
    -gq_population,
    -rate_check
  ) %>%
  dplyr::rename(
    UMDI_Population = Population.x
  ) %>% 
  mutate(
    UMDI_Population = round(UMDI_Population, 0),
    HH_Population = round((UMDI_Population)*hh_formation_r,0)
  ) %>% 
  select(
    -c(
      HHPopulation_SF1,
      Original_MPO_Rate
    )
  ) %>% 
  dplyr::rename(
    Household_Formation_Rate = hh_formation_r
  )

write.csv(
  umdi_pop_MAPC97_HHPop,
  paste0(root,"UMDI_v2022_Population_HHPopulation_10yr_MAPC97_v2.csv"),
  row.names = FALSE
)


umdi_pop_MAPC97_RT_HHPop <- umdi_pop_MAPC97_RT  %>% 
  left_join(Umdi_pop_sf1_hhpop_2010_RPA_RT, by = c("rebel_towns_RPA" = "RPA_RT","Sex","Age Group"="Age.Group")) %>% 
  dplyr::rename(
    HHPopulation_SF1 = HHPopulation
  ) %>%  
  select(
    -Population.y,
    -gq_population
  ) %>%
  dplyr::rename(
    UMDI_Population = Population.x
  ) %>% 
  mutate(
    UMDI_Population = round(UMDI_Population, 0),
    HH_Population = round((UMDI_Population)*hh_formation_r,0)
  ) %>% 
  select(
    -c(
      HHPopulation_SF1
    )
  ) %>% 
  dplyr::rename(
    Household_Formation_Rate = hh_formation_r
  )

write.csv(
  umdi_pop_MAPC97_RT_HHPop,
  paste0(root,"UMDI_v2022_Population_HHPopulation_10yr_MAPC97_RT_v1.csv"),
  row.names = FALSE
)
#-------------------------------------------------------------------------------
umdi_pop_MAPC101_HHPop <- umdi_pop_MAPC101 %>% 
  left_join(umdi_pop_mpo, by = c("rpa_acr" = "MPO", "Sex", "Age Group")) %>% 
  mutate(
    Population = round(Population, 0),
    HH_Population = round((Population - `mil pop 2010`)*`HH pop`,0)
  ) %>% 
  select(
    -c(
      `mil pop 2010`,
      `HH pop`
    )
  )

umdi_pop_MAPC101_HHPop_TEST <- umdi_pop_MAPC101_HHPop %>% 
  group_by(
    Year,
    rpa_acr
  ) %>% 
  summarise(
    Population = sum(Population),
    HH_Population = sum(HH_Population)
  )

write.csv(
  umdi_pop_MAPC101_HHPop,
  paste0(root,"UMDI_v2022_Population_HHPopulation_10yr_MAPC101.csv"),
  row.names = FALSE
)

#-------------------------------------------------------------------------------