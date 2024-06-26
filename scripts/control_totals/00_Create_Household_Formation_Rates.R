#
#Author: Brandon Stanaway
#Date: 3/1/2023
#Purpose: Aggregate population totals to reflect MAPC101 (RPA) instead of MAPC97 (MPO)
#UMDI uses the term RPA and MPO interchangeably

#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(mapcdatakeys)
library(tidycensus)

root <- "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/"
#root <- ""

census_api_key("d590f657a113f2f78b5a422462ea00745e79111c")

keys <- mapcdatakeys::all_muni_data_keys %>% 
  select(
    muni_name,
    mpo,
    rpa_acr
  ) %>% 
  mutate(
    muni_name = if_else(muni_name == "Manchester-by-the-Sea", "Manchester", muni_name),
    rebel_towns_RPA = if_else(muni_name %in% c("Stoughton","Hanover","Pembroke","Duxbury"),"RT",rpa_acr)
  )

#MAPC Data keys - for each muni
munis <- mapcdatakeys::all_muni_data_keys %>% 
  select(muni_id,cosub_cn10) %>% 
  mutate(GEOID = as.character(cosub_cn10)) %>% 
  select(GEOID,muni_id)

#MPO Rates
#From Sarah
mpo_rates <- read_excel("K:/DataServices/Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/HHPopmultipliers_byRPA_032023.xlsx",
                        sheet = 1)
mpo_rates <- mpo_rates %>% 
  pivot_longer(
    !MPO,
    names_to = "ageCAT",
    values_to = "hh_pop_r"
  ) %>% 
  mutate(
    sex1 = str_extract(ageCAT, "Female"),
    sex2 = str_extract(ageCAT, "Male"),
    sex = coalesce(sex1,sex2),
    ageCAT = case_when(
      ageCAT %in% c("Sum of Male: Under 5 years","Sum of Female: Under 5 years") ~ "1",
      ageCAT %in% c("Sum of Male: 5 to 9 years","Sum of Female: 5 to 9 years") ~ "2",
      ageCAT %in% c("Sum of Male: 10 to 14 years","Sum of Female: 10 to 14 years") ~ "3",
      ageCAT %in% c("Sum of Male: 15 to 17 years","Sum of Female: 15 to 17 years") ~ "4",
      ageCAT %in% c("Sum of Male: 18 and 19 years","Sum of Female: 18 and 19 years") ~ "4",
      ageCAT %in% c("Sum of Male:20-24","Sum of Female: 20-24") ~ "5",
      ageCAT %in% c("Sum of Male: 25 to 29 years","Sum of Female: 25 to 29 years") ~ "6",
      ageCAT %in% c("Sum of Male: 30 to 34 years","Sum of Female: 30 to 34 years") ~ "7",
      ageCAT %in% c("Sum of Male: 35 to 39 years","Sum of Female: 35 to 39 years") ~ "8",
      ageCAT %in% c("Sum of Male: 40 to 44 years","Sum of Female: 40 to 44 years") ~ "9",
      ageCAT %in% c("Sum of Male: 45 to 49 years","Sum of Female: 45 to 49 years") ~ "10",
      ageCAT %in% c("Sum of Male: 50 to 54 years","Sum of Female: 50 to 54 years") ~ "11",
      ageCAT %in% c("Sum of Male: 55 to 59 years","Sum of Female: 55 to 59 years") ~ "12",
      ageCAT %in% c("Sum of Male: 60-64","Sum of Female: 60-64") ~ "13",
      ageCAT %in% c("Sum of Male: 65-69","Sum of Female: 65-69") ~ "14",
      ageCAT %in% c("Sum of Male: 70 to 74 years","Sum of Female: 70 to 74 years") ~ "15",
      ageCAT %in% c("Sum of Male: 75 to 79 years","Sum of Female: 75 to 79 years") ~ "16",
      ageCAT %in% c("Sum of Male: 80 to 84 years","Sum of Female: 80 to 84 years") ~ "17",
      ageCAT %in% c("Sum of Male: 85 years and over","Sum of Female: 85 years and over") ~ "18"
    )
  ) %>% 
  select(
    -c(
      sex1,
      sex2
    )
  ) %>% 
  group_by(
    MPO,
    sex,
    ageCAT
  ) %>% 
  summarise(
    hh_pop_r = mean(hh_pop_r)
  )

#UMDI Population by Age and Sex by Muni

umdi_pop_muni <- read_excel(paste0(root,"UMDI_V2022_Population_Projections_AgeSexMCD_5yr.xlsx"),
                            sheet = 1) %>% 
  left_join(keys, by = c("MCD" = "muni_name")) %>% 
  mutate(
    `MCD Code` = as.character(`MCD Code`),
    `Age Group` = as.character(`Age Group`)
  ) %>% 
  dplyr::rename(
    MPO = RPA,
    RPA = rpa_acr,
    RPA_RT = rebel_towns_RPA
  ) %>% 
  filter(
    Year == 2010
  ) %>% 
  relocate(
    Year,
    MPO,
    RPA,
    RPA_RT,
    MCD,
    `MCD Code`,
    Sex,
    `Age Group`,
    Population
  ) %>% 
  select(
    -County
  )

#Population In Households SF1 2010 by Muni
vars2010 <- load_variables(2010, 'sf1')

pa.vars <- vars2010 %>% 
  filter(
    concept=='SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS'
  ) %>% 
  select(
    name
  ) %>% 
  mutate(
    name = as.character(name)
  ) %>% 
  pull(
    name
  )


pa.labels <- vars2010 %>% 
  filter(
    concept=='SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS'
  ) %>% 
  select(
    name,
    label
  ) %>% 
  mutate(
    sex1 = str_extract(label, "Female"),
    sex2 = str_extract(label, "Male"),
    sex = coalesce(sex1,sex2),
    label = gsub("Total!!Male!!","", label),
    label = gsub("Total!!Female!!","", label),
    label = gsub(" ", "_", label),
    label = paste0("pop_", label)
  ) %>% 
  select(
    -c(
      sex1,
      sex2
    )
  ) %>% 
  na.omit()

pa.values <-
  get_decennial(
    year = 2010,
    state = 'MA',
    sumfile = "sf1",
    geography = 'county subdivision',
    variables = pa.vars
  )
pop <- pa.values %>%
  filter(GEOID %in% munis$GEOID) %>% 
  right_join(pa.labels,by = c('variable'="name")) %>%
  mutate(
    ageCAT6 = case_when(
      label == 'pop_Under_5_years' ~ '1',
      label == "pop_5_to_9_years" ~ '2',
      label == "pop_10_to_14_years" ~ '3',
      label %in% c("pop_15_to_17_years","pop_18_and_19_years") ~ '4',
      label %in% c("pop_20_years","pop_21_years","pop_22_to_24_years") ~ '5',
      label == "pop_25_to_29_years" ~ '6',
      label == "pop_30_to_34_years" ~ '7',
      label == "pop_35_to_39_years" ~ '8',
      label == "pop_40_to_44_years" ~ '9',
      label == "pop_45_to_49_years" ~ '10',
      label == "pop_50_to_54_years" ~ '11',
      label == "pop_55_to_59_years" ~ '12',
      label %in% c("pop_60_and_61_years","pop_62_to_64_years") ~ '13',
      label %in% c("pop_65_and_66_years","pop_67_to_69_years") ~ '14',
      label == "pop_70_to_74_years" ~ '15',
      label == "pop_75_to_79_years" ~ '16',
      label == "pop_80_to_84_years" ~ '17',
      label == "pop_85_years_and_over" ~ '18'
    )
  ) %>% 
  filter(!is.na(ageCAT6)) %>% 
  right_join(munis,by = 'GEOID') %>% 
  mutate(muni_id = as.character(muni_id)) %>% 
  dplyr::rename(
    HHPopulation = value,
    Sex = sex
  ) %>% 
  select(
    c(
      muni_id,
      Sex,
      ageCAT6,
      HHPopulation
    )
  ) %>% 
  group_by(
    muni_id,
    Sex,
    ageCAT6
  ) %>% 
  summarise(
    HHPopulation = sum(HHPopulation)
  ) %>% 
  ungroup()

Umdi_pop_sf1_hhpop_2010 <- left_join(
  umdi_pop_muni,
  pop,
  by  = c("MCD Code" = "muni_id", "Sex", "Age Group" = "ageCAT6")
)

#Generate the Household Formation Rate for the MPO Aggregation
#Check against MPO rates generated for prior rounds of projections.
Umdi_pop_sf1_hhpop_2010_MPO <- Umdi_pop_sf1_hhpop_2010 %>% 
  group_by(
    MPO,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population),
    HHPopulation = sum(HHPopulation)
  ) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(
    hh_formation_r = (HHPopulation/Population),
    gq_population = Population - HHPopulation
  ) %>% 
  left_join(
    mpo_rates,
    by = c("MPO", "Sex" = "sex", "Age Group" = "ageCAT")
  ) %>% 
  mutate(
    rate_check = hh_formation_r - hh_pop_r
  )

#Write a file that demonstrates the rates + checks
write.csv(Umdi_pop_sf1_hhpop_2010_MPO,
          "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_MPO_DRAFT.csv",
          row.names = FALSE)

#Create a dataframe with the rates and relevant joining characteristics
hh_formation_rates_MPO <- Umdi_pop_sf1_hhpop_2010_MPO %>% 
  select(
    MPO,
    Sex,
    `Age Group`,
    hh_formation_r
  ) %>% 
  dplyr::rename(
    HH_Formation_Rate = hh_formation_r
  )

#Reinitialize the Population projections data 
#Join in the household formation rates
#Calculate the population in households for all data years

umdi_pop_muni <- read_excel(paste0(root,"UMDI_V2022_Population_Projections_AgeSexMCD_5yr.xlsx"),
                            sheet = 1) %>% 
  mutate(
    `MCD Code` = as.character(`MCD Code`),
    `Age Group` = as.character(`Age Group`)
  ) %>% 
  dplyr::rename(
    MPO = RPA,
  ) %>% 
  filter(
    Year %in% c(2000,2010,2020,2030,2040,2050)
  ) %>% 
  relocate(
    Year,
    MPO,
    MCD,
    `MCD Code`,
    Sex,
    `Age Group`,
    Population
  ) %>% 
  select(
    -County
  )%>% 
  group_by(
    Year,
    MPO,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population)
  ) %>% 
  ungroup() %>%
  #Join household formation rates back onto the population projections data
  left_join(
    hh_formation_rates_MPO,
    by = c("MPO","Sex","Age Group")
  ) %>%
  rowwise() %>% 
  mutate(
    HH_Population = Population*HH_Formation_Rate
  )

#Create a dataframe with just the population in households by each relevant
#demographic category.

UMDI_HH_Population_Projection <- umdi_pop_muni %>% 
  select(
    Year,
    MPO,
    Sex,
    `Age Group`,
    HH_Population
  )

#Write a file that demonstrates the rates + checks
write.csv(
  UMDI_HH_Population_Projection,
  "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/UMDI_HHPopulation_Projections_Age_Sex_MPO_v1.csv",
  row.names = FALSE)

#Test
UMDI_HH_Population_Projection_TEST <- UMDI_HH_Population_Projection %>% 
  group_by(
    MPO,
    Year
  ) %>% 
  summarise(
    HH_Population = sum(HH_Population)
  )




#------------------

Umdi_pop_sf1_hhpop_2010_RPA <- Umdi_pop_sf1_hhpop_2010 %>% 
  group_by(
    RPA,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population),
    HHPopulation = sum(HHPopulation)
  ) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(
    hh_formation_r = (HHPopulation/Population),
    gq_population = Population - HHPopulation
  )
write.csv(Umdi_pop_sf1_hhpop_2010_RPA,
          "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_RPA_DRAFT.csv",
          row.names = FALSE)

Umdi_pop_sf1_hhpop_2010_RPA_RT <- Umdi_pop_sf1_hhpop_2010 %>% 
  group_by(
    RPA_RT,
    Sex,
    `Age Group`
  ) %>% 
  summarise(
    Population = sum(Population),
    HHPopulation = sum(HHPopulation)
  ) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(
    hh_formation_r = (HHPopulation/Population),
    gq_population = Population - HHPopulation
  )

write.csv(Umdi_pop_sf1_hhpop_2010_RPA_RT,
          "K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/HHPopulation_Rates_RPA_RT_DRAFT.csv",
          row.names = FALSE)
#===============================================================================

