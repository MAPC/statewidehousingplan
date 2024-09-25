library(tidyverse)
library(data.table)


root <- 'S:/Network Shares/K Drive/DataServices/Projects/'
# root <- 'K:/DataServices/Projects/'

dir <- paste0(root, 'Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/Headship_rates')


acs21 <- fread(paste0(dir,'headship_rates.1721.csv')) %>% 
  rename(hhtype = var,
         rate1721 = freq,
         rpa = rpa_acr) 

t00 <- fread(paste0(dir,'hh_rates_hhtype_2000.csv')) %>% 
  pivot_longer(cols=names(.)[-1]) %>% 
  rename(hhtype=name,
         rate2000=value) %>% 
  mutate(hhtype = gsub('2000','',hhtype))

a00 <- fread(paste0(dir,'hh_rates_by_age_RPA_2000.csv')) %>% 
  pivot_longer(cols=names(.)[-1]) %>%
  rename(rpa=name, 
         rate2000=value) %>% 
  mutate(rpa = gsub('_2000','',rpa))


hhcomp <- acs21 %>% 
  left_join(t00, by = c('ageCAT6','hhtype')) %>% 
  
  

# 1. Household pop by age and by RPA
# 2. Headship by age and RPA
# 3. HHtype by age and RPA (for 2000 use HHtype by age statewide fractions for each RPA)


#Set PUMS vintage final year for {tidycensus} API query.
yr <- 2021

#List of all 2017-2021 ACS PUMS Variables
allvars <- pums_variables |>  filter(year==yr) |>  select(var_code) |>  unique()

#List of PUMS variables
variable_list <- c("PUMA", "TYPEHUGQ", "SEX", "AGEP", "RAC1P", "HISP", "ESR",
                   "WKHP", "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER", "HINCP")
#SEMP bottom coding has changed from $1 to $4 like WAGP

#Query {tidycensus} for 2017-2021 5-Year PUMS data
pums_data <- get_pums(
  state = "MA",
  survey = "acs5",
  year = yr,
  variables = variable_list
) |> 
  #Join RPA to PUMA crosswalk to PUMS data
  left_join(
    xw,
    by = c('PUMA')
  ) |>  
  mutate(
    #Generate five-year age groupings to match UMDI population projections data.
    ageCAT6 = cut(AGEP, breaks = c(-Inf, seq(4, 84, 5), Inf), labels = 1:18),
    # Consolidated Age Category
    # NOTE: Levels are (0) 0-14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 34; (7) 35 to 44; (8) 45 to 54; (9) 55 to 64 (10) 65 to 74 (11) 75+
    PAGEC2 = cut(AGEP, breaks = c(-Inf, 14, 19, 24, 34, 44, 54, 64, 74, 79, 84, Inf),
                 labels = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)),
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
      WKHP >= 35 ~ "Full time (35 hrs or more)",
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
    
    # Determine if person is head of household
    HousHder = case_when(
      SPORDER == 1 ~ 1,
      SPORDER > 1 ~ 2
    ),
    
    # Determine if case counts as person
    person = case_when(
      SPORDER != 0 ~ 1,
      SPORDER == 0 ~ 0
    ),
    
    # Determine if case counts as child
    child = case_when(
      AGEP < 18 ~ 1,
      AGEP >= 18 ~ 0
    )
  ) |> 
  
  #Generating Household Variables
  group_by(SERIALNO) |> 
  mutate(
    Worker_Total = sum(worker),
    Person_Total = sum(person),
    Child_Total = sum(child)
  ) |> 
  ungroup() |> 
  # Categorizing household sizes by number of persons (1) one person, (2) two persons, (3) three persons, (4) four or more persons
  mutate(
    HHSize = case_when(
      Person_Total == 1 ~ 1,
      Person_Total == 2 ~ 2,
      Person_Total == 3 ~ 3,
      Person_Total >= 4 ~ 4
    ),
    
    # Categorizing worker categories (0) no workers, (1) one worker, (2) two workers, (3) three or more workers
    WRKHH = case_when(
      Worker_Total == 0 ~ 0,
      Worker_Total == 1 ~ 1,
      Worker_Total == 2 ~ 2,
      Worker_Total >= 3 ~ 3
    ),
    
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
  )

#Generating headship rates by RPA and 5-year age group for the ACS 2017-2022 PUMS
headship_rates <- pums_data |> 
  #Inclues only observations for people in households (omits GQ)                       
  filter(TYPEHUGQ == "1") |> 
  #Groups by variables in the dataframe necessary to getting the granularity of
  #data we're interested in.
  group_by(rpa_acr, ageCAT6) |> 
  #Computes the weighted sum of individuals in each HH type category by the groups
  #assigned above.
  count(var = HHtype, wt = WGTP) |>
  #Recodes factor variables as the categories they represent.
  #Converts numbers into frequencies.                       
  mutate(
    var = case_when(
      var == 1 ~ "hhderchild",
      var == 2 ~ "nothhderchild",
      var == 3 ~ "hhdernochild",
      var == 4 ~ "nothhdernochild",
      var == 5 ~ "single"),
    freq = (n/sum(n))) |> 
  #Removes the level of aggregation set earlier by group_by()
  ungroup() |> 
  select(
    -c(n)
  )

#QC Check - determine if the frequencies in each RP, Age Group crosstab add to 1
hr_check <- headship_rates |> 
  group_by(
    rpa_acr,
    ageCAT6
  ) |> 
  summarise(
    freq_check = sum(freq)
  ) |> 
  ungroup() |> 
  mutate(
    flag = ifelse(freq_check != 1, 1, 0)
  )

if (sum(hr_check$flag) > 0){
  print("STOP! Check headship rate calculations!")
} else{
  print("Proceed!")
}