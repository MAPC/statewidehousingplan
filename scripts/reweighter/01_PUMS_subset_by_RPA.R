library(tidyverse)
library(tidycensus)
library(data.table)

##get rid of scientific notation
options(scipen = 999)

sun <- function(x){sort(unique(x))}

root <- 'S:/Network Shares/K Drive/DataServices/Projects/'
# root <- 'K:/DataServices/Projects/'
# root <- '//data-001/public/DataServices/Projects/'

scenario <- 'Baseline'

inputDIR <-
  paste0(root,
         'Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Reweighter/',scenario,'/')
setwd(inputDIR)

# List of RPA names
mpos <- unlist(c(mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()))

# PUMA crosswalk
mxw <- fread('../PUMS_data/ma_muni_puma10_join.csv')
mxw <- mxw[, .(TOWN_ID, PUMACE10)]
setnames(mxw, c('muni_id', 'PUMA'))
mxw <- mxw[, .(muni_id, PUMA)]
axw <-
  mapcdatakeys::all_muni_data_keys %>% select(muni_id, rpa_acr, mpo) %>% left_join(mxw, by = 'muni_id') %>% select(-muni_id) %>% unique() %>% setDT()

mapc.pumas <- axw[rpa_acr == 'MAPC', unique(PUMA)]
swm.pumas <- axw[mpo != 'MAPC', .(mpo, PUMA)] %>% unique()

#Set up datakeys for future joins.
mkeys <- mapcdatakeys::all_muni_data_keys |>  select(muni_id,rpa_acr,mpo)

#Load in the municipality to PUMA crosswalk.
psf <- fread('../PUMS_data/pums_muni_inter.csv') %>% 
  select(TOWN_ID,PUMACE10,Shape_Area) %>% 
  # psf <- fread('../ma_muni_puma10_join.csv') |>
  dplyr::rename(
    muni_id=TOWN_ID,
    PUMA = PUMACE10
  ) %>% 
  left_join(
    mkeys,
    by = c('muni_id')
  ) %>% 
  setDT()

#Generate RPA to PUMA crosswalk.
lsf <- psf[,lapply(.SD,sum,na.rm=T),.(rpa_acr,PUMA),.SDcols='Shape_Area']
lsf[,mx:=max(Shape_Area),PUMA]
xw <- lsf[Shape_Area==mx, .(rpa_acr,PUMA)]
xw[,PUMA:=sprintf("%05d",PUMA)]

# Manufacturing MVC and NPEDC RPAs manually

cc <- data.table(rpa_acr=c('MVC','NPEDC'), PUMA=c('04800','04800'))

#Append MVC and NPEDC data to PUMA to RPA/MPO crosswalk
xw <- rbind(xw, cc)

# PUMS 2021 5-yr data
yr <-  2021
variable_list <- c("PUMA", "TYPEHUGQ", "SEX", "AGEP", "RAC1P", "HISP", "ESR",
                   "WKHP", "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER", "HINCP")

pums_data <- get_pums(
  state = "MA",
  survey = "acs5",
  year = yr,
  variables = variable_list
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
    ),
    # Adjust income for inflation/deflator
    ADJINC = as.numeric(ADJINC),
    HINCP = round((as.numeric(HINCP)*ADJINC)*(0.86), 0), #ADD INFLATION ADJUSTMENT
    WAGPALLC_US =
      cut(
        HINCP,
        breaks = c(-Inf, 35000, 75000, 125000, 225000, Inf),
        labels = c("1","2","3","4","5")
      )
  ) %>% 
  setDT()

pums_data[,NP:=max(SPORDER),SERIALNO]

fwrite(pums_data,'../PUMS_data/PUMS2021_formatted.csv')


# Make Input folders for reweighter by RPA
for (m in unique(xw$rpa_acr)){
  dir.create(paste0('./Input_Files/',m),showWarnings = F)
}

# MAPC 101 subset
fwrite(pums_data[PUMA %in% mapc.pumas], paste0('Input_Files/MAPC/PUMS',yr,'_MAPC.csv'))
message(paste0('MAPC 101 PUMS export complete'))


# SWM join PUMA and export subsets
for (m in unique(swm.pumas$mpo)) {
  pums_data %>%
    left_join(swm.pumas, by = 'PUMA') %>%
    filter(mpo == m) %>%
    fwrite(paste0(m, '/PUMS',yr,'_', m, '.csv'))
  message(paste0(m, ' PUMS export complete'))
}
