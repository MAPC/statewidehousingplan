library(tidyverse)
library(tidycensus)
library(magrittr)
library(data.table)
library(janitor)
library(zoo)
library(readxl)


PUMS2019_formatted <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs2/PUMS2019_formatted.csv")

pums_list <- 2019
variable_list <- c("PUMA", "TYPE", "SEX", "AGEP", "WAGP", "SEMP", "ADJINC", "SPORDER", "HINCP", "NP")

#2019
PUMS_data_2019 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
},  .id = "year")

PUMS_data_2019 <- PUMS_data_2019 %>% 
  select(
    SERIALNO,
    SPORDER,
    HINCP,
    NP,
    ADJINC
  ) %>%
  mutate(
    #Adjust HINCP to 2013 Dollars
    ADJINC = as.numeric(ADJINC),
    HINCP = round((as.numeric(HINCP)*ADJINC)*(342.5/376.5), 0), #ADD INFLATION ADJUSTMENT
    WAGPALLC_US =
      cut(
        HINCP,
        breaks = c(-Inf, 35000, 75000, 125000, 225000, Inf),
        labels = c("1","2","3","4","5")
      )
  )

PUMS2019_formatted_edit <- left_join(PUMS2019_formatted, PUMS_data_2019, c("SERIALNO", "SPORDER"))


PUMS2019_formatted_edit <- PUMS2019_formatted_edit %>% 
  select(
    -WAGPALLC_US.x
  ) %>% 
  dplyr::rename(
    WAGPALLC_US = WAGPALLC_US.y
  )


write.csv(
  PUMS2019_formatted_edit,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Inputs2/PUMS2019_formatted_edited.csv",
  row.names = FALSE
)
