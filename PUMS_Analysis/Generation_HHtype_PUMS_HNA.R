library(plyr)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(R.utils)

##2000 data was downloaded from IPUMS

D2020h <- read.csv ("C:\\Users\\sphilbrick\\Downloads\\csv_hma\\psam_h25.csv")
D2010h <- read.csv ("K:\\DataServices\\Datasets\\U.S. Census and Demographics\\PUMS\\Raw\\pums_2008_12\\csv_hma\\ss12hma.csv")
D2010p <- read.csv ("K:\\DataServices\\Datasets\\U.S. Census and Demographics\\PUMS\\Raw\\pums_2008_12\\ss12pma.csv")
#D2000 <- gunzip("C:\\Users\\sphilbrick\\Downloads\\usa_00001.csv.gz")
D2000hp <- read.csv("C:\\Users\\sphilbrick\\Downloads\\usa_00001.csv")

#Remove scientific notation
options(scipen = 999)

#join person to hhds in 2010, remove non hhders
PHa10 <- join(D2010h,D2010p, by = c("SERIALNO"), type = "left", match = "all")
DHH10 <- PHa10[PHa10$TYPE==1,]
DHH10 <- PHa10[PHa10$SPORDER==1,]
rm(PHa10)

#remove sample to only include hhders 2000
D2000h <- D2000hp[D2000hp$RELATE==1,]

#Find average condo fee
D2020h$adj <- (D2020h$ADJINC/1000000)
D2020h$adjcondo = D2020h$CONP*D2020h$adj
D2020h$adjcondowgt = D2020h$adjcondo * D2020h$WGTP
D2020condo <- D2020h[D2020h$adjcondo!=0,]
D2020condo <- D2020condo %>% 
  filter(if_any(adjcondo, ~ !is.na(.)))
mean_2020 <- sum(D2020condo$adjcondowgt)/ sum(D2020condo$WGTP)
#mean(D2020h$adjcondo[D2020h$adjcondo != 0], na.rm = TRUE)
#median(D2020h$adjcondo[D2020h$adjcondo != 0], na.rm = TRUE)

D2010h$adj <- (D2010h$ADJINC/1000000)
D2010h$adjcondo = D2010h$CONP*D2010h$adj
D2010h$adjcondowgt = D2010h$adjcondo * D2010h$WGTP
D2010h$adjcondowgt = D2010h$adjcondo * D2010h$WGTP
D2010condo <- D2010h[D2010h$adjcondo!=0,]
D2010condo <- D2010condo %>% 
  filter(if_any(adjcondo, ~ !is.na(.)))
mean_2010 <- sum(D2010condo$adjcondowgt)/ sum(D2010condo$WGTP)
#mean(D2010h$adjcondo[D2010h$adjcondo != 0], na.rm = TRUE)
#median(D2010h$adjcondo[D2010h$adjcondo != 0], na.rm = TRUE)

D2000h$adjcondowgt = D2000h$CONDOFEE * D2000h$HHWT
D2000condo <- D2000h[D2000h$CONDOFEE!=0,]
D2000condo <- D2000condo %>%
  filter(if_any(adjcondowgt, ~!is.na(.)))
mean_2000 <- sum(D2000condo$adjcondowgt)/ sum(D2000condo$HHWT)
#mean(D2000h$CONDOFEE[D2000h$CONDOFEE != 0], na.rm = TRUE)
#median(D2000h$CONDOFEE[D2000h$CONDOFEE != 0], na.rm = TRUE)


#add in record year 
DHH10$year <- as.numeric(substr(DHH10$SERIALNO, 1,4))
D2020h$year <- as.numeric(substr(D2020h$SERIALNO, 1,4))

#create year of birth category
DHH10$birthyr <- DHH10$year - DHH10$AGEP
D2020h$birthyr <- D2020h$year - D2020h$HHLDRAGEP

#define generations
DHH10$generation[DHH10$birthyr>= 1928 & DHH10$birthyr< 1946] = "Silent"
DHH10$generation[DHH10$birthyr>= 1946 & DHH10$birthyr< 1965] = "Boomer" 
DHH10$generation[DHH10$birthyr>= 1965 & DHH10$birthyr< 1981] = "Gen X" 
DHH10$generation[DHH10$birthyr>= 1981 & DHH10$birthyr< 1997] = "Millenial"
DHH10$generation[DHH10$birthyr>=1997] = "Gen Z"

D2020h$generation[D2020h$birthyr>= 1928 & D2020h$birthyr< 1946] = "Silent"
D2020h$generation[D2020h$birthyr>= 1946 & D2020h$birthyr< 1965] = "Boomer" 
D2020h$generation[D2020h$birthyr>= 1965 & D2020h$birthyr< 1981] = "Gen X" 
D2020h$generation[D2020h$birthyr>= 1981 & D2020h$birthyr< 1997] = "Millenial"
D2020h$generation[D2020h$birthyr>=1997] = "Gen Z"

D2000h$generation[D2000h$BIRTHYR>= 1928 & D2000h$BIRTHYR< 1946] = "Silent"
D2000h$generation[D2000h$BIRTHYR>= 1946 & D2000h$BIRTHYR< 1965] = "Boomer" 
D2000h$generation[D2000h$BIRTHYR>= 1965 & D2000h$BIRTHYR< 1981] = "Gen X" 
D2000h$generation[D2000h$BIRTHYR>= 1981 & D2000h$BIRTHYR< 1997] = "Millenial"
D2000h$generation[D2000h$BIRTHYR>=1997] = "Gen Z"


#Household type 
D2000h$HHtype[D2000h$US2000A_PERSONS==1] = "single person"
D2000h$HHtype[D2000h$US2000A_PERSONS>=2 & D2000h$US2000A_P18>0] = "household with child"
D2000h$HHtype[D2000h$US2000A_PERSONS>=2 & D2000h$US2000A_P18==0] = "multiple people, no child"

DHH10$HHtype[DHH10$NP==1] = "single person"
DHH10$HHtype[DHH10$NP>=2 & DHH10$HUPAC==4] = "multiple people, no child"
DHH10$HHtype[DHH10$NP>=2 & DHH10$HUPAC<4] = "household with child"

D2020h$HHtype[D2020h$NP==1] = "single person"
D2020h$HHtype[D2020h$NP>=2 & D2020h$HUPAC==4] = "multiple people, no child"
D2020h$HHtype[D2020h$NP>=2 & D2020h$HUPAC<4] = "household with child"


#write only necessary columns 
output10 <- select(DHH10, c( 'WGTP', 'HHtype', 'generation', 'birthyr'))
output20 <- select(D2020h, c('WGTP', 'HHtype', 'generation', 'birthyr'))
output00 <- select(D2000h, c('HHWT', 'HHtype', 'generation', 'BIRTHYR'))

write.csv(output00, file= "K:\\DataServices\\Projects\\Current_Projects\\Housing\\StatewideHousingPlan\\04_Analysis\\Data\\Working\\HNA\\HNA_Generation_HHds_2000.csv")
write.csv(output20, file="K:\\DataServices\\Projects\\Current_Projects\\Housing\\StatewideHousingPlan\\04_Analysis\\Data\\Working\\HNA\\HNA_Generation_HHds_17_21.csv" )
write.csv(output10, file="K:\\DataServices\\Projects\\Current_Projects\\Housing\\StatewideHousingPlan\\04_Analysis\\Data\\Working\\HNA\\HNA_Generation_HHds_08_12.csv")
