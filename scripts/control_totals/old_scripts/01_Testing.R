#install.packages("pacman")
pacman::p_load(tidyverse, data.table, janitor, readxl)

#Code within double #s written by Brandon Stanaway for the purpose of converting data.table syntax to tidyverse syntax.

### Set working directories

#Remote Directory
setwd("S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs")
#Non-Remote Directory
#setwd("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs")


# Output directory for file export
#Remote Directory
out <- 'S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'
#Non-Remote Directory
#out <- 'K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/'

# Load UMDI population projections
umdi <- read_excel('UMDI_V2022_Population_Projections_AgeSexMCD_5yr.xlsx',
                   sheet = 1)
umdi <- umdi %>%
  mutate(`Age Group` = as.character(`Age Group`))

############################ Could they be replaced with datakeys package? Ask Aseem.
# Load RPA keys
rpa_key <- read.csv('20190821_rpa-key.csv',
                    stringsAsFactors = F)
#Load Muni keys
munis <- read.csv('muni_datakeys.csv')

#Create a master geography key
geo_key <- left_join(munis, rpa_key, by = c("rpa_id")) %>%
  select(muni_id, rpa_id, rpa_abr)

### Revist this later, when we have munilevel data
# Lookup table for MAPC with 101 munis
mapc101 <- read.csv(
  '20190814_muni-lookup-mapc101.csv',
  stringsAsFactors = F,
  colClasses = c('muni_id' = 'character')
)

# MAPC 97 munis
mapc97 <- read.csv(
  '20190814_muni-lookup-mapc97.csv',
  stringsAsFactors = F,
  colClasses = c('muni_id' = 'character')
) ##What is the difference between these two (mapc101 & mapc97)?

# Male Military Population
mil_m <- read.csv('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/20190820_male-armed-forces.csv',
                  stringsAsFactors = F)
mil_m <- mil_m %>%
  pivot_longer(!rpa, names_to = "age_cat", values_to = "mil_m")
  

# Female Military Population
mil_f <- read.csv('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/20190820_female-armed-forces.csv',
                  stringsAsFactors = F)
mil_f <- mil_f %>%
  pivot_longer(!rpa, names_to = "age_cat", values_to = "mil_f")

#Join Military population together
mil_pop <- left_join(mil_m, mil_f, by = c("rpa", "age_cat")) %>%
  mutate(age_cat = str_replace(age_cat, "X", ""))

#mil_pop represents the 2010 military population by age and sex for each municipality.
#The subsequent code controls that data to 2020 PL94 military GQ population by municipality to produce a synthetic, updated
#military gq population..
#mil_pl94 <- read_excel("PL94_2020_GQ_Pop.xlsx",
                       #sheet = 1)
#mil_pl94 <- mil_pl94 %>%
  #select(municipal, mil_gq)

#write.csv(mil_pop, "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/mil_pop.csv")

#Group Quarters

gq <- fread('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/ma_2020_group_quarters_muni.csv')
gq <- gq %>%
  select(TOWN_ID, sum_quarterpop) %>%
  rename("muni_id" = TOWN_ID, "gq_pop" = sum_quarterpop)
gq <- left_join(geo_key, gq, by = c("muni_id")) %>%
  group_by(rpa_abr) %>%
  summarise(gq_pop = sum(gq_pop))

### Convert population by age and sex to Household population by age and sex using 2010 proportions, and then adjust to ensure match with 2020 Group Quarters totals by muni
hhpop <- read.csv('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/input_data/HHPopmultipliers_byRPA.csv')
hhpop$Town <- gsub(" town", "", hhpop$Town)
hhpop$Town <- gsub(" city", "", hhpop$Town)
hhpop$Town <- gsub(" Town city", "", hhpop$Town)
hhpop$Town <- gsub(" Town", "", hhpop$Town)
hhpop_cleaned <- hhpop %>%
  group_by(Town) %>%
  mutate(Male.15.to.19.years = mean(Male.15.to.17.years, Male.18.and.19.years),
         Female.15.to.19.years = mean(Female.15.to.17.years, Female.18.and.19.years)) %>%
  ungroup() %>%
  select(-PctMale15_17, -PctMale18, -PctMale19, -PctFemale15_17, -PctFemale18, -PctFemale19,
         -Male.15.to.17.years, -Male.18.and.19.years, -Female.15.to.17.years, -Female.18.and.19.years) %>%
  rename(m_1 = Male.Under.5.years,
         m_2 = Male.5.to.9.years,
         m_3 = Male.10.to.14.years,
         m_4 = Male.15.to.19.years,
         m_5 = Male.20.to.24.years,
         m_6 = Male.25.to.29.years,
         m_7 = Male.30.to.34.years,
         m_8 = Male.35.to.39.years,
         m_9 = Male.40.to.44.years,
         m_10 = Male.45.to.49.years,
         m_11 = Male.50.to.54.years,
         m_12 = Male.55.to.59.years,
         m_13 = Male.60.to.64.years,
         m_14 = Male.65.to.69.years,
         m_15 = Male.70.to.74.years,
         m_16 = Male.75.to.79.years,
         m_17 = Male.80.to.84.years,
         m_18 = Male.85.years.and.over,
         f_1 = Female.Under.5.years,
         f_2 = Female.5.to.9.years,
         f_3 = Female.10.to.14.years,
         f_4 = Female.15.to.19.years,
         f_5 = Female.20.to.24.years,
         f_6 = Female.25.to.29.years,
         f_7 = Female.30.to.34.years,
         f_8 = Female.35.to.39.years,
         f_9 = Female.40.to.44.years,
         f_10 = Female.45.to.49.years,
         f_11 = Female.50.to.54.years,
         f_12 = Female.55.to.59.years,
         f_13 = Female.60.to.64.years,
         f_14 = Female.65.to.69.years,
         f_15 = Female.70.to.74.years,
         f_16 = Female.75.to.79.years,
         f_17 = Female.80.to.84.years,
         f_18 = Female.85.years.and.over) %>%
  select(-muni_id, -RPA) %>%
  pivot_longer(!Town, names_to = "age_cat", values_to = "HH_p")

male <- c("m_1", "m_2", "m_3", "m_4", "m_5", "m_6", "m_7", "m_8", "m_9",
          "m_10", "m_11", "m_12", "m_13", "m_14", "m_15", "m_16", "m_17", "m_18")
female <- c("f_1", "f_2", "f_3", "f_4", "f_5", "f_6", "f_7", "f_8", "f_9",
            "f_10", "f_11", "f_12", "f_13", "f_14", "f_15", "f_16", "f_17", "f_18")

male_hhpop <- hhpop_cleaned %>%
  filter(age_cat %in% male) %>%
  mutate(Sex = "Male")
male_hhpop$age_cat <- gsub("m_", "", male_hhpop$age_cat)

female_hhpop <- hhpop_cleaned %>%
  filter(age_cat %in% female) %>%
  mutate(Sex = "Female")
female_hhpop$age_cat <- gsub("f_", "", female_hhpop$age_cat)

hhpop_final <- rbind(male_hhpop, female_hhpop)

#write.csv(hhpop, "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/hh_pop.csv")
#Now join the household ratios to the UMDI data.

umdi_hhpop <- left_join(umdi, hhpop_final, by = c("MCD" = "Town", "Age Group" = "age_cat", "Sex"))

#Determine the total HH population (population * % of pop in a household)

umdi_hhpop <- umdi_hhpop %>%
  group_by(MCD, Year, Sex, `Age Group`) %>%
  mutate(HH_population = Population*HH_p) %>%
  ungroup() %>%
  group_by(RPA, Year, Sex, `Age Group`) %>%
  summarise(HH_population = sum(HH_population))
  
umdi_hhpop_milpop <- left_join(umdi_hhpop, mil_pop, by = c("RPA" = "rpa", "Age Group" = "age_cat"))

umdi_hhpop_milpop <- umdi_hhpop_milpop %>%
  group_by(RPA, Year, Sex, `Age Group`) %>%
  mutate(HH_population_final = HH_population - (mil_m + mil_f),
         HH_population_final = round(HH_population_final, 0))
write.csv(umdi_hhpop_milpop, "S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Outputs/Adj_population_projections_20202050.csv")

# Join tables
umdi <- umdi[gq]

# Calculate non-group quarters population (i.e. household population) by muni
umdi[, hhpop := total_pop - gq_pop]

# Join to 2010 household population shares
umdi <- hhpop[umdi]

# Calculate 2020 household-dwelling (non-GQ) population by age and sex using 2010 GQ pop rates
c20 <-
  c(
    "male_under_5_years",
    "male_5_to_9_years",
    "male_10_to_14_years",
    "male_20_to_24_years",
    "male_25_to_29_years",
    "male_30_to_34_years",
    "male_35_to_39_years",
    "male_40_to_44_years",
    "male_45_to_49_years",
    "male_50_to_54_years",
    "male_55_to_59_years",
    "male_60_to_64_years",
    "male_65_to_69_years",
    "male_70_to_74_years",
    "male_75_to_79_years",
    "male_80_to_84_years",
    "male_85_years_and_over",
    "female_under_5_years",
    "female_5_to_9_years",
    "female_10_to_14_years",
    "female_20_to_24_years",
    "female_25_to_29_years",
    "female_30_to_34_years",
    "female_35_to_39_years",
    "female_40_to_44_years",
    "female_45_to_49_years",
    "female_50_to_54_years",
    "female_55_to_59_years",
    "female_60_to_64_years",
    "female_65_to_69_years",
    "female_70_to_74_years",
    "female_75_to_79_years",
    "female_80_to_84_years",
    "female_85_years_and_over"
  )

for (i in c20) {
  j <- paste0('umdi_', i)
  umdi[, (j) := get(i) * get(j)]
}

# Re-constitute teenage categories. Why are we multiplying the full set (15-19) by the limited set (15-17)?
umdi[, umdi_male_15_to_17_years := pct_male15_17 * male_15_to_17_years *
       umdi_male_15_to_19_years]

umdi[, umdi_male_18_years := pct_male18 * male_18_and_19_years * umdi_male_15_to_19_years]

umdi[, umdi_male_19_years := pct_male19 * male_18_and_19_years * umdi_male_15_to_19_years]

umdi[, umdi_female_15_to_17_years := pct_female15_17 * female_15_to_17_years *
       umdi_female_15_to_19_years]

umdi[, umdi_female_18_years := pct_female18 * female_18_and_19_years * umdi_female_15_to_19_years]

umdi[, umdi_female_19_years := pct_female19 * female_18_and_19_years * umdi_female_15_to_19_years]

# Remove surplus columns
umdi[, umdi_male_15_to_19_years := NULL]
umdi[, umdi_female_15_to_19_years := NULL]

# Subset to only columns needed
colkeep <-
  c(
    'muni_id',
    'rpa',
    'town',
    'umdi_male_under_5_years',
    'umdi_male_5_to_9_years',
    'umdi_male_10_to_14_years',
    'umdi_male_15_to_17_years',
    'umdi_male_18_years',
    'umdi_male_19_years',
    'umdi_male_20_to_24_years',
    'umdi_male_25_to_29_years',
    'umdi_male_30_to_34_years',
    'umdi_male_35_to_39_years',
    'umdi_male_40_to_44_years',
    'umdi_male_45_to_49_years',
    'umdi_male_50_to_54_years',
    'umdi_male_55_to_59_years',
    'umdi_male_60_to_64_years',
    'umdi_male_65_to_69_years',
    'umdi_male_70_to_74_years',
    'umdi_male_75_to_79_years',
    'umdi_male_80_to_84_years',
    'umdi_male_85_years_and_over',
    'umdi_female_under_5_years',
    'umdi_female_5_to_9_years',
    'umdi_female_10_to_14_years',
    'umdi_female_15_to_17_years',
    'umdi_female_18_years',
    'umdi_female_19_years',
    'umdi_female_20_to_24_years',
    'umdi_female_25_to_29_years',
    'umdi_female_30_to_34_years',
    'umdi_female_35_to_39_years',
    'umdi_female_40_to_44_years',
    'umdi_female_45_to_49_years',
    'umdi_female_50_to_54_years',
    'umdi_female_55_to_59_years',
    'umdi_female_60_to_64_years',
    'umdi_female_65_to_69_years',
    'umdi_female_70_to_74_years',
    'umdi_female_75_to_79_years',
    'umdi_female_80_to_84_years',
    'umdi_female_85_years_and_over',
    'hhpop'
  )

umdi_cln <- umdi[, (colkeep), with = F]

# Adjust the household-dwelling population by age and sex that were estimated with 2010 proportions to match the 2020 total household population by muni

# Total 2020 household population is calculated using 2020 municipal population totals minus 2020 group quarters population

umdi_cln[, sub_pop := rowSums(umdi_cln[, 4:43])]
ns <- names(umdi_cln)[4:43]

# Loop through columns and adjust values
for (j in ns)
  set(umdi_cln,
      j = j,
      value = umdi_cln[[j]] / umdi_cln$sub_pop * umdi_cln$hhpop)

umdi_cln[, check_pop := rowSums(umdi_cln[, 4:43])]

# Clean columns and names and save file
subcols <-
  c(
    "muni_id",
    "umdi_male_under_5_years",
    "umdi_male_5_to_9_years",
    "umdi_male_10_to_14_years",
    "umdi_male_15_to_17_years",
    "umdi_male_18_years",
    "umdi_male_19_years",
    "umdi_male_20_to_24_years",
    "umdi_male_25_to_29_years",
    "umdi_male_30_to_34_years",
    "umdi_male_35_to_39_years",
    "umdi_male_40_to_44_years",
    "umdi_male_45_to_49_years",
    "umdi_male_50_to_54_years",
    "umdi_male_55_to_59_years",
    "umdi_male_60_to_64_years",
    "umdi_male_65_to_69_years",
    "umdi_male_70_to_74_years",
    "umdi_male_75_to_79_years",
    "umdi_male_80_to_84_years",
    "umdi_male_85_years_and_over",
    "umdi_female_under_5_years",
    "umdi_female_5_to_9_years",
    "umdi_female_10_to_14_years",
    "umdi_female_15_to_17_years",
    "umdi_female_18_years",
    "umdi_female_19_years",
    "umdi_female_20_to_24_years",
    "umdi_female_25_to_29_years",
    "umdi_female_30_to_34_years",
    "umdi_female_35_to_39_years",
    "umdi_female_40_to_44_years",
    "umdi_female_45_to_49_years",
    "umdi_female_50_to_54_years",
    "umdi_female_55_to_59_years",
    "umdi_female_60_to_64_years",
    "umdi_female_65_to_69_years",
    "umdi_female_70_to_74_years",
    "umdi_female_75_to_79_years",
    "umdi_female_80_to_84_years",
    "umdi_female_85_years_and_over"
  )

v2 <- umdi_cln[, (subcols), with = F]
names(v2) <-
  c(
    "muni_id",
    "M_HH_04",
    "M_HH_59",
    "M_HH_1014",
    "M_HH_1517",
    "M_HH_18",
    "M_HH_19",
    "M_HH_2024",
    "M_HH_2529",
    "M_HH_3034",
    "M_HH_3539",
    "M_HH_4044",
    "M_HH_4549",
    "M_HH_5054",
    "M_HH_5559",
    "M_HH_6064",
    "M_HH_6569",
    "M_HH_7074",
    "M_HH_7579",
    "M_HH_8084",
    "M_HH_85+",
    "F_HH_04",
    "F_HH_59",
    "F_HH_1014",
    "F_HH_1517",
    "F_HH_18",
    "F_HH_19",
    "F_HH_2024",
    "F_HH_2529",
    "F_HH_3034",
    "F_HH_3539",
    "F_HH_4044",
    "F_HH_4549",
    "F_HH_5054",
    "F_HH_5559",
    "F_HH_6064",
    "F_HH_6569",
    "F_HH_7074",
    "F_HH_7579",
    "F_HH_8084",
    "F_HH_85+"
  )

fwrite(v2, '2020_V2.csv')

# Process all forecast decades
files <- c('2020_V2.csv', '2030_V2.csv', '2040_V2.csv', '2050_V2.csv')

scenarios <- lapply(
  files,
  read.csv,
  na.strings = '-',
  colClasses = 'character',
  stringsAsFactors = F
)

names(scenarios) <- substr(files, 0, nchar(files) - 4)

for (i in 1:length(files)) {
  scenarios[[i]]$scenario <-
    substr(files[i], 0, nchar(files[i]) - 4) # assign scenario name
}

###### get data in workable format ######
scenarios <- bind_rows(scenarios)
scenarios[is.na(scenarios)] <- 0 #Are we getting NAs? Since the data is mostly numeric, introducing 0s would probably lead to some less-than-ideal outcomes.

# 101 Municipalities for MAPC
scenarios101 <- scenarios %>%
  left_join(mapc101, by = 'muni_id') # join 101 IDs

scenarios101 <- scenarios101 %>%
  select(-muni_id) %>%
  select(scenario, rpa_abr, everything())

scenarios101[, -c(1, 2)] <-
  lapply(scenarios101[, -c(1, 2)], as.numeric)
scenarios101[is.na(scenarios101)] <- 0 # fill NAs


###### get sums by RPA: 101 ######
s101_sums <-
  aggregate(. ~ rpa_abr + scenario, scenarios101, sum, na.rm = T) # aggregate by rpa and scenario #Why use aggreagte() here if we can use summarise()?

# Get 15-19 Age Category
s101_sums <- s101_sums %>%
  mutate(M_HH_1519 = M_HH_1517 + M_HH_18 + M_HH_19,
         F_HH_1519 = F_HH_1517 + F_HH_18 + F_HH_19) %>%
  select(-M_HH_1517,-M_HH_18,-M_HH_19,-F_HH_1517,-F_HH_18,-F_HH_19)

s101_adj <- s101_sums %>%
  left_join(mil_m, c('rpa_abr' = 'rpa')) %>%
  left_join(mil_f, c('rpa_abr' = 'rpa')) %>%
  rename(
    Male1 = M_HH_04,
    Male2 = M_HH_59,
    Male3 = M_HH_1014,
    Male4 = M_HH_1519,
    Male5 = M_HH_2024,
    Male6 = M_HH_2529,
    Male7 = M_HH_3034,
    Male8 = M_HH_3539,
    Male9 = M_HH_4044,
    Male10 = M_HH_4549,
    Male11 = M_HH_5054,
    Male12 = M_HH_5559,
    Male13 = M_HH_6064,
    Male14 = M_HH_6569,
    Male15 = M_HH_7074,
    Male16 = M_HH_7579,
    Male17 = M_HH_8084,
    Male18 = M_HH_85.
  ) %>%
  rename(
    Female1 = F_HH_04,
    Female2 = F_HH_59,
    Female3 = F_HH_1014,
    Female4 = F_HH_1519,
    Female5 = F_HH_2024,
    Female6 = F_HH_2529,
    Female7 = F_HH_3034,
    Female8 = F_HH_3539,
    Female9 = F_HH_4044,
    Female10 = F_HH_4549,
    Female11 = F_HH_5054,
    Female12 = F_HH_5559,
    Female13 = F_HH_6064,
    Female14 = F_HH_6569,
    Female15 = F_HH_7074,
    Female16 = F_HH_7579,
    Female17 = F_HH_8084,
    Female18 = F_HH_85.
  ) %>%
  select(
    rpa_abr,
    scenario,
    # reorder
    Female1,
    Female2,
    Female3,
    Female4,
    Female5,
    Female6,
    Female7,
    Female8,
    Female9,
    Female10,
    Female11,
    Female12,
    Female13,
    Female14,
    Female15,
    Female16,
    Female17,
    Female18,
    Male1,
    Male2,
    Male3,
    Male4,
    Male5,
    Male6,
    Male7,
    Male8,
    Male9,
    Male10,
    Male11,
    Male12,
    Male13,
    Male14,
    Male15,
    Male16,
    Male17,
    Male18
  )

###### Format and write out csvs for CivHH & HH populations
scenarios <- unique(s101_adj$scenario) # get scenario ID
rpas <- unique(s101_adj$rpa_abr) # get RPA ID

for (i in scenarios) {
  temp <- subset(s101_adj, scenario == i) # subset by scenario
  for (j in rpas) {
    # CivHH
    temp_mil <-
      as.data.frame(cbind(t(subset(mil_f, rpa == j))[-1], # female mil for rpa
                          t(subset(mil_m, rpa == j))[-1])) # male mil for rpa
    colnames(temp_mil) <- c('Female', 'Male')
    temp_mil$Female <-
      as.numeric(as.character(temp_mil$Female)) # change col type for later addition
    temp_mil$Male <- as.numeric(as.character(temp_mil$Male))
    
    temp2 <- subset(temp, rpa_abr == j) # subset by RPA
    
    temp3 <- matrix(NA, 2, 20) # create empty matrix to be populated
    temp3[1, 1:20] <-
      as.matrix(temp2[1, 1:20]) # fill first row with female pop
    temp3[2, 1:20] <-
      as.matrix(cbind(temp2[1, 1:2], temp2[1, 21:38])) # fill second row w. male pop
    
    temp4 <-
      as.data.frame(cbind(seq(1, 18, 1), t(temp3)[-c(1, 2), ])) # remove scenario and RPA info
    colnames(temp4) <- c('AgeC', 'Female', 'Male') # format col names
    temp4$Female <-
      as.numeric(as.character(temp4$Female)) # change col type for later addition
    temp4$Male <- as.numeric(as.character(temp4$Male))
    
    temp5 <- temp4 %>%
      mutate(Female = Female - temp_mil$Female,
             # subtract off military population for this particular rpa
             Male = Male - temp_mil$Male)
    write.csv(temp5,
              paste(out, j, '_', i, '_CivHHPop_101.csv', sep = ''),
              row.names = F) # write csv, changing name for MAPC to specify 101 vs. 97 munis
    
    # HH
    temp4$HH <-
      temp4$Female + temp4$Male # get household pop from pre-subtracted data
    temp6 <- temp4 %>%
      select(AgeC, HH) # reformat data frame
    write.csv(temp6,
              paste(out, j, '_', i, '_HHPop_101.csv', sep = ''),
              row.names = F) # write csv
  }
}
remove(temp, temp2, temp3, temp4, temp5)
