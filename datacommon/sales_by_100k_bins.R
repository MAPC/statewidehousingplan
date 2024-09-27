# SUMMARIZE WARREN GROUP RESIDENTIAL SALES DATA

# code by lberman 2024-09  
# create bins by price of 100,000 with sales per bin
# get total sales by county
# total sales by type (R1F or CON) within counties are repeated for each row in county 


# Libraries
library(tidyverse)
library(RPostgres)
library(dplyr)

# Data
warren_year <- 2023

warren_data <- read_csv("K:/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified/20240201_warren_group_2000_2023_residential_final.csv")

# Process -----------------------------------------------------------------


# set column with bin value for each sales price, using $100,000 bins ------------------------------------------------------------

raw_100k_bins <- warren_data %>% 
  filter(year == warren_year) %>% 
  select(county_id, county, muni_id, municipal, ct_id, year, restype, usecode, usage, price) %>% 
  mutate(bins = "0")

raw_100k_bins$bins[raw_100k_bins$price <= 100000] <- "100k"
raw_100k_bins$bins[raw_100k_bins$price > 100000 & raw_100k_bins$price < 200001 ] <- "200k"
raw_100k_bins$bins[raw_100k_bins$price > 200000 & raw_100k_bins$price < 300001 ] <- "300k"
raw_100k_bins$bins[raw_100k_bins$price > 300000 & raw_100k_bins$price < 400001 ] <- "400k"
raw_100k_bins$bins[raw_100k_bins$price > 400000 & raw_100k_bins$price < 500001 ] <- "500k"
raw_100k_bins$bins[raw_100k_bins$price > 500000 & raw_100k_bins$price < 600001 ] <- "600k"
raw_100k_bins$bins[raw_100k_bins$price > 600000] <- "600k_up"

sf_100k_bins <- raw_100k_bins %>% 
  filter(restype %in% c('R1F', 'CON')) %>% 
  mutate(county_sum = 0) %>% 
  mutate(bins_name = bins) %>% 
  mutate(restype_name = restype)



# list possible counties

county_list <- sf_100k_bins %>% 
  distinct(county_id, county) %>% 
  arrange(county)

# aggregations   group by COUNTY_ID SUMMARIZE by BINS, RESTYPE

#--------------- COUNTIES --------------#

## barnstable county  BEGIN ##
barns_cnty <- sf_100k_bins %>% 
  filter(county_id == 25001) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
barns_bins <- barns_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
barns_type <- barns_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
barns_set <- barns_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
barns_R1F <- barns_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
barns_CON <- barns_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

barns_merge <- left_join(barns_R1F, barns_CON) %>% 
  select(-c(type_count,restype,usecode,usage))
  
get_county_id <- head(barns_merge,1) %>% 
  select(county_id)

get_county_name <- head(barns_merge,1) %>% 
  select(c(county))


barns_summary <- barns_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

barns_summary$county_id[is.na(barns_summary$county_id)] <- get_county_id
barns_summary$county[is.na(barns_summary$county)] <- get_county_name
barns_summary$year[is.na(barns_summary$year)] <- warren_year

## barnstable county  END ##

# ------ <<<<

## RESET county_id value for each COUNTY ##

## berkshire county  BEGIN ##
berks_cnty <- sf_100k_bins %>% 
  filter(county_id == 25003) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
berks_bins <- berks_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
berks_type <- berks_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
berks_set <- berks_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
berks_R1F <- berks_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
berks_CON <- berks_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

berks_merge <- left_join(berks_R1F, berks_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(berks_merge,1) %>% 
  select(county_id)

get_county_name <- head(berks_merge,1) %>% 
  select(c(county))


berks_summary <- berks_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

berks_summary$county_id[is.na(berks_summary$county_id)] <- get_county_id
berks_summary$county[is.na(berks_summary$county)] <- get_county_name
berks_summary$year[is.na(berks_summary$year)] <- warren_year

## berkshire county  END ##

# ---- <<<<

## bristol county  BEGIN ##
brist_cnty <- sf_100k_bins %>% 
  filter(county_id == 25005) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
brist_bins <- brist_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
brist_type <- brist_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
brist_set <- brist_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
brist_R1F <- brist_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
brist_CON <- brist_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

brist_merge <- left_join(brist_R1F, brist_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(brist_merge,1) %>% 
  select(county_id)

get_county_name <- head(brist_merge,1) %>% 
  select(c(county))


brist_summary <- brist_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

brist_summary$county_id[is.na(brist_summary$county_id)] <- get_county_id
brist_summary$county[is.na(brist_summary$county)] <- get_county_name
brist_summary$year[is.na(brist_summary$year)] <- warren_year

## bristol county  END ##

# ---- <<<<

## dukes county  BEGIN ##
dukes_cnty <- sf_100k_bins %>% 
  filter(county_id == 25007) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
dukes_bins <- dukes_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
dukes_type <- dukes_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
dukes_set <- dukes_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
dukes_R1F <- dukes_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
dukes_CON <- dukes_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

dukes_merge <- left_join(dukes_R1F, dukes_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(dukes_merge,1) %>% 
  select(county_id)

get_county_name <- head(dukes_merge,1) %>% 
  select(c(county))


dukes_summary <- dukes_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

dukes_summary$county_id[is.na(dukes_summary$county_id)] <- get_county_id
dukes_summary$county[is.na(dukes_summary$county)] <- get_county_name
dukes_summary$year[is.na(dukes_summary$year)] <- warren_year

## dukes county  END ##

# ---- <<<<

## ESSEX county  BEGIN ##
essex_cnty <- sf_100k_bins %>% 
  filter(county_id == 25009) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
essex_bins <- essex_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
essex_type <- essex_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
essex_set <- essex_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
essex_R1F <- essex_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
essex_CON <- essex_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

essex_merge <- left_join(essex_R1F, essex_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(essex_merge,1) %>% 
  select(county_id)

get_county_name <- head(essex_merge,1) %>% 
  select(c(county))


essex_summary <- essex_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

essex_summary$county_id[is.na(essex_summary$county_id)] <- get_county_id
essex_summary$county[is.na(essex_summary$county)] <- get_county_name
essex_summary$year[is.na(essex_summary$year)] <- warren_year

## essex county  END ##

# ---- <<<<

## FRANKLIN county  BEGIN ##
frank_cnty <- sf_100k_bins %>% 
  filter(county_id == 25011) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
frank_bins <- frank_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
frank_type <- frank_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
frank_set <- frank_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
frank_R1F <- frank_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
frank_CON <- frank_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

frank_merge <- left_join(frank_R1F, frank_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(frank_merge,1) %>% 
  select(county_id)

get_county_name <- head(frank_merge,1) %>% 
  select(c(county))


frank_summary <- frank_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

frank_summary$county_id[is.na(frank_summary$county_id)] <- get_county_id
frank_summary$county[is.na(frank_summary$county)] <- get_county_name
frank_summary$year[is.na(frank_summary$year)] <- warren_year

## frank county  END ##

# ---- <<<<

## HAMPDEN county  BEGIN ##
hampd_cnty <- sf_100k_bins %>% 
  filter(county_id == 25013) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
hampd_bins <- hampd_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
hampd_type <- hampd_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
hampd_set <- hampd_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
hampd_R1F <- hampd_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
hampd_CON <- hampd_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

hampd_merge <- left_join(hampd_R1F, hampd_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(hampd_merge,1) %>% 
  select(county_id)

get_county_name <- head(hampd_merge,1) %>% 
  select(c(county))


hampd_summary <- hampd_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

hampd_summary$county_id[is.na(hampd_summary$county_id)] <- get_county_id
hampd_summary$county[is.na(hampd_summary$county)] <- get_county_name
hampd_summary$year[is.na(hampd_summary$year)] <- warren_year

## hampd county  END ##

# ----<<<<

## HAMPSHIRE county  BEGIN ##
hmpsh_cnty <- sf_100k_bins %>% 
  filter(county_id == 25015) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
hmpsh_bins <- hmpsh_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
hmpsh_type <- hmpsh_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
hmpsh_set <- hmpsh_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
hmpsh_R1F <- hmpsh_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
hmpsh_CON <- hmpsh_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

hmpsh_merge <- left_join(hmpsh_R1F, hmpsh_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(hmpsh_merge,1) %>% 
#  select(c(as.numeric(county_id)))
  select(county_id)

get_county_name <- head(hmpsh_merge,1) %>% 
  select(c(county))


hmpsh_summary <- hmpsh_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

hmpsh_summary$county_id[is.na(hmpsh_summary$county_id)] <- get_county_id
hmpsh_summary$county[is.na(hmpsh_summary$county)] <- get_county_name
hmpsh_summary$year[is.na(hmpsh_summary$year)] <- warren_year

## hmpsh county  END ##

# ----<<<<

## MIDDLESEX county  BEGIN ##
midsx_cnty <- sf_100k_bins %>% 
  filter(county_id == 25017) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
midsx_bins <- midsx_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
midsx_type <- midsx_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
midsx_set <- midsx_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
midsx_R1F <- midsx_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
midsx_CON <- midsx_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

midsx_merge <- left_join(midsx_R1F, midsx_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(midsx_merge,1) %>% 
#  select(c(as.numeric(county_id)))
  select(county_id)

get_county_name <- head(midsx_merge,1) %>% 
  select(c(county))
 

midsx_summary <- midsx_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

midsx_summary$county_id[is.na(midsx_summary$county_id)] <- get_county_id
midsx_summary$county[is.na(midsx_summary$county)] <- get_county_name
midsx_summary$year[is.na(midsx_summary$year)] <- warren_year

## midsx county  END ##

# ----<<<<

## NANTUCKET county  BEGIN ##
nantk_cnty <- sf_100k_bins %>% 
  filter(county_id == 25019) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
nantk_bins <- nantk_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
nantk_type <- nantk_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
nantk_set <- nantk_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
nantk_R1F <- nantk_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
nantk_CON <- nantk_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

nantk_merge <- left_join(nantk_R1F, nantk_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(nantk_merge,1) %>% 
#  select(c(as.numeric(county_id)))
  select(county_id)

get_county_name <- head(nantk_merge,1) %>% 
  select(c(county))


nantk_summary <- nantk_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

nantk_summary$county_id[is.na(nantk_summary$county_id)] <- get_county_id
nantk_summary$county[is.na(nantk_summary$county)] <- get_county_name
nantk_summary$year[is.na(nantk_summary$year)] <- warren_year

## nantk county  END ##

# ----<<<<

## NORFOLK county  BEGIN ##
norfk_cnty <- sf_100k_bins %>% 
  filter(county_id == 25021) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
norfk_bins <- norfk_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
norfk_type <- norfk_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
norfk_set <- norfk_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
norfk_R1F <- norfk_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
norfk_CON <- norfk_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

norfk_merge <- left_join(norfk_R1F, norfk_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(norfk_merge,1) %>% 
#  select(c(as.numeric(county_id)))
  select(county_id)

get_county_name <- head(norfk_merge,1) %>% 
  select(c(county))


norfk_summary <- norfk_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

norfk_summary$county_id[is.na(norfk_summary$county_id)] <- get_county_id
norfk_summary$county[is.na(norfk_summary$county)] <- get_county_name
norfk_summary$year[is.na(norfk_summary$year)] <- warren_year

## norfk county  END ##

# ----<<<<

## PLYMOUTH county  BEGIN ##
plymo_cnty <- sf_100k_bins %>% 
  filter(county_id == 25023) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
plymo_bins <- plymo_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
plymo_type <- plymo_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
plymo_set <- plymo_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
plymo_R1F <- plymo_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
plymo_CON <- plymo_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

plymo_merge <- left_join(plymo_R1F, plymo_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(plymo_merge,1) %>% 
#  select(c(as.numeric(county_id)))
  select(county_id)

get_county_name <- head(plymo_merge,1) %>% 
  select(c(county))


plymo_summary <- plymo_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

plymo_summary$county_id[is.na(plymo_summary$county_id)] <- get_county_id
plymo_summary$county[is.na(plymo_summary$county)] <- get_county_name
plymo_summary$year[is.na(plymo_summary$year)] <- warren_year

## plymo county  END ##

# ----<<<<

## SUFFOLK county  BEGIN ##
suffk_cnty <- sf_100k_bins %>% 
  filter(county_id == 25025) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
suffk_bins <- suffk_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
suffk_type <- suffk_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
suffk_set <- suffk_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
suffk_R1F <- suffk_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
suffk_CON <- suffk_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

suffk_merge <- left_join(suffk_R1F, suffk_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(suffk_merge,1) %>% 
  select(county_id)

get_county_name <- head(suffk_merge,1) %>% 
  select(c(county))


suffk_summary <- suffk_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

suffk_summary$county_id[is.na(suffk_summary$county_id)] <- get_county_id
suffk_summary$county[is.na(suffk_summary$county)] <- get_county_name
suffk_summary$year[is.na(suffk_summary$year)] <- warren_year

## suffk county  END ##

# ----<<<<

## WORCESTER county  BEGIN ##
worce_cnty <- sf_100k_bins %>% 
  filter(county_id == 25027) %>% 
  select(county_id, county, year, restype, usecode, usage, price, bins, bins_name, restype_name)

# group & count by bins
worce_bins <- worce_cnty %>% 
  group_by(bins) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(bins),everything()) %>% 
  mutate(bins_count = count) %>% 
  select(-c(count))


# group & count by restype
worce_type <- worce_bins %>% 
  group_by(restype) %>% 
  summarize(count = n(), across()) %>% 
  ungroup %>% 
  select(c(restype),everything()) %>%
  mutate(type_count = count) %>% 
  select(-c(count))


# subset to bins for each county split into types R1F and CON
worce_set <- worce_type %>% distinct(bins,restype, .keep_all=TRUE) 

# set left table for join
worce_R1F <- worce_set %>%  filter(restype == "R1F") %>% 
  mutate(type_R1F = restype) %>%
  mutate(R1F_count = type_count) %>% 
  select(-c(restype_name,price,bins_name))

# set right table for join
worce_CON <- worce_set %>%  filter(restype == "CON") %>% 
  mutate(type_CON = restype) %>% 
  mutate(CON_count = type_count) %>% 
  select(-c(restype,restype_name,bins_name,county_id,county,year,usecode,usage,price,bins_count,type_count))

#merge

worce_merge <- left_join(worce_R1F, worce_CON) %>% 
  select(-c(type_count,restype,usecode,usage))

get_county_id <- head(worce_merge,1) %>% 
  select(county_id)

get_county_name <- head(worce_merge,1) %>% 
  select(c(county))


worce_summary <- worce_merge %>%
  bind_rows(summarise(., 
                      bins_count = sum(bins_count),
                      bins = "TOTAL"))

worce_summary$county_id[is.na(worce_summary$county_id)] <- get_county_id
worce_summary$county[is.na(worce_summary$county)] <- get_county_name
worce_summary$year[is.na(worce_summary$year)] <- warren_year

## worce county  END ##



mass_bind <- rbind(barns_summary,
                   berks_summary,
                   brist_summary,
                   dukes_summary,
                   essex_summary,
                   frank_summary,
                   hampd_summary,
                   hmpsh_summary,
                   midsx_summary,
                   nantk_summary,
                   norfk_summary,
                   plymo_summary,
                   suffk_summary,
                   worce_summary)



# re-order cols
mass_100k_bins <- mass_bind [c('county_id',
                          'county',  
                          'year',  
                          'bins',  
                          'bins_count',
                          'type_R1F',
                          'R1F_count',
                          'type_CON',
                          'CON_count')]

# export
mass_100k_bins %>% 
  write_csv(., paste0("K:/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified/sales_by_100k_bins_2023.csv"))


