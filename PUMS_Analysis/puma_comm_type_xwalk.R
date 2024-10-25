library(dplyr)
library(tidycensus)
library(sf)
library(geojsonsf)
library(ggplot2)
library(readxl)
library(stringi)

# lberman 2024-10-24
# try to map puma to community types


### 0. SETUP VARIABLES SECTION

# 0.1 REQUIRED:  INPUT ACS 5YR VALUE
input_year = "2018-22"

# 0.2 REQUIRED: SET PATHS
# write table to exp_path
# exp_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

commtype_path = "K:/DataServices/Projects/Current_Projects/Community Types/"

shp_path = "K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Reweighter/Municipal_Controls_2010/PUMA_muni_crosswalk/"


# 1 get keys

ct_keys <- mapcdatakeys::community_type


# 2 load pop data

muni_pop <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.demo_general_demographics_m&database=ds&years=2010&year_col=acs_year")) %>% 
  select(c(muni_id,pop,hh))


# pums_inter <- read.csv("K:DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Regional_Control_Totals/pums_muni_inter.csv") %>% 
#   select(c(TOWN_ID,TOWN,POP2010,SUM_ACRES,PUMACE10),everything())

# 2.1 read shp from K drive

puma_10_shp <- read_sf(paste0(shp_path,"tl_2019_25_puma10.shp")) %>% 
  st_transform(26986)
st_crs(puma_10_shp)

# ct164_shp <- read_sf("K:/DataServices/Projects/Current_Projects/Community Types/maps/project file/CommType164.shp") 
# st_crs(ct164_shp)

muni_shp <- read_sf("K:/DataServices/Datasets/Boundaries/Municipal/ma_municipalities.shp") 
st_crs(muni_shp)


# 2.2  centroids of muni
muni_centroid <- muni_shp %>% 
  st_transform(4326) %>% 
  mutate(centroids = st_centroid(st_geometry(.)))

muni_xy <- muni_centroid %>%
  mutate(long = unlist(map(muni_centroid$centroids,1)),
         lat = unlist(map(muni_centroid$centroids,2)))

muni_xy_df <- muni_xy %>% st_drop_geometry() %>% 
  select(-c(centroids))

muni_xy_sf <- st_as_sf(muni_xy_df, coords = c(6:7))

st_crs(muni_xy_sf) <- 4326

muni_xy_sf <- muni_xy_sf %>% 
  st_transform(26986) 
  

# puma_20 <- read_sf("H:/0_PROJECTS/2024_puma_comm_type/ipums_puma_2020/ipums_puma_2020.shp") 
# puma_20_ma <- puma_20 %>% 
#   filter(STATEFIP == "25")
# st_crs(puma_20_ma)
# puma_20_ma <- puma_20_ma %>%
#   st_transform(26986)
# st_crs(puma_20_ma)


# 3. run intersection

puma_mu_xy <- st_intersection(puma_10_shp, muni_xy_sf)

puma_mu_xy <- puma_mu_xy %>% 
  select(c(muni_id,municipal,PUMACE10),everything())


# 4.  join keys to intersection
puma_keys <- puma_mu_xy %>%
  left_join(.,
            ct_keys %>% select(-c(muni_id,muni_name),everything()),
            by = c('muni_id' = 'muni_id')) %>% 
  arrange(PUMACE10) 


# 5.  join pop to intersection
puma_pop <- puma_keys %>%
  left_join(.,
            muni_pop %>% select(c(muni_id,pop,hh)),
            by = c('muni_id' = 'muni_id')) %>% 
  arrange(PUMACE10) 


# 6. concatenate PUMACE10 with cmtyp08_id for aggr value

puma_concat <- puma_pop %>% 
  mutate(puma_type = paste0(PUMACE10,"_",cmtyp08_id))

###
# 7 summarize on comm type type one-by-one
###

# 7.0 convert from spatial to plain df
puma_df <- puma_concat %>% st_drop_geometry() #%>% 
#  filter(PUMACE10 == '00100')
class(puma_df)

# 7.1  check to see what the comm types are for input PUMA value
puma_df_uniq_types <- puma_df %>% 
  distinct(cmtyp08)

# 7.2 set aggr function
commtype_aggr_func <- function(grp_id, grp_name){
  puma_df %>%  
    group_by({{grp_id}}, {{grp_name}}) %>%
    summarise(across(.cols = c(pop,hh), sum, na.rm = TRUE)) %>% 
    rowwise() %>% 
    ungroup() %>% 
    dplyr::rename(puma_type = 1, municipal = 2) %>% 
    select(c(puma_type, municipal), everything())
}

# 7.3 run aggregation for pop by comm type
commtype_aggr <-
  bind_rows(
    commtype_aggr_func(grp_id = puma_type, grp_name = cmtyp08)) %>% 
  filter(!is.na(puma_type))  %>% 
  filter(municipal != "Metropolitan Area Planning Council") %>% 
  dplyr::distinct()


### 
# 8 find max value pop row for each uniq PUMACE10
###

# 8.1 reset the PUMACE10
commtype_loop <- commtype_aggr %>% 
  mutate(PUMACE10 = stri_sub(puma_type,1,5)
  )

# 8.2  find the uniq vals to slice
puma10ce_uniq <- commtype_loop %>% 
  distinct(PUMACE10)

# 8.3 export uniq to concat the slice commands
write.csv(puma10ce_uniq,"H:/0_PROJECTS/2024_puma_comm_type/ipums_puma_2020/uniq_puma10ce.csv")

# concat example: 

# 8.4 slice each max pop value
# test version

slice_00100 <- commtype_loop %>% 
  filter(PUMACE10 == '00100') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00200 <- commtype_loop %>% 
  filter(PUMACE10 == '00200') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00300 <- commtype_loop %>% 
  filter(PUMACE10 == '00300') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00301 <- commtype_loop %>% 
  filter(PUMACE10 == '00301') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00302 <- commtype_loop %>% 
  filter(PUMACE10 == '00302') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00303 <- commtype_loop %>% 
  filter(PUMACE10 == '00303') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00304 <- commtype_loop %>% 
  filter(PUMACE10 == '00304') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00400 <- commtype_loop %>% 
  filter(PUMACE10 == '00400') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00501 <- commtype_loop %>% 
  filter(PUMACE10 == '00501') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00502 <- commtype_loop %>% 
  filter(PUMACE10 == '00502') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00503 <- commtype_loop %>% 
  filter(PUMACE10 == '00503') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00504 <- commtype_loop %>% 
  filter(PUMACE10 == '00504') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00505 <- commtype_loop %>% 
  filter(PUMACE10 == '00505') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00506 <- commtype_loop %>% 
  filter(PUMACE10 == '00506') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00507 <- commtype_loop %>% 
  filter(PUMACE10 == '00507') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00508 <- commtype_loop %>% 
  filter(PUMACE10 == '00508') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00701 <- commtype_loop %>% 
  filter(PUMACE10 == '00701') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00702 <- commtype_loop %>% 
  filter(PUMACE10 == '00702') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00703 <- commtype_loop %>% 
  filter(PUMACE10 == '00703') %>% 
  slice_max(pop, with_ties = FALSE)


slice_00704 <- commtype_loop %>% 
  filter(PUMACE10 == '00704') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01000 <- commtype_loop %>% 
  filter(PUMACE10 == '01000') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01300 <- commtype_loop %>% 
  filter(PUMACE10 == '01300') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01400 <- commtype_loop %>% 
  filter(PUMACE10 == '01400') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01600 <- commtype_loop %>% 
  filter(PUMACE10 == '01600') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01900 <- commtype_loop %>% 
  filter(PUMACE10 == '01900') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01901 <- commtype_loop %>% 
  filter(PUMACE10 == '01901') %>% 
  slice_max(pop, with_ties = FALSE)


slice_01902 <- commtype_loop %>% 
  filter(PUMACE10 == '01902') %>% 
  slice_max(pop, with_ties = FALSE)


slice_02400 <- commtype_loop %>% 
  filter(PUMACE10 == '02400') %>% 
  slice_max(pop, with_ties = FALSE)


slice_02800 <- commtype_loop %>% 
  filter(PUMACE10 == '02800') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03304 <- commtype_loop %>% 
  filter(PUMACE10 == '03304') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03306 <- commtype_loop %>% 
  filter(PUMACE10 == '03306') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03400 <- commtype_loop %>% 
  filter(PUMACE10 == '03400') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03500 <- commtype_loop %>% 
  filter(PUMACE10 == '03500') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03601 <- commtype_loop %>% 
  filter(PUMACE10 == '03601') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03602 <- commtype_loop %>% 
  filter(PUMACE10 == '03602') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03603 <- commtype_loop %>% 
  filter(PUMACE10 == '03603') %>% 
  slice_max(pop, with_ties = FALSE)


slice_03900 <- commtype_loop %>% 
  filter(PUMACE10 == '03900') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04000 <- commtype_loop %>% 
  filter(PUMACE10 == '04000') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04200 <- commtype_loop %>% 
  filter(PUMACE10 == '04200') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04301 <- commtype_loop %>% 
  filter(PUMACE10 == '04301') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04302 <- commtype_loop %>% 
  filter(PUMACE10 == '04302') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04303 <- commtype_loop %>% 
  filter(PUMACE10 == '04303') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04500 <- commtype_loop %>% 
  filter(PUMACE10 == '04500') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04700 <- commtype_loop %>% 
  filter(PUMACE10 == '04700') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04800 <- commtype_loop %>% 
  filter(PUMACE10 == '04800') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04901 <- commtype_loop %>% 
  filter(PUMACE10 == '04901') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04902 <- commtype_loop %>% 
  filter(PUMACE10 == '04902') %>% 
  slice_max(pop, with_ties = FALSE)


slice_04903 <- commtype_loop %>% 
  filter(PUMACE10 == '04903') %>% 
  slice_max(pop, with_ties = FALSE)



## 9 bind all the max rows
slice_bind <- rbind(slice_00100, slice_00200, slice_00300, slice_00301, slice_00302, slice_00303, slice_00304, slice_00400, slice_00501, slice_00502, slice_00503, slice_00504, slice_00505, slice_00506, slice_00507, slice_00508, slice_00701, slice_00702, slice_00703, slice_00704, slice_01000, slice_01300, slice_01400, slice_01600, slice_01900, slice_01901, slice_01902, slice_02400, slice_02800, slice_03304, slice_03306, slice_03400, slice_03500, slice_03601, slice_03602, slice_03603, slice_03900, slice_04000, slice_04200, slice_04301, slice_04302, slice_04303, slice_04500, slice_04700, slice_04800, slice_04901, slice_04902, slice_04903)

# 9.1 drop & rename cols
puma_cmtyp08 <- slice_bind %>% 
  mutate(
    max_pop_type = puma_type,
    max_pop_name = municipal,
    max_pop = pop
  ) %>% 
  mutate(cmtyp08_id = stri_sub(max_pop_type,-3,-1)) %>% 
  select(-c(municipal,pop,hh,puma_type)) %>% 
  arrange(PUMACE10)

class(puma_cmtyp08)




# 10 export uniq to concat the slice commands
write.csv(puma_cmtyp08,"H:/0_PROJECTS/2024_puma_comm_type/ipums_puma_2020/puma_cmtyp08_xwalk.csv")


#####


# rm all slices at once
#rm(list = ls()[grepl("^slice", ls())])

# 10 join to shp for checking 

puma_map <- puma_10_shp %>%
  left_join(.,
            puma_cmtyp08 %>% select(c(PUMACE10),everything()),
            by = c('PUMACE10' = 'PUMACE10')) %>% 
  arrange(PUMACE10) 

puma_map <- merge(puma_10_shp, puma_cmtyp08, all = TRUE)

class(puma_10_shp)



