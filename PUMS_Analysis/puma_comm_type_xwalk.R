library(dplyr)
library(tidycensus)
library(tidyverse)
library(sf)
library(geojsonsf)
library(ggplot2)
library(readxl)
library(stringi)

# lberman 2024-10-24
# try to map puma to community types


### 0. SETUP VARIABLES SECTION

# 0.1 REQUIRED: SET PATHS
# write table to exp_path
# exp_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

commtype_path = "K:/DataServices/Projects/Current_Projects/Community Types/"

shp_path = "K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Reweighter/Municipal_Controls_2010/PUMA_muni_crosswalk/"


# 1 get keys

ct_keys <- mapcdatakeys::community_type


# 2 load pop data

muni_pop <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.demo_general_demographics_m&database=ds&years=2010&year_col=acs_year")) %>% 
  select(c(muni_id,pop,hh))


# pums_inter <- read.csv("K:DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/Regional_Control_Totals/pums_muni_inter.csv") #%>% 
#    select(c(TOWN_ID,TOWN,POP2010,SUM_ACRES,PUMACE10),everything())

# 2.1 read shp from K drive

puma_10_shp <- read_sf(paste0(shp_path,"tl_2019_25_puma10.shp")) %>% 
  st_transform(26986)
st_crs(puma_10_shp)

puma_10_shp <- puma_10_shp %>% 
  arrange(GEOID10)

# pums_inter <- pums_inter %>% 
#   arrange(TOWN) %>% 
#   select(c(TOWN,GEOID10),everything())

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


# 7. patch in missing BOSTON PUMAs [3301,3302,3303,3305] using 3304 as template

# 7.1 Allston Brighton Fenway
slice_bos_03301 <- puma_concat %>% 
  filter(PUMACE10 == '03304') %>% 
  mutate(PUMACE10 = '03301') %>% 
  mutate(GEOID10 = '2503301') %>% 
  mutate(puma_type = '03301_378') %>% 
  mutate(NAMELSAD10 = 'Boston City--Allston-Brighton-Fenway PUMA')

# 7.2 Boston City--Back Bay-Beacon Hill-Charlestown-East Boston-Central & South End
slice_bos_03302 <- puma_concat %>% 
  filter(PUMACE10 == '03304') %>% 
  mutate(PUMACE10 = '03302') %>% 
  mutate(GEOID10 = '2503302') %>% 
  mutate(puma_type = '03302_378') %>% 
  mutate(NAMELSAD10 = 'Boston City--Back Bay-Beacon Hill-Charlestown-East Boston-Central & South End PUMA')

# 7.3 Dorchester & South Boston
slice_bos_03303 <- puma_concat %>% 
  filter(PUMACE10 == '03304') %>% 
  mutate(PUMACE10 = '03303') %>% 
  mutate(GEOID10 = '2503303') %>% 
  mutate(puma_type = '03303_378') %>% 
  mutate(NAMELSAD10 = 'Boston City--Dorchester & South Boston PUMA')

# 7.4 Hyde Park Jamaica Plain Roslindale & West Roxbury 
slice_bos_03305 <- puma_concat %>% 
  filter(PUMACE10 == '03304') %>% 
  mutate(PUMACE10 = '03305') %>% 
  mutate(GEOID10 = '2503305') %>% 
  mutate(puma_type = '03305_378') %>% 
  mutate(NAMELSAD10 = 'Boston City--Hyde Park-Jamaica Plain-Roslindale & West Roxbury PUMA')

# 7.5 bind the Inner Boston PUMAS 
puma_bind <- rbind(puma_concat,slice_bos_03301,slice_bos_03302,slice_bos_03303,slice_bos_03305) %>% 
  arrange(PUMACE10)


###
# 8 summarize on comm type type one-by-one
###

# 8.0 convert from spatial to plain df
puma_df <- puma_bind %>% st_drop_geometry() #%>% 
#  filter(PUMACE10 == '00100')
class(puma_df)

# 8.1  check to see what the comm types are for input PUMA value
puma_df_uniq_types <- puma_df %>% 
  distinct(cmtyp08)

# 8.2 set aggr function
commtype_aggr_func <- function(grp_id, grp_name){
  puma_df %>%  
    group_by({{grp_id}}, {{grp_name}}) %>%
    summarise(across(.cols = c(pop,hh), sum, na.rm = TRUE)) %>% 
    rowwise() %>% 
    ungroup() %>% 
    dplyr::rename(puma_type = 1, municipal = 2) %>% 
    select(c(puma_type, municipal), everything())
}

# 8.3 run aggregation for pop by comm type
commtype_aggr <-
  bind_rows(
    commtype_aggr_func(grp_id = puma_type, grp_name = cmtyp08)) %>% 
  filter(!is.na(puma_type))  %>% 
  filter(municipal != "Metropolitan Area Planning Council") %>% 
  dplyr::distinct()


### 
# 9 pivot wider with type columns and aggr population for each
###

# 9.1 retrieve the PUMACE10
commtype_pumas <- commtype_aggr %>% 
  mutate(PUMACE10 = stri_sub(puma_type,1,5)
  )

# 9.2  pivot wider
pivot_on_types <- commtype_pumas %>%
  pivot_wider(names_from = municipal, values_from = pop) %>% 
  mutate(
    dev_sub = `Developing Suburb`, # backticks for blank spaces in var names
    reg_urb = `Regional Urban Center`,
    rur_twn = `Rural Town`,
    mat_sub = `Maturing Suburb`,
    inn_cor = `Inner Core`,
    puma_name = PUMACE10
  ) %>% 
  select(-c(`Developing Suburb`,`Regional Urban Center`,`Rural Town`,`Maturing Suburb`,`Inner Core`,hh,puma_type)) %>% 
  select(c(PUMACE10,puma_name),everything())


# 10. aggregate the puma_type rows into PUMAs

# 10.1 set aggr function
puma_aggr_func <- function(grp_id, grp_name){
  pivot_on_types %>%  
    group_by({{grp_id}}, {{grp_name}}) %>%
    summarise(across(.cols = c(dev_sub,reg_urb,rur_twn,mat_sub,inn_cor), sum, na.rm = TRUE)) %>% 
    rowwise() %>% 
    ungroup() %>% 
#    dplyr::rename(PUMACE10 = 1, puma_name = 2) %>% 
    select(c(PUMACE10,puma_name), everything())
}

# 10.2 run aggregation for pop by comm type
puma_aggr <-
  bind_rows(
    puma_aggr_func(grp_id = PUMACE10, grp_name = puma_name)) %>% 
  dplyr::distinct()


# 10.3 calc total of suburbs
puma_total <- puma_aggr %>% 
  mutate(suburb = (dev_sub + mat_sub)
  )

# 10.4 calc total pop for each PUMA
puma_total <- puma_total %>% 
  mutate(total_pop = (dev_sub + reg_urb + rur_twn + mat_sub + inn_cor)
  )


# 11 calculate the percentages
# 
# # 11.1  original types
# puma_pct <- puma_total %>% 
#   mutate(
#     dev_sub_p = round((dev_sub/total_pop),2),
#     reg_urb_p = round((reg_urb/total_pop),2),
#     rur_twn_p = round((rur_twn/total_pop),2),
#     mat_sub_p = round((mat_sub/total_pop),2),
#     inn_cor_p = round((dev_sub/total_pop),2),
#     suburb_p = round((suburb/total_pop),2)
#   )

# 11.1  original types
puma_pct <- puma_total %>% 
  mutate(
    dev_sub_p = round((dev_sub/total_pop),2),
    reg_urb_p = round((reg_urb/total_pop),2),
    rur_twn_p = round((rur_twn/total_pop),2),
    mat_sub_p = round((mat_sub/total_pop),2),
    inn_cor_p = round((inn_cor/total_pop),2),
    suburb_p = round((suburb/total_pop),2)
  )


# 12 assign the types based on percentages

# # 12.1 original rates
# puma_new_types <- puma_pct %>% 
#   mutate(
#     puma_type = case_when(
#       inn_cor_p > 0.7 ~ 'Core',
#       reg_urb_p > 0.7 ~ 'RegUrbCtr',
#       suburb_p > 0.7 ~ 'Suburb',
#       rur_twn_p > 0.7 ~ 'RuralTown',
#       (reg_urb_p + suburb) > 0.7 ~ 'RegUrbCnt+Suburb',
#       inn_cor_p <= 0 ~ 'Core'
#     )
#   )

# 12.1 original rates
puma_new_types <- puma_pct %>% 
  mutate(
    puma_type = case_when(
      inn_cor_p > 0.7 ~ 'Core',
      reg_urb_p > 0.7 ~ 'RegUrbCtr',
      suburb_p > 0.7 ~ 'Suburb',
      rur_twn_p > 0.7 ~ 'RuralTown',
      (reg_urb_p + suburb) > 0.7 ~ 'RegUrbCnt+Suburb',
      inn_cor_p = 1 ~ 'Core'
    )
  )

# 13 export new PUMA Community Types
write.csv(puma_new_types,"H:/0_PROJECTS/2024_puma_comm_type/ipums_puma_2020/new_puma_types_2024.csv")


#####



#  join to shp for checking 

puma_map <- puma_10_shp %>%
  left_join(.,
            puma_new_types %>% select(c(PUMACE10),everything()),
            by = c('PUMACE10' = 'PUMACE10')) %>% 
  arrange(PUMACE10) 

# puma_map <- merge(puma_10_shp, puma_cmtyp08, all = TRUE)
# 
# class(puma_10_shp)


puma_centroid <- puma_map %>% 
  st_transform(4326) %>% 
  mutate(centroids = st_centroid(st_geometry(.)))

puma_xy <- puma_centroid %>%
  mutate(long = unlist(map(puma_centroid$centroids,1)),
         lat = unlist(map(puma_centroid$centroids,2)))

map_title = paste0("puma by type")

puma_plot <- ggplot(puma_xy) +
  geom_sf(aes(fill = puma_type), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_colour_viridis_d(
    #    trans = "log", breaks = c(1, 5, 10, 20, 50, 100),
    name = "pct",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    )
  ) +
  labs(
    title = map_title,
    subtitle = "chloropleth of community types",
    caption = "Data: percent formula for each types within PUMA"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 20, hjust = 0.01, color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      size = 15, hjust = 0.01,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.43, l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(
      size = 10,
      color = "#4e4d47",
      margin = margin(
        b = 0.3, r = -99, t = 0.3,
        unit = "cm"
      )
    ),
    legend.position = c(0.1, 0.09)
  )

puma_plot

