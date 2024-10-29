library(dplyr)
library(tidycensus)
library(sf)
library(geojsonsf)
library(ggplot2)
library(readxl)
library(stringi)
library(purrr)

# lberman 2024-10-24
# pumas by count of community types within each puma


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


# 2  prepare spatial data for intersection

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


# 5. concatenate PUMACE10 with cmtyp08_id for aggr value

puma_concat <- puma_keys %>% 
  mutate(puma_type = paste0(PUMACE10,"_",cmtyp08_id))

###
# 6 summarize on comm type by counting number of occurrences with each PUMA
###

# 6.0 convert from spatial to plain df
puma_df <- puma_concat %>% st_drop_geometry() #%>% 
#  filter(PUMACE10 == '00100')
class(puma_df)

# check to see what the comm types are for input PUMA value
# puma_df_uniq_types <- puma_df %>% 
#   distinct(cmtyp08)

# 6.1   count the unique types within each PUMACE10 group
count_types <- puma_df %>% 
  group_by(PUMACE10, puma_type) %>% 
  summarize(count=n())


# 6.2 trim orig table for join
puma_trim <- puma_df %>% 
  distinct(puma_type, .keep_all = TRUE) %>% 
  select(c(puma_type,cmtyp08,cmtyp08_id))


# 6.3 join keys info to PUMA
puma_w_counts <- count_types %>%
  left_join(.,
            puma_trim %>% select(c(puma_type),everything()),
            by = c('puma_type' = 'puma_type')) %>%
  unique()
  arrange(PUMACE10,cmtyp08)
  
# 
# 
# ### 
# # 7 find max count of types for each uniq PUMACE10
# ###
  
# 7.1 slice each max count value

  slice_00100 <- puma_w_counts %>% 
    filter(PUMACE10 == '00100') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00200 <- puma_w_counts %>% 
    filter(PUMACE10 == '00200') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00300 <- puma_w_counts %>% 
    filter(PUMACE10 == '00300') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00301 <- puma_w_counts %>% 
    filter(PUMACE10 == '00301') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00302 <- puma_w_counts %>% 
    filter(PUMACE10 == '00302') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00303 <- puma_w_counts %>% 
    filter(PUMACE10 == '00303') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00304 <- puma_w_counts %>% 
    filter(PUMACE10 == '00304') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00400 <- puma_w_counts %>% 
    filter(PUMACE10 == '00400') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00501 <- puma_w_counts %>% 
    filter(PUMACE10 == '00501') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00502 <- puma_w_counts %>% 
    filter(PUMACE10 == '00502') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00503 <- puma_w_counts %>% 
    filter(PUMACE10 == '00503') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00504 <- puma_w_counts %>% 
    filter(PUMACE10 == '00504') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00505 <- puma_w_counts %>% 
    filter(PUMACE10 == '00505') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00506 <- puma_w_counts %>% 
    filter(PUMACE10 == '00506') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00507 <- puma_w_counts %>% 
    filter(PUMACE10 == '00507') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00508 <- puma_w_counts %>% 
    filter(PUMACE10 == '00508') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00701 <- puma_w_counts %>% 
    filter(PUMACE10 == '00701') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00702 <- puma_w_counts %>% 
    filter(PUMACE10 == '00702') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00703 <- puma_w_counts %>% 
    filter(PUMACE10 == '00703') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_00704 <- puma_w_counts %>% 
    filter(PUMACE10 == '00704') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01000 <- puma_w_counts %>% 
    filter(PUMACE10 == '01000') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01300 <- puma_w_counts %>% 
    filter(PUMACE10 == '01300') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01400 <- puma_w_counts %>% 
    filter(PUMACE10 == '01400') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01600 <- puma_w_counts %>% 
    filter(PUMACE10 == '01600') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01900 <- puma_w_counts %>% 
    filter(PUMACE10 == '01900') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01901 <- puma_w_counts %>% 
    filter(PUMACE10 == '01901') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_01902 <- puma_w_counts %>% 
    filter(PUMACE10 == '01902') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_02400 <- puma_w_counts %>% 
    filter(PUMACE10 == '02400') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_02800 <- puma_w_counts %>% 
    filter(PUMACE10 == '02800') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03304 <- puma_w_counts %>% 
    filter(PUMACE10 == '03304') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03306 <- puma_w_counts %>% 
    filter(PUMACE10 == '03306') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03400 <- puma_w_counts %>% 
    filter(PUMACE10 == '03400') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03500 <- puma_w_counts %>% 
    filter(PUMACE10 == '03500') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03601 <- puma_w_counts %>% 
    filter(PUMACE10 == '03601') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03602 <- puma_w_counts %>% 
    filter(PUMACE10 == '03602') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03603 <- puma_w_counts %>% 
    filter(PUMACE10 == '03603') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_03900 <- puma_w_counts %>% 
    filter(PUMACE10 == '03900') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04000 <- puma_w_counts %>% 
    filter(PUMACE10 == '04000') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04200 <- puma_w_counts %>% 
    filter(PUMACE10 == '04200') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04301 <- puma_w_counts %>% 
    filter(PUMACE10 == '04301') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04302 <- puma_w_counts %>% 
    filter(PUMACE10 == '04302') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04303 <- puma_w_counts %>% 
    filter(PUMACE10 == '04303') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04500 <- puma_w_counts %>% 
    filter(PUMACE10 == '04500') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04700 <- puma_w_counts %>% 
    filter(PUMACE10 == '04700') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04800 <- puma_w_counts %>% 
    filter(PUMACE10 == '04800') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04901 <- puma_w_counts %>% 
    filter(PUMACE10 == '04901') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04902 <- puma_w_counts %>% 
    filter(PUMACE10 == '04902') %>% 
    slice_max(count, with_ties = FALSE)
  
  
  slice_04903 <- puma_w_counts %>% 
    filter(PUMACE10 == '04903') %>% 
    slice_max(count, with_ties = FALSE)
  
## 8 bind all the max rows
slice_bind <- rbind(slice_00100, slice_00200, slice_00300, slice_00301, slice_00302, slice_00303, slice_00304, slice_00400, slice_00501, slice_00502, slice_00503, slice_00504, slice_00505, slice_00506, slice_00507, slice_00508, slice_00701, slice_00702, slice_00703, slice_00704, slice_01000, slice_01300, slice_01400, slice_01600, slice_01900, slice_01901, slice_01902, slice_02400, slice_02800, slice_03304, slice_03306, slice_03400, slice_03500, slice_03601, slice_03602, slice_03603, slice_03900, slice_04000, slice_04200, slice_04301, slice_04302, slice_04303, slice_04500, slice_04700, slice_04800, slice_04901, slice_04902, slice_04903)

# 9 export uniq to concat the slice commands
write.csv(slice_bind,"H:/0_PROJECTS/2024_puma_comm_type/ipums_puma_2020/puma_cmtyp08_xwalk_by_type_count.csv")


# 10 test map

class(slice_bind)
puma_map <- puma_10_shp %>%
  left_join(.,
            slice_bind %>% select(-c(PUMACE10,cmtyp08,cmtyp08_id),everything()),
            by = c('PUMACE10' = 'PUMACE10')) %>% 
  arrange(PUMACE10) 
class(puma_map)
st_crs(puma_map)

puma_centroid <- puma_map %>% 
  st_transform(4326) %>% 
  mutate(centroids = st_centroid(st_geometry(.)))

puma_xy <- puma_centroid %>%
  mutate(long = unlist(map(puma_centroid$centroids,1)),
         lat = unlist(map(puma_centroid$centroids,2)))

map_title = paste0("puma by type")

puma_plot <- ggplot(puma_xy) +
  geom_sf(aes(fill = cmtyp08), linewidth = 0, alpha = 0.9) +
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
    caption = "Data: highest count of type for each PUMA"
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
