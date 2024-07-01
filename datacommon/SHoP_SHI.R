library(dplyr)
library(tidycensus)
library(sf)
library(geojsonsf)
library(ggplot2)
library(stringr)

# lberman 2024-06-24
# pull data from DataCommon, modify for specific SHoP use case 
# Title Chapter 40B Subsidized Housing Inventory (SHI)
# Table: hous_shi_m

### 0. SETUP VARIABLES SECTION

# 0.1 REQUIRED:  INPUT ACS 5YR VALUE
# input_year = "2018-22"

# SHI is for the entire range of years, using multi-year selection URL


# 0.2 REQUIRED: SET PATH TO OUTPUT FOLDER 
# write table to exp_path
# exp_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/DataCommon/"


###
## 1. Retrieve table directly from DataCommon

get_shi_range <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.hous_shi_m&database=ds&years=2023-06,2002-04,2003-10,2004-02,2005-02,2006-02,2007-02,2007-09,2008-02,2008-09,2009-09,2010-04,2011-05,2012-05,2012-10,2013-04,2014-12,2017-09,2020-12&year_col=shi_date"))



# 1.1 select columns
#select_data <- get_data %>% 
#  select(c(muni_id,municipal,geoid,logrecno,acs_year,vac_tot,vac_tot_m,))

# limit range of data to boston muni_id 352

calc <- get_shi_range %>% 
  filter(str_detect(muni_id, "\\b35\\b"))

# # 2. merge tabular data on muni_id (if more than one table is needed)

#calc <- get_b25002_b25003

# calc <- get_b25127 %>%
#   left_join(.,
#             get_b25024 %>% select(c(muni_id,hu,hu_me)),
#             by = c('muni_id' = 'muni_id')) %>%
#   arrange(muni_id)


# ## 3.  run calculations (as needed)
# calc <- calc %>%  #count of units built
#   rowwise() %>%
#   mutate(from_2000 = sum(c(c(h10,h0009)))) %>% 
#   mutate(from_2000m = moe_sum(moe = c(c(h10m,h0009m)), estimate = c(from_2000)))
# 
# calc <- calc %>%  #percent of units built
#   mutate(f2000_pct = (from_2000/hu)*100) %>% 
#   mutate(f2000_pctm = round(moe_prop(from_2000, hu, from_2000m, hu_me)*100, 2))
# 


## 3.1  replace NaN values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan)) #credit Hong Ooi on stack overflow

calc[is.nan(calc)] <- 0   #or whatever replacement value

### 4. get spatial data for join (if needed)

# 4.1 AGOL version
# from services list of arcgis server: use query form to get all records, for example: Area_acres > 0, format geojson, then copy URL of resulting geojson object 
# works but lacks muni_id and towns are spelled all caps

#request <- "https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/Massachusetts_Municipalities_Hosted/FeatureServer/0/query?where=AREA_ACRES+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token="
#geo_raw <- st_read(request)
#st_crs(geo_raw)

# 4.2 read shp from K drive version
# geo_shp <- read_sf("K:/DataServices/Datasets/Boundaries/Municipal/ma_municipalities.shp") 
# st_crs(geo_shp)
# 
# # 5.  join b25127 table df to shp
# shp_join <- geo_shp %>%
#   left_join(.,
#             calc %>% select(-c(seq_id,municipal)),
#             by = c('muni_id' = 'muni_id')) %>% 
#   arrange(muni_id) 
# 

# 6. sample chloropleth map

# 6.1  plain black outline
# ggplot(shp_join) +
#   geom_sf(fill = "white", color = "black", linewidth = 0.3) +
#   theme_void()

# 6.2 with chloropleth on vac_pct column Vacancy Percentage

# calc <- calc %>%  # eg whi_ohu
#   mutate(whi_ohu = as.integer(whi_ohu)) %>% 
#   mutate(aa_ohu = as.integer(aa_ohu)) %>% 
#   mutate(lat_ohu = as.integer(lat_ohu))
# 
# map_title = paste0(input_year," White Owner Occupancy")
# 
# whi_ohu_plot <- ggplot(shp_join) +
#   geom_sf(aes(fill = whi_ohu), linewidth = 0, alpha = 0.9) +
#   theme_void() +
#   scale_fill_viridis_c(
#     trans = "log",
#     # breaks = c(1, 5, 10, 20, 50, 100),
#     # labels = c('1', '5', '10', 20, 50, 100),
#     name = "units",
#     guide = guide_legend(
#       keyheight = unit(3, units = "mm"),
#       keywidth = unit(12, units = "mm"),
#       label.position = "bottom",
#       title.position = "top",
#       nrow = 1
#     )
#   ) +
#   labs(
#     title = map_title,
#     subtitle = "count of Housing Units",
#     caption = "Data: ACS Housing Tenure by Race [b25002 & b25003]"
#   ) +
#   theme(
#     text = element_text(color = "#22211d"),
#     plot.background = element_rect(fill = "#f5f5f2", color = NA),
#     panel.background = element_rect(fill = "#f5f5f2", color = NA),
#     legend.background = element_rect(fill = "#f5f5f2", color = NA),
#     plot.title = element_text(
#       size = 20, hjust = 0.01, color = "#4e4d47",
#       margin = margin(
#         b = -0.1, t = 0.4, l = 2,
#         unit = "cm"
#       )
#     ),
#     plot.subtitle = element_text(
#       size = 15, hjust = 0.01,
#       color = "#4e4d47",
#       margin = margin(
#         b = -0.1, t = 0.43, l = 2,
#         unit = "cm"
#       )
#     ),
#     plot.caption = element_text(
#       size = 10,
#       color = "#4e4d47",
#       margin = margin(
#         b = 0.3, r = -99, t = 0.3,
#         unit = "cm"
#       )
#     ),
#     legend.position = c(0.2, 0.09)
#   )
# 
# whi_ohu_plot

#6.3  plain bar graph

# fix text date into number for graph
calc <- calc %>% 
  mutate(shi_qtr = shi_date)

calc$shi_qtr <- gsub('-', '', calc$shi_qtr)

calc <- calc %>% 
  mutate(date = as.integer(shi_qtr)) %>% 
  mutate(pct = shi_p) %>% 
  select(-c(shi_qtr))


set_chart <- ggplot(calc, aes(x = date, y = pct))

# Label inside bars, vjust = 1.6
set_chart + geom_col(fill = "#0073C2FF")+
  geom_text(aes(label = pct), vjust = -0.3)
#    geom_text(aes(label = shi_p), vjust = 1.6, color = "white")


## 7. EXPORT THE DATA to file for input year
write.csv(get_shi_range, paste0(exp_path, "SHoP_SHI_2002-2023.csv"))


