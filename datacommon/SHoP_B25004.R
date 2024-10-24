library(dplyr)
library(tidycensus)
library(sf)
library(geojsonsf)
library(ggplot2)

# lberman 2024-06-18
# pull data from DataCommon, modify for specific SHoP use case 
# Vacancy Status b25004_hu_vacancy_status_acs_m
# Housing Units in a Structure b25024_hu_units_in_structure_acs_m


### 0. SETUP VARIABLES SECTION

# 0.1 REQUIRED:  INPUT ACS 5YR VALUE
input_year = "2018-22"

# 0.2 REQUIRED: SET PATH TO OUTPUT FOLDER 
# write table to exp_path
# exp_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/DataCommon/"


###
## 1. Retrieve table directly from DataCommon

get_b25004 <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.b25004_hu_vacancy_status_acs_m&database=ds&years=",input_year,"&year_col=acs_year"))

get_b25024 <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.b25024_hu_units_in_structure_acs_m&database=ds&years=",input_year,"&year_col=acs_year"))


# 1.1 select columns
#select_data <- get_data %>% 
#  select(c(muni_id,municipal,geoid,logrecno,acs_year,vac_tot,vac_tot_m,))



## 2. merge tabular data on muni_id
calc <- get_b25004 %>%
  left_join(.,
            get_b25024 %>% select(c(muni_id,hu,hu_me)),
            by = c('muni_id' = 'muni_id')) %>% 
  arrange(muni_id) 


## 3.  run calculations (as needed)
calc <- calc %>%  #percent of vacant units
  mutate(vac_pct = (vac_tot/hu)*100) %>% 
  mutate(vac_pct_m = moe_prop(vac_tot, hu, vac_tot_m, hu_me)*100)

calc <- calc %>%  #percent of units for sale or for rent
  mutate(avail_pct = ((av_rent+av_sale)/hu)*100) %>% 
  mutate(avail_pct_m = round(moe_prop((av_rent+av_sale), hu, ((av_rent_m + av_sale_m)), hu_me)*100, 2))

# ? how to get moe_prop for sum of columns in previous line?

# 3 clean up for export

# 3.1 round the values joined from the input tables

clean_for_export <- calc %>% 
  mutate(
    vac_pct = round(vac_pct,2),
    vac_pct_m = round(vac_pct_m,2),
    avail_pct = round(avail_pct,2),
    avail_pct_m = round(avail_pct_m,2),
    av_rent_mp = round(av_rent_mp,2),
    av_rent_p = round(av_rent_p,2),
    av_sale_mp = round(av_sale_mp,2),
    av_sale_p = round(av_sale_p,2),
    migrwrk_mp = round(migrwrk_mp,2),
    migrwrk_p = round(migrwrk_p,2),
    oth_vac_mp = round(oth_vac_mp,2),
    oth_vac_p = round(oth_vac_p,2),
    r_noccu_mp = round(r_noccu_mp,2),
    r_noccu_p = round(r_noccu_p,2),
    s_noccu_mp = round(s_noccu_mp,2),
    s_noccu_p = round(s_noccu_p,2),
    ssnlrcr_mp = round(ssnlrcr_mp,2),
    ssnlrcr_p = round(ssnlrcr_p,2)
    )

## 3.2  replace NaN values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan)) #credit Hong Ooi on stack overflow

clean_for_export[is.nan(clean_for_export)] <- 0   #or whatever replacement value


## 3.3 EXPORT THE DATA to file for input year
write.csv(clean_for_export, paste0(exp_path, "redo_SHoP_b25004_b25024_vacancy_",input_year,".csv"))


##### PLOT EXAMPLE FOLLOWS

### 4. get spatial data for join (if needed)

# 4.1 AGOL version
# from services list of arcgis server: use query form to get all records, for example: Area_acres > 0, format geojson, then copy URL of resulting geojson object 
# works but lacks muni_id and towns are spelled all caps

#request <- "https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/Massachusetts_Municipalities_Hosted/FeatureServer/0/query?where=AREA_ACRES+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token="
#geo_raw <- st_read(request)
#st_crs(geo_raw)

# 4.2 read shp from K drive version
geo_shp <- read_sf("K:/DataServices/Datasets/Boundaries/Municipal/ma_municipalities.shp") 
st_crs(geo_shp)

# 5.  join b25004 table df to shp
shp_join <- geo_shp %>%
  left_join(.,
            clean_for_export %>% select(-c(seq_id,municipal)),
            by = c('muni_id' = 'muni_id')) %>% 
  arrange(muni_id) 


# 6. sample chloropleth map

# 6.1  plain black outline
# ggplot(shp_join) +
#   geom_sf(fill = "white", color = "black", linewidth = 0.3) +
#   theme_void()

# 6.2 with chloropleth on vac_pct column Vacancy Percentage

map_title = paste0(input_year," Vacancy Status")

vac_plot <- ggplot(shp_join) +
  geom_sf(aes(fill = vac_pct), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis_c(
    trans = "log", breaks = c(1, 5, 10, 20, 50, 100),
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
    subtitle = "percentage of total",
    caption = "Data: ACS | B25004, B25024"
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
    legend.position = c(0.7, 0.09)
  )

vac_plot

# 6.3 with chloropleth on avail_pct column Units For Rent or For Sale Percentage

map_title = paste0(input_year," % Units Available for Sale or Rent")

avail_plot <- ggplot(shp_join) +
  geom_sf(aes(fill = avail_pct), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis_c(
    trans = "log", breaks = c(1, 5, 10, 20, 50, 100),
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
    subtitle = "percentage of total",
    caption = "Data: ACS | B25004, B25024"
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
    legend.position = c(0.7, 0.09)
  )

avail_plot



