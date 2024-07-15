library(dplyr)
library(tidycensus)
library(sf)
library(geojsonsf)
library(ggplot2)

# lberman 2024-06-18
# pull data from DataCommon, modify for specific SHoP use case 
# Housing Tenure by Year Built b25127_hu_tenure_year_built_units_acs_m
# Housing Units in a Structure b25024_hu_units_in_structure_acs_m

# DATA ERROR? see section 3.2

### 0. SETUP VARIABLES SECTION

# 0.1 REQUIRED:  INPUT ACS 5YR VALUE
input_year = "2018-22"

# 0.2 REQUIRED: SET PATH TO OUTPUT FOLDER 
# write table to exp_path
# exp_path = "H:/0_PROJECTS/2024_statewide_housing_plan/output/"

exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/DataCommon/"


###
## 1. Retrieve table directly from DataCommon

get_b25127 <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.b25127_hu_tenure_year_built_units_acs_m&database=ds&years=",input_year,"&year_col=acs_year"))

get_b25024 <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.b25024_hu_units_in_structure_acs_m&database=ds&years=",input_year,"&year_col=acs_year"))

# 1.1 select columns
#select_data <- get_data %>% 
#  select(c(muni_id,municipal,geoid,logrecno,acs_year,vac_tot,vac_tot_m,))



# 2. merge tabular data on muni_id (if more than one table is needed)
calc <- get_b25127 %>%
  left_join(.,
            get_b25024 %>% select(c(muni_id,hu,hu_me)),
            by = c('muni_id' = 'muni_id')) %>%
  arrange(muni_id)


## 3.  run calculations (as needed)
calc <- calc %>%  #count of units built
  rowwise() %>%
  mutate(from_2000 = sum(c(c(h10,h0009)))) %>% 
  mutate(from_2000m = moe_sum(moe = c(c(h10m,h0009m)), estimate = c(from_2000)))

calc <- calc %>%  #percent of units built
  mutate(f2000_pct = (from_2000/hu)*100) %>% 
  mutate(f2000_pctm = round(moe_prop(from_2000, hu, from_2000m, hu_me)*100, 2))



## 3.1  replace NaN values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan)) #credit Hong Ooi on stack overflow

calc[is.nan(calc)] <- 0   #or whatever replacement value


## 3.2  check for discrepancies in percent columns

#select renter occ hu built before 1939 as pct of hu (over 80 pct)
rent39 <- filter(calc, r39p > 80) %>% 
  select(c(municipal,r39p))

# result{Lawrence:116,
        # Chelsea:113,
        # Somerville:100,
        # Everett:93,
        # Fall River:86}
# two of them over 100% ?
rm(rent39)

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

# 5.  join b25127 table df to shp
shp_join <- geo_shp %>%
  left_join(.,
            calc %>% select(-c(seq_id,municipal)),
            by = c('muni_id' = 'muni_id')) %>% 
  arrange(muni_id) 


# 6. sample chloropleth map

# 6.1 with chloropleth on r39p column Units For Rent or For Sale Percentage

map_title = paste0(input_year," Rental units built before 1939")

# brandon plot example with round(breaks)
r39p_plot <- ggplot() +
  geom_sf(data = shp_join, linewidth = 0.001, aes(fill = r39p), color = "grey")+
  scale_fill_continuous(
    high = "orange", low = "white",
    breaks = c(
      round(max(shp_join$r39p)*(1/5),0),
      round(max(shp_join$r39p)*(2/5),0),
      round(max(shp_join$r39p)*(3/5),0),
      round(max(shp_join$r39p)*(4/5),0),
      round(max(shp_join$r39p),0)),
    name = "pct"
  )+
  geom_sf(data = shp_join, fill = NA, color = "black", linewidth = 0.5)+
  labs(
    title = map_title,
    subtitle = "percent of total units",
    caption = "Data: ACS | B25127, B25024"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  )


r39p_plot




## 7. EXPORT THE DATA to file for input year
write.csv(calc, paste0(exp_path, "SHoP_b25127_b25024_units_built_",input_year,".csv"))



