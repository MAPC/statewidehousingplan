
## Municipalities Geometry ##

ma_munis<- st_read("https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/Massachusetts_Municipalities_Hosted/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## MC-FRM ##
## ArcOnline MC-FRM layers that didn't work for some reason but should be identical to the ones loaded below saved in a geodatabase locally
psep_2030<- arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/0")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union()

psep_2050<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/1")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union()

psep_2070<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/2")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union()

# MC-FRM Percent Storm Exceedance Probability (0.1% annual) 2030, 2050, 2070
psep_2030_shp<- arc.data2sf(arc.select(arc.open(paste0(arc_pro_gdb, "/psep2030_exp"))))%>%
  st_transform(crs = st_crs(ma_munis))

psep_2050_shp<- arc.data2sf(arc.select(arc.open(paste0(arc_pro_gdb, "/psep2050_exp"))))%>%
  st_transform(crs = st_crs(ma_munis))

psep_2070_shp<- arc.data2sf(arc.select(arc.open(paste0(arc_pro_gdb, "/psep2070_exp"))))%>%
  st_transform(crs = st_crs(ma_munis))


### Munis in MC-FRM ##

muni_list <- st_intersection(st_make_valid(ma_munis), st_make_valid(psep_2070))%>%
  distinct(TOWN)

## parcels ##

# creates a list of tables with spatial parcel data with the MAPC parcel database
full_parcels<- NULL

for (x in 1:length(muni_list$TOWN)){
  
  full_parcels[[x]]<- get_full_parcel_data(muni_name = muni_list$TOWN[x])
  
  }

