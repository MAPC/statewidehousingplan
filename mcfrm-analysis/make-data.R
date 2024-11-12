
## Municipalities Geometry ##

ma_munis<- st_read("https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/Massachusetts_Municipalities_Hosted/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## MC-FRM ##

psep_2030<- arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/0")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union

psep_2050<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/1")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union

psep_2070<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/2")%>%
  st_transform(crs = st_crs(ma_munis))%>%
  st_make_valid()%>%
  st_union()


### Munis in MC-FRM ##

muni_list <- st_intersection(st_make_valid(ma_munis), st_make_valid(psep_2070))%>%
  distinct(TOWN)

## parcels ##

# # Break in case of function failure
# ma_par_arc<- arc_read("https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/L3_TAXPAR_POLY_ASSESS_gdb/FeatureServer/0")%>%
#    filter(CITY %in% muni_list$TOWN)

#full_parcels<- NULL


for (x in 80:length(muni_list$TOWN)){
  
  full_parcels[[x]]<- get_full_parcel_data(muni_name = muni_list$TOWN[x])
  
  }

#all_parcels<-rbindlist(full_parcels)

l#ength(muni_list$TOWN)
