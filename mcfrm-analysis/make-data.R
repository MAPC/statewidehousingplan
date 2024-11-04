## Municipalities Geometry ##

ma_munis<- st_read("https://services1.arcgis.com/hGdibHYSPO59RG1h/ArcGIS/rest/services/Massachusetts_Municipalities_Hosted/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## MC-FRM ##

psep_2030<- arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/0")%>%
  st_transform(crs = st_crs(ma_munis))

psep_2050<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/1")%>%
  st_transform(crs = st_crs(ma_munis))

psep_2070<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Coastal_flooding_probability_1000yr_storms/FeatureServer/2")%>%
  st_transform(crs = st_crs(ma_munis))


### Munis in MC-FRM ##

muni_list <- st_intersection(st_make_valid(ma_munis), st_make_valid(psep_2070))%>%
  distinct(TOWN)


## parcels ##

parcel_links<- rio::import("https://www.mass.gov/doc/massgis-parcel-data-download-links-table/download")

full_parcels<- NULL

for (x in 1:length(muni_list$TOWN)){
  temp <- tempfile()
  temp2 <- tempfile()
  
  #ID the town to download
  muni_load<-muni_list$TOWN[x]
  print(muni_load)
  
  # Download the zip file and save to 'temp' 
  URL <- parcel_links%>%
    filter(`Town Name` == muni_load)
  
  #par_geom<-read_shape_URL(URL$`File GDB Download URL`)
  
  # download.file(URL$`Shapefile Download URL`, temp)
  # 
  # # Unzip the contents of the temp and save unzipped content in 'temp2'
  # unzip(zipfile = temp, exdir = temp2)
  # 
  # # Read the shapefile
  # par_geom <-st_read(temp2)
  # 
  pdb_lookup<- str_to_title(muni_load)

  print(paste(x, pdb_lookup))

  pdb<- read_csv(paste0("K:/DataServices/Datasets/Parcel_DB/Data/LPDB_Municipal_Data/current/LPDB_DRAFT_", pdb_lookup,
                        "_12.18.23.csv"))
  
  # full_parcels[[x]]<- full_join(par_geom, pdb, by = "LOC_ID")%>%
  #   distinct(LOC_ID, .keep_all = TRUE)
}

read_shape_URL <- function(URL){
  cur_tempfile <- tempfile()
  download.file(url = URL, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  arc.open(out_directory) #read_sf also works here
}


