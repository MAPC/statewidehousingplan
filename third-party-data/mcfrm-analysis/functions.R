#### functions MCRFM Analysis ####

get_full_parcel_data<-function (muni_name){
  
  
  #Make sure the input is in all caps for the parcel link filter 
  #and Manchester by the Sea is "Manchester" for parcel links and pdb
  if (toupper(muni_name) == "MANCHESTER-BY-THE-SEA"){
    muni_load <- "MANCHESTER"
    pdb_lookup<- "Manchester-By-The-Sea"
    
  } else {
  muni_load<-toupper(muni_name)
  pdb_lookup<- str_to_title(muni_load)
  }

  # muni_load is the look up for the massGIS table of gdbs (UPPER)
  # pdb_lookup is the look up variable for the MAPC parcel database in the K drive. (Title Case)
  #print(paste(muni_load, pdb_lookup))
  
  if (muni_load == "BOSTON"){
    par_geom<- arcgislayers::arc_read("https://gisportal.boston.gov/arcgis/rest/services/Assessing/ASG_PROPERTY_ASSESSMENT_PARCEL_JOIN_FY24/FeatureServer/0", 
                        n_max = getOption("arcgislayers.n_max", 200000))%>%
      rename(geom = geometry)%>%
      select(LOC_ID, geom)
      
    
  } else{
    #ID the town to download
    #Load in the MassGIS Excel Sheet of Links
    parcel_links<- rio::import("https://www.mass.gov/doc/massgis-parcel-data-download-links-table/download")%>%
      mutate(tax_par = paste0(str_sub(`File GDB Download URL`, -20, -17), "TaxPar"), #Name of the TaxPar feature class
             gdb_file = paste0(str_sub(`File GDB Download URL`, -20, -9)), #Name of the GDB folder name when unzipped
             shp_file = str_sub(`Shapefile Download URL`, 63, -9))
    
    #Filter the Excel Sheet of Links to the muni we are loading
    download_inputs <- parcel_links%>%
      filter(`Town Name` == muni_load)
    
    # Function to download, unzip, and read a gdb
    read_gdb_url <- function(URL, gdb_file, tax_par){
      
      #create a tempfile to dowloand
      dl_tempfile <- tempfile()
      #download to a temp file
      download.file(url = URL, destfile = dl_tempfile)
      # specificy a real folder to unzip to
      out_directory <- getwd()
      #unzip to a real folder
      unzip(dl_tempfile, exdir = out_directory)
      #check that the gdb is in the right place
      real_folder<- paste0(out_directory, "/", list.files(out_directory, pattern = gdb_file))
      print(real_folder)
      # open the spatial data as an sf or sfc (Boston...)
      par_sf<-arc.data2sf(arc.select(arc.open(paste0(real_folder, "/", tax_par))))
      
      #delete the real folder
      unlink(real_folder, recursive = TRUE)
      
      file.remove(list.files(pattern = "*.lyr"))
      
      return(par_sf)
    }
    
    #read in the thing as arc.open
    par_geom<-read_gdb_url(URL = download_inputs$`File GDB Download URL`, 
                           gdb_file = download_inputs$gdb_file,
                           tax_par = download_inputs$tax_par)%>%
      select(LOC_ID, geom)
  }
  
  loc_count<-length(unique(par_geom$LOC_ID))
  
  #print("Parcels:", par_geom$LOC_ID, "Rows:", nrow(par_geom))
  
  #read in the parcel database
  pdb<- read_csv(paste0("K:/DataServices/Datasets/Parcel_DB/Data/LPDB_Municipal_Data/current/LPDB_DRAFT_", pdb_lookup,
                        "_12.18.23.csv"))
  
  #print("Parcels:",as.numeric(length(unique(pdb$LOC_ID))), "Rows:", nrow(pdb))
  
  full_parcels<- full_join(par_geom, pdb, by = "LOC_ID")
  
  #print("Parcels:",as.numeric(length(unique(full_parcels$LOC_ID))), "Rows:", nrow(pdb))
  
  return(full_parcels)
}

get_inundated_units<-function(parcels, ext_2030, ext_2050, ext_2070, sp_out = FALSE){
  
  #create final output object
  spatial_output<-NULL
  #prepare to count residential parcels
  res_parcels<- parcels%>%filter(str_starts(Max_LUC_Assign, "1") | USE_CODE_SYMB == "Condominium")
  
  
  tot_res_units<-sum(res_parcels$imputed_units)
  total_res_parcels<-length(unique(res_parcels$LOC_ID))
  tot_res_value<- sum(res_parcels$TOTAL_VAL)
  
  print(paste("Units", tot_res_units, "Value", tot_res_value))
  
  #make sure all parcels have the right crs  
  res_parcels_int <-st_make_valid(res_parcels)%>%
    st_transform(crs = st_crs(ma_munis))
  
  
  ##filter parcels to the MC-FRM 2030 
  units_in_mcfrm30<- res_parcels_int%>%
    st_filter(st_make_valid(ext_2030))
  
  #summarize by use code (so later we can make sure only residential units are counted)
  mcfrm30_sum<-units_in_mcfrm30 %>%
    st_drop_geometry()%>%
    group_by(CITY, USE_CODE_SYMB)%>%
    summarize(units_2030 = sum(imputed_units),
              value_2030 = sum(TOTAL_VAL),
              parcels_2030 = length(unique(LOC_ID)))%>%
    mutate(pct_units_2030 = units_2030/tot_res_units,
           pct_value_2030 = value_2030/tot_res_value,
           pct_parcels_2030 = parcels_2030/total_res_parcels)
  
  
  
  #Repeat for 2050 scenario
  units_in_mcfrm50<- res_parcels_int%>%
    st_filter(st_make_valid(ext_2050))
  
  mcfrm50_sum<-units_in_mcfrm50 %>%
    st_drop_geometry()%>%
    group_by(CITY, USE_CODE_SYMB)%>%
    summarize(units_2050 = sum(imputed_units),
              value_2050 = sum(TOTAL_VAL),
              parcels_2050 = length(unique(LOC_ID)))%>%
    mutate(pct_units_2050 = units_2050/tot_res_units,
           pct_value_2050 = value_2050/tot_res_value,
           pct_parcels_2050 = parcels_2050/total_res_parcels)
  
  #Repeat for 2070 scenario
  units_in_mcfrm70 <-res_parcels_int%>%
    st_filter(st_make_valid(ext_2070))
  
  mcfrm70_sum<-units_in_mcfrm70 %>%
    st_drop_geometry()%>%
    group_by(CITY, USE_CODE_SYMB)%>%
    summarize(units_2070 = sum(imputed_units),
              value_2070 = sum(TOTAL_VAL),
              parcels_2070 = length(unique(LOC_ID)))%>%
    mutate(pct_units_2070 = units_2070/tot_res_units,
           pct_value_2070 = value_2070/tot_res_value,
           pct_parcels_2070 = parcels_2070/total_res_parcels)
  
  #joins the summarized tables to one table
  output<- full_join(mcfrm30_sum, mcfrm50_sum, by = c("CITY", "USE_CODE_SYMB"))%>%
    full_join(., mcfrm70_sum, by = c("CITY", "USE_CODE_SYMB"))
  
  if (sp_out == TRUE){
  spatial_output[[1]] <-units_in_mcfrm30
  spatial_output[[2]] <-units_in_mcfrm50
  spatial_output[[3]] <-units_in_mcfrm70
  spatial_output[[4]] <-output
  
  
  return(spatial_output)
  
  }
  else {
    return (output)
  }
  }
  
  

