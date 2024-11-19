#### functions ####

get_full_parcel_data<-function (muni_name){
  #Make sure the input is in all caps for the parcel link filter and Manchester by the Sea is "Manchester" for parcel links and pdb
  
  if (muni_name == toupper("MANCHESTER-BY-THE-SEA")){
    muni_load <- "MANCHESTER"
    pdb_lookup<- "Manchester-By-The-Sea"
    
  } else {
  muni_load<-toupper(muni_name)
  pdb_lookup<- str_to_title(muni_load)
  }
  print(paste(x, muni_load, pdb_lookup))
  
  if (muni_load == "BOSTON"){
    par_geom<- arc_read("https://gisportal.boston.gov/arcgis/rest/services/Assessing/ASG_PROPERTY_ASSESSMENT_PARCEL_JOIN_FY24/FeatureServer/0", 
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
      # open the thing
      par_sf<-arc.data2sf(arc.select(arc.open(paste0(real_folder, "/", tax_par))))
      
      #delete the real folder
      unlink(real_folder, recursive = TRUE)
      
      return(par_sf)
    }
    
    #read in the thing as arc.open
    par_geom<-read_gdb_url(URL = download_inputs$`File GDB Download URL`, 
                           gdb_file = download_inputs$gdb_file,
                           tax_par = download_inputs$tax_par)%>%
      select(LOC_ID, geom)
  }
  
  #read in the parcel database
  pdb<- read_csv(paste0("K:/DataServices/Datasets/Parcel_DB/Data/LPDB_Municipal_Data/current/LPDB_DRAFT_", pdb_lookup,
                        "_12.18.23.csv"))
  
  full_parcels<- full_join(par_geom, pdb, by = "LOC_ID")
  
  return(full_parcels)
}



