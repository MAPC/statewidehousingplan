library(tidyverse)
library(tidycensus)
library(magrittr)
library(data.table)
library(janitor)
library(zoo)
library(readxl)

###
#0.1 Set up
#Disengage scientific notation.
options(scipen=999)

#Set Census API Key.
census_api_key("d590f657a113f2f78b5a422462ea00745e79111c")

#MAPC/UrbanSim Subregion Crosswalk File.
subregion_xwalk <- read_excel("K:/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/subregions.xlsx",
                              sheet = 1)

###
#1.1 Load in reweighted PUMS files
#Remote directory
#root <- "S:/Network Shares/DS Projects/Current_Projects/Projections/Reweighter/Input_Files/"

#In-office Directory
root <-"K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/"

#Updated reweighter output reading + cleaning code (OPERATIONAL)
#-------------------------------------------------------------------------------
#ms <- mapcdatakeys::all_muni_data_keys %>% select(mpo) %>% unique()
#mpos <- sort(ms$mpo)
#years <- c(2010,2020,2030,2040,2050)

#rw <- data.frame()

#for (year in years[1:5]){
  
#  rw_tmp <- data.frame()
  
#  for (mid in mpos[1:13]){
    
#    tmp <- read.csv(
#       paste0(root, mid,"/weights_final_Projections2050_muni_",mid,"_",year,".csv")
#     ) %>% 
#     mutate(
#       RPA_flag = mid,
#       year = year
#     )
    
#    rw_tmp <- bind_rows(rw_tmp,tmp)
  
#  }
#  rw <- bind_rows(rw,rw_tmp) 
#}

#rw <- rw %>% 
#  select(
#    -c(
#      WFACTOR,
#      new_PWGTP
#    )
#  ) %>%
#  dplyr::rename(
#    WGTP = new_WGTP
#  ) %>% 
#  pivot_wider(
#    names_from = year,
#    values_from = c("WGTP")
#  ) %>% 
#  mutate(
#    SPORDER = as.character(SPORDER)
#  )
#-------------------------------------------------------------------------------

rw_2010_BRPC <- read.csv(paste0(
  root,
  "BRPC/",
  "weights_final_Projections2050_muni_BRPC_2010.csv")
) %>% mutate(RPA_flag = "BRPC")
rw_2010_CCC <- read.csv(paste0(
  root,
  "CCC/",
  "weights_final_Projections2050_muni_CCC_2010.csv")
)%>% mutate(RPA_flag = "CCC")
rw_2010_CMRPC <- read.csv(paste0(
  root,
  "CMRPC/",
  "weights_final_Projections2050_muni_CMRPC_2010.csv")
)%>% mutate(RPA_flag = "CMRPC")
rw_2010_FRCOG <- read.csv(paste0(
  root,
  "FRCOG/",
  "weights_final_Projections2050_muni_FRCOG_2010.csv")
)%>% mutate(RPA_flag = "FRCOG")
rw_2010_MAPC <- read.csv(paste0(
  root,
  "MAPC/",
  "weights_final_Projections2050_muni_MAPC_2010.csv")
)%>% mutate(RPA_flag = "MAPC")
rw_2010_MRPC <- read.csv(paste0(
  root,
  "MRPC/",
  "weights_final_Projections2050_muni_MRPC_2010.csv")
)%>% mutate(RPA_flag = "MRPC")
rw_2010_MVC <- read.csv(paste0(
  root,
  "MVC/",
  "weights_final_Projections2050_muni_MVC_2010.csv")
)%>% mutate(RPA_flag = "MVC")
rw_2010_MVPC <- read.csv(paste0(
  root,
  "MVPC/",
  "weights_final_Projections2050_muni_MVPC_2010.csv")
)%>% mutate(RPA_flag = "MVPC")
rw_2010_NMCOG <- read.csv(paste0(
  root,
  "NMCOG/",
  "weights_final_Projections2050_muni_NMCOG_2010.csv")
)%>% mutate(RPA_flag = "NMCOG")
rw_2010_NPEDC <- read.csv(paste0(
  root,
  "NPEDC/",
  "weights_final_Projections2050_muni_NPEDC_2010.csv")
)%>% mutate(RPA_flag = "NPEDC")
rw_2010_OCPC <- read.csv(paste0(
  root,
  "OCPC/",
  "weights_final_Projections2050_muni_OCPC_2010.csv")
)%>% mutate(RPA_flag = "OCPC")
rw_2010_PVPC <- read.csv(paste0(
  root,
  "PVPC/",
  "weights_final_Projections2050_muni_PVPC_2010.csv")
)%>% mutate(RPA_flag = "PVPC")
rw_2010_SRPEDD <- read.csv(paste0(
  root,
  "SRPEDD/",
  "weights_final_Projections2050_muni_SRPEDD_2010.csv")
)%>% mutate(RPA_flag = "SRPEDD")

rw_2010 <- rbind(rw_2010_BRPC, rw_2010_CCC, rw_2010_CMRPC, rw_2010_FRCOG, rw_2010_MAPC,
                 rw_2010_MRPC, rw_2010_MVC, rw_2010_MVPC, rw_2010_NMCOG, rw_2010_NPEDC,
                 rw_2010_OCPC, rw_2010_PVPC, rw_2010_SRPEDD)

rm(rw_2010_BRPC, rw_2010_CCC, rw_2010_CMRPC, rw_2010_FRCOG, rw_2010_MAPC,
   rw_2010_MRPC, rw_2010_MVC, rw_2010_MVPC, rw_2010_NMCOG, rw_2010_NPEDC,
   rw_2010_OCPC, rw_2010_PVPC, rw_2010_SRPEDD)

#rw_2020 <- read.csv(
  #"weights_final_Projections2050_2020_V1.csv"
#)
#
rw_2020_BRPC <- read.csv(paste0(
  root,
  "BRPC/",
  "weights_final_Projections2050_muni_BRPC_2020.csv")
) %>% mutate(RPA_flag = "BRPC")
rw_2020_CCC <- read.csv(paste0(
  root,
  "CCC/",
  "weights_final_Projections2050_muni_CCC_2020.csv")
)%>% mutate(RPA_flag = "CCC")
rw_2020_CMRPC <- read.csv(paste0(
  root,
  "CMRPC/",
  "weights_final_Projections2050_muni_CMRPC_2020.csv")
)%>% mutate(RPA_flag = "CMRPC")
rw_2020_FRCOG <- read.csv(paste0(
  root,
  "FRCOG/",
  "weights_final_Projections2050_muni_FRCOG_2020.csv")
)%>% mutate(RPA_flag = "FRCOG")
rw_2020_MAPC <- read.csv(paste0(
  root,
  "MAPC/",
  "weights_final_Projections2050_muni_MAPC_2020.csv")
)%>% mutate(RPA_flag = "MAPC")
rw_2020_MRPC <- read.csv(paste0(
  root,
  "MRPC/",
  "weights_final_Projections2050_muni_MRPC_2020.csv")
)%>% mutate(RPA_flag = "MRPC")
rw_2020_MVC <- read.csv(paste0(
  root,
  "MVC/",
  "weights_final_Projections2050_muni_MVC_2020.csv")
)%>% mutate(RPA_flag = "MVC")
rw_2020_MVPC <- read.csv(paste0(
  root,
  "MVPC/",
  "weights_final_Projections2050_muni_MVPC_2020.csv")
)%>% mutate(RPA_flag = "MVPC")
rw_2020_NMCOG <- read.csv(paste0(
  root,
  "NMCOG/",
  "weights_final_Projections2050_muni_NMCOG_2020.csv")
)%>% mutate(RPA_flag = "NMCOG")
rw_2020_NPEDC <- read.csv(paste0(
  root,
  "NPEDC/",
  "weights_final_Projections2050_muni_NPEDC_2020.csv")
)%>% mutate(RPA_flag = "NPEDC")
rw_2020_OCPC <- read.csv(paste0(
  root,
  "OCPC/",
  "weights_final_Projections2050_muni_OCPC_2020.csv")
)%>% mutate(RPA_flag = "OCPC")
rw_2020_PVPC <- read.csv(paste0(
  root,
  "PVPC/",
  "weights_final_Projections2050_muni_PVPC_2020.csv")
)%>% mutate(RPA_flag = "PVPC")
rw_2020_SRPEDD <- read.csv(paste0(
  root,
  "SRPEDD/",
  "weights_final_Projections2050_muni_SRPEDD_2020.csv")
)%>% mutate(RPA_flag = "SRPEDD")

rw_2020 <- rbind(rw_2020_BRPC, rw_2020_CCC, rw_2020_CMRPC, rw_2020_FRCOG, rw_2020_MAPC,
                 rw_2020_MRPC, rw_2020_MVC, rw_2020_MVPC, rw_2020_NMCOG, rw_2020_NPEDC,
                 rw_2020_OCPC, rw_2020_PVPC, rw_2020_SRPEDD)

rm(rw_2020_BRPC, rw_2020_CCC, rw_2020_CMRPC, rw_2020_FRCOG, rw_2020_MAPC,
   rw_2020_MRPC, rw_2020_MVC, rw_2020_MVPC, rw_2020_NMCOG, rw_2020_NPEDC,
   rw_2020_OCPC, rw_2020_PVPC, rw_2020_SRPEDD)
#
#rw_2030 <- read.csv(
  #"weights_final_Projections2050_2030_V1.csv"
#)

#
rw_2030_BRPC <- read.csv(paste0(
  root,
  "BRPC/",
  "weights_final_Projections2050_muni_BRPC_2030.csv")
) %>% mutate(RPA_flag = "BRPC")
rw_2030_CCC <- read.csv(paste0(
  root,
  "CCC/",
  "weights_final_Projections2050_muni_CCC_2030.csv")
)%>% mutate(RPA_flag = "CCC")
rw_2030_CMRPC <- read.csv(paste0(
  root,
  "CMRPC/",
  "weights_final_Projections2050_muni_CMRPC_2030.csv")
)%>% mutate(RPA_flag = "CMRPC")
rw_2030_FRCOG <- read.csv(paste0(
  root,
  "FRCOG/",
  "weights_final_Projections2050_muni_FRCOG_2030.csv")
)%>% mutate(RPA_flag = "FRCOG")
rw_2030_MAPC <- read.csv(paste0(
  root,
  "MAPC/",
  "weights_final_Projections2050_muni_MAPC_2030.csv")
)%>% mutate(RPA_flag = "MAPC")
rw_2030_MRPC <- read.csv(paste0(
  root,
  "MRPC/",
  "weights_final_Projections2050_muni_MRPC_2030.csv")
)%>% mutate(RPA_flag = "MRPC")
rw_2030_MVC <- read.csv(paste0(
  root,
  "MVC/",
  "weights_final_Projections2050_muni_MVC_2030.csv")
)%>% mutate(RPA_flag = "MVC")
rw_2030_MVPC <- read.csv(paste0(
  root,
  "MVPC/",
  "weights_final_Projections2050_muni_MVPC_2030.csv")
)%>% mutate(RPA_flag = "MVPC")
rw_2030_NMCOG <- read.csv(paste0(
  root,
  "NMCOG/",
  "weights_final_Projections2050_muni_NMCOG_2030.csv")
)%>% mutate(RPA_flag = "NMCOG")
rw_2030_NPEDC <- read.csv(paste0(
  root,
  "NPEDC/",
  "weights_final_Projections2050_muni_NPEDC_2030.csv")
)%>% mutate(RPA_flag = "NPEDC")
rw_2030_OCPC <- read.csv(paste0(
  root,
  "OCPC/",
  "weights_final_Projections2050_muni_OCPC_2030.csv")
)%>% mutate(RPA_flag = "OCPC")
rw_2030_PVPC <- read.csv(paste0(
  root,
  "PVPC/",
  "weights_final_Projections2050_muni_PVPC_2030.csv")
)%>% mutate(RPA_flag = "PVPC")
rw_2030_SRPEDD <- read.csv(paste0(
  root,
  "SRPEDD/",
  "weights_final_Projections2050_muni_SRPEDD_2030.csv")
)%>% mutate(RPA_flag = "SRPEDD")

rw_2030 <- rbind(rw_2030_BRPC, rw_2030_CCC, rw_2030_CMRPC, rw_2030_FRCOG, rw_2030_MAPC,
                 rw_2030_MRPC, rw_2030_MVC, rw_2030_MVPC, rw_2030_NMCOG, rw_2030_NPEDC,
                 rw_2030_OCPC, rw_2030_PVPC, rw_2030_SRPEDD)

rm(rw_2030_BRPC, rw_2030_CCC, rw_2030_CMRPC, rw_2030_FRCOG, rw_2030_MAPC,
   rw_2030_MRPC, rw_2030_MVC, rw_2030_MVPC, rw_2030_NMCOG, rw_2030_NPEDC,
   rw_2030_OCPC, rw_2030_PVPC, rw_2030_SRPEDD)
#

#rw_2040 <- read.csv(
 # "weights_final_Projections2050_2040_V1.csv"
#)

#
rw_2040_BRPC <- read.csv(paste0(
  root,
  "BRPC/",
  "weights_final_Projections2050_muni_BRPC_2040.csv")
) %>% mutate(RPA_flag = "BRPC")
rw_2040_CCC <- read.csv(paste0(
  root,
  "CCC/",
  "weights_final_Projections2050_muni_CCC_2040.csv")
)%>% mutate(RPA_flag = "CCC")
rw_2040_CMRPC <- read.csv(paste0(
  root,
  "CMRPC/",
  "weights_final_Projections2050_muni_CMRPC_2040.csv")
)%>% mutate(RPA_flag = "CMRPC")
rw_2040_FRCOG <- read.csv(paste0(
  root,
  "FRCOG/",
  "weights_final_Projections2050_muni_FRCOG_2040.csv")
)%>% mutate(RPA_flag = "FRCOG")
rw_2040_MAPC <- read.csv(paste0(
  root,
  "MAPC/",
  "weights_final_Projections2050_muni_MAPC_2040.csv")
)%>% mutate(RPA_flag = "MAPC")
rw_2040_MRPC <- read.csv(paste0(
  root,
  "MRPC/",
  "weights_final_Projections2050_muni_MRPC_2040.csv")
)%>% mutate(RPA_flag = "MRPC")
rw_2040_MVC <- read.csv(paste0(
  root,
  "MVC/",
  "weights_final_Projections2050_muni_MVC_2040.csv")
)%>% mutate(RPA_flag = "MVC")
rw_2040_MVPC <- read.csv(paste0(
  root,
  "MVPC/",
  "weights_final_Projections2050_muni_MVPC_2040.csv")
)%>% mutate(RPA_flag = "MVPC")
rw_2040_NMCOG <- read.csv(paste0(
  root,
  "NMCOG/",
  "weights_final_Projections2050_muni_NMCOG_2040.csv")
)%>% mutate(RPA_flag = "NMCOG")
rw_2040_NPEDC <- read.csv(paste0(
  root,
  "NPEDC/",
  "weights_final_Projections2050_muni_NPEDC_2040.csv")
)%>% mutate(RPA_flag = "NPEDC")
rw_2040_OCPC <- read.csv(paste0(
  root,
  "OCPC/",
  "weights_final_Projections2050_muni_OCPC_2040.csv")
)%>% mutate(RPA_flag = "OCPC")
rw_2040_PVPC <- read.csv(paste0(
  root,
  "PVPC/",
  "weights_final_Projections2050_muni_PVPC_2040.csv")
)%>% mutate(RPA_flag = "PVPC")
rw_2040_SRPEDD <- read.csv(paste0(
  root,
  "SRPEDD/",
  "weights_final_Projections2050_muni_SRPEDD_2040.csv")
)%>% mutate(RPA_flag = "SRPEDD")

rw_2040 <- rbind(rw_2040_BRPC, rw_2040_CCC, rw_2040_CMRPC, rw_2040_FRCOG, rw_2040_MAPC,
                 rw_2040_MRPC, rw_2040_MVC, rw_2040_MVPC, rw_2040_NMCOG, rw_2040_NPEDC,
                 rw_2040_OCPC, rw_2040_PVPC, rw_2040_SRPEDD)

rm(rw_2040_BRPC, rw_2040_CCC, rw_2040_CMRPC, rw_2040_FRCOG, rw_2040_MAPC,
   rw_2040_MRPC, rw_2040_MVC, rw_2040_MVPC, rw_2040_NMCOG, rw_2040_NPEDC,
   rw_2040_OCPC, rw_2040_PVPC, rw_2040_SRPEDD)
#

#rw_2050 <- read.csv(
  #"weights_final_Projections2050_2050_V1.csv"
#)

#
rw_2050_BRPC <- read.csv(paste0(
  root,
  "BRPC/",
  "weights_final_Projections2050_muni_BRPC_2050.csv")
) %>% mutate(RPA_flag = "BRPC")
rw_2050_CCC <- read.csv(paste0(
  root,
  "CCC/",
  "weights_final_Projections2050_muni_CCC_2050.csv")
)%>% mutate(RPA_flag = "CCC")
rw_2050_CMRPC <- read.csv(paste0(
  root,
  "CMRPC/",
  "weights_final_Projections2050_muni_CMRPC_2050.csv")
)%>% mutate(RPA_flag = "CMRPC")
rw_2050_FRCOG <- read.csv(paste0(
  root,
  "FRCOG/",
  "weights_final_Projections2050_muni_FRCOG_2050.csv")
)%>% mutate(RPA_flag = "FRCOG")
rw_2050_MAPC <- read.csv(paste0(
  root,
  "MAPC/",
  "weights_final_Projections2050_muni_MAPC_2050.csv")
)%>% mutate(RPA_flag = "MAPC")
rw_2050_MRPC <- read.csv(paste0(
  root,
  "MRPC/",
  "weights_final_Projections2050_muni_MRPC_2050.csv")
)%>% mutate(RPA_flag = "MRPC")
rw_2050_MVC <- read.csv(paste0(
  root,
  "MVC/",
  "weights_final_Projections2050_muni_MVC_2050.csv")
)%>% mutate(RPA_flag = "MVC")
rw_2050_MVPC <- read.csv(paste0(
  root,
  "MVPC/",
  "weights_final_Projections2050_muni_MVPC_2050.csv")
)%>% mutate(RPA_flag = "MVPC")
rw_2050_NMCOG <- read.csv(paste0(
  root,
  "NMCOG/",
  "weights_final_Projections2050_muni_NMCOG_2050.csv")
)%>% mutate(RPA_flag = "NMCOG")
rw_2050_NPEDC <- read.csv(paste0(
  root,
  "NPEDC/",
  "weights_final_Projections2050_muni_NPEDC_2050.csv")
)%>% mutate(RPA_flag = "NPEDC")
rw_2050_OCPC <- read.csv(paste0(
  root,
  "OCPC/",
  "weights_final_Projections2050_muni_OCPC_2050.csv")
)%>% mutate(RPA_flag = "OCPC")
rw_2050_PVPC <- read.csv(paste0(
  root,
  "PVPC/",
  "weights_final_Projections2050_muni_PVPC_2050.csv")
)%>% mutate(RPA_flag = "PVPC")
rw_2050_SRPEDD <- read.csv(paste0(
  root,
  "SRPEDD/",
  "weights_final_Projections2050_muni_SRPEDD_2050.csv")
)%>% mutate(RPA_flag = "SRPEDD")

rw_2050 <- rbind(rw_2050_BRPC, rw_2050_CCC, rw_2050_CMRPC, rw_2050_FRCOG, rw_2050_MAPC,
                 rw_2050_MRPC, rw_2050_MVC, rw_2050_MVPC, rw_2050_NMCOG, rw_2050_NPEDC,
                 rw_2050_OCPC, rw_2050_PVPC, rw_2050_SRPEDD)

rm(rw_2050_BRPC, rw_2050_CCC, rw_2050_CMRPC, rw_2050_FRCOG, rw_2050_MAPC,
   rw_2050_MRPC, rw_2050_MVC, rw_2050_MVPC, rw_2050_NMCOG, rw_2050_NPEDC,
   rw_2050_OCPC, rw_2050_PVPC, rw_2050_SRPEDD)
#
#1.2 Clean reweighted PUMS files

rw_2010 %<>%
  dplyr::rename(
    WFACTOR_10 = WFACTOR,
    PWGTP_10 = new_PWGTP,
    WGTP_10 = new_WGTP
  )

rw_2020 %<>%
  dplyr::rename(
   WFACTOR_20 = WFACTOR,
   PWGTP_20 = new_PWGTP,
   WGTP_20 = new_WGTP
  )

rw_2030 %<>%
  dplyr::rename(
    WFACTOR_30 = WFACTOR,
    PWGTP_30 = new_PWGTP,
    WGTP_30 = new_WGTP
  )

rw_2040 %<>%
  dplyr::rename(
    WFACTOR_40 = WFACTOR,
    PWGTP_40 = new_PWGTP,
    WGTP_40 = new_WGTP
  )

rw_2050 %<>%
  dplyr::rename(
    WFACTOR_50 = WFACTOR,
    PWGTP_50 = new_PWGTP,
    WGTP_50 = new_WGTP
  )

rw <- list(
  rw_2010,
  rw_2020,
  rw_2030,
  rw_2040,
  rw_2050
)

rw <- inner_join(rw_2010, rw_2020, by = c("SERIALNO", "SPORDER", "RPA_flag"))
rw <- inner_join(rw, rw_2030, by = c("SERIALNO", "SPORDER", "RPA_flag"))
rw <- inner_join(rw, rw_2040, by = c("SERIALNO", "SPORDER", "RPA_flag"))
rw <- inner_join(rw, rw_2050, by = c("SERIALNO", "SPORDER", "RPA_flag"))

rw %<>%
  mutate(SPORDER = as.character(SPORDER))
###
#2.1 Load 2019 PUMS data

pums_list <- 2019
variable_list <- c("PUMA", "TYPE", "SEX", "AGEP", "RAC1P", "HISP", "ESR", "WKHP",
                   "SCHL", "WAGP", "SEMP", "ADJINC", "SPORDER", "HINCP")


#2019
PUMS_data_5 <- map_dfr(pums_list, ~{
  get_pums(
    variables = variable_list,
    state = "MA",
    survey = "acs5",
    year = .x
  )
},  .id = "year")

PUMS_data_5 %<>%
  filter(TYPE == 1) %>% 
  mutate(SPORDER = as.character(SPORDER))

#PUMS_2019_full_reweighted <- left_join(PUMS_data_5, rw, by = c("SERIALNO", "SPORDER"))

#write.csv(PUMS_2019_full_reweighted,
          #"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS_2019_full_reweighted.csv")

###
#3.1 Cleaning PUMS data
#PUMS 2019
PUMS_data_5_d <- PUMS_data_5 %>%
  mutate(
    
    #PUMA Adjustment
    #Change variable's data type and pad 0s to the front of the string to match the PUMA crosswalk
    PUMA = as.character(PUMA),
    #PUMA = str_pad(PUMA, 5, side = "left", pad = "0"),
    
    # Individual Age Category
    # NOTE: Levels are (1) 0 to 4; (2) 5 to 9; (3) 10 to 14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 29; (7) 30 to 34; (8) 35 to 39; (9) 40 to 44 (10) 45 to 49 (11) 50 to 54 (12) 55 to 59 (13) 60 to 64 (14) 65 to 69 (15) 70 to 74 (16) 75 to 79 (17) 80-84 (18)85p
    AGEP = as.numeric(AGEP), #Change datatype of 
    PAGEC =
      cut(AGEP,
          breaks = c(-Inf, seq(4, 84, 5), Inf),
          labels = 1:18),
    
    # Consolidated Age Category
    # NOTE: Levels are (0) 0-14; (4) 15 to 19; (5) 20 to 24; (6) 25 to 34; (7) 35 to 44; (8) 45 to 54; (9) 55 to 64 (10) 65 to 74 (11) 75+
    PAGEC2 =
      cut(
        AGEP,
        breaks = c(-Inf, 14, 19, 24, 34, 44, 54, 64, 74, 79, 84, Inf),
        labels = c(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
      ),
    
    #UrbanSim Age Categories
    #NOTE: Levels are (0) 0-15, (1) 16-35, (2) 36-45, (3) 46-65, (4) 66-75, (5) 76+
    PAGEC3 =
      cut(
        AGEP,
        breaks = c(-Inf,15,35,45,65,75,Inf),
        labels = c("0","1","2","3","4","5")
      ),
    ageCAT4 = case_when(
      AGEP >= 15 & AGEP <= 24 ~ "1",
      AGEP >= 25 & AGEP <= 34 ~ "2",
      AGEP >= 35 & AGEP <= 44 ~ "3",
      AGEP >= 45 & AGEP <= 54 ~ "4",
      AGEP >= 55 & AGEP <= 59 ~ "5",
      AGEP >= 60 & AGEP <= 64 ~ "6",
      AGEP >= 65 & AGEP <= 74 ~ "7",
      AGEP >= 75 & AGEP <= 84 ~ "8",
      AGEP >= 85 ~ "9"
    ),
    
    # Individual Race Category (Analysis may need more specificity than this)
    # NOTE: Categories are (1) White, non hispanic/latino (2)Black, non hispanic/latino (3) hispanic/latino (4)Asian/ Pacific Islander non hispanic/latino (5) other, non hispanic/latino
    PRACE = case_when(
      RAC1P == 1 & HISP == 1 ~ 1,
      RAC1P == 2 & HISP == 1 ~ 2,
      (RAC1P == 6 | RAC1P == 7) & HISP == 1 ~ 4,
      HISP != 1 ~ 3,
      TRUE ~ 5
    ),
    
    # Age, Sex, Race Cat in order of Age Cat, Sex Cat, Race Cat
    #ASRC = paste(PAGEC, SEX, PRACE, sep = "_"),
    # Age, Sex Cat in order of Age Cat, Sex Cat
    #ASC = paste(PAGEC, SEX, sep = "_"),
    # Age, Race Cat in the order of Age Cat, Race Cat
    #ARC = paste(PAGEC, PRACE, sep = "_"),
    
    # Categorizing employment status
    empstat = case_when(
      ESR == "b" ~ "Under 16 not eligible for work",
      ESR == 1 ~ "Employed",
      ESR == 2 ~ "Employed but not at work",
      ESR == 3 ~ "Unemployed",
      ESR == 4 ~ "Armed forces",
      ESR == 5 ~ "Armed forces but not at work",
      ESR == 6 ~ "Not in labor force"
    ),
    
    # Categorizing Civilian Labor Force or not
    # NOTE: Categories are (0) under 16 or non-civilian, (1) in labor force, (2) not in labor force
    lf = case_when(
      ESR == "b" | ESR == 4 | ESR == 5  ~ 0,
      ESR == 1 | ESR == 2 | ESR == 3  ~ 1,
      ESR == 6 ~ 2,
      is.na(ESR) ~ 0
    ),
    
    # Counts as Worker
    # NOTE: Categories are (0) under 16 or not in labor force, or (1) in labor force or non-civilian worker
    worker = case_when(
      (ESR == "b" | ESR == 6) ~ 0,
      (ESR == 1 | ESR == 2 | ESR == 3 | ESR == 4 | ESR == 5) ~ 1,
      is.na(ESR) ~ 0
    ),
    
    # Categorizing full or part-time work
    wrkfull = case_when(
      WKHP < 35 ~ "Part time (less than 35 hrs)",
      WKHP > 34 ~ "Full time (35 hrs or more)",
      is.na(WKHP) ~ "Under 16, unemployed, or not in labor force"
    ),
    
    # Categorizing educational attainment
    # NOTE: Categories are (1) High school degree or less, (2) Associate's degree or some college, (3) Bachelor's, and (4) MA or higher
    SCHL = as.numeric(SCHL),
    eduattn = case_when(
      SCHL <= 17 ~ 1,
      SCHL %in% 18:20 ~ 2,
      SCHL == 21 ~ 3,
      SCHL >= 22 ~ 4,
      is.na(SCHL) ~ 0
    ),
    
    # Adjust wages to 2019 dollars and then to 2013 dollars 
    WAGPA = round((as.numeric(ADJINC) * as.numeric(WAGP))*0.911207377, 0),
    
    # Adjust total self employment income to 2019 dollars and then to 2013 dollars
    SWAGPA = round((as.numeric(ADJINC) * as.numeric(SEMP))*0.911207377, 0),
    
    # Combine wages and self employment
    WAGPALL = WAGPA + SWAGPA,
    
    WAGPALL =  case_when(
      WAGPALL < 0 ~ 0,
      WAGPALL >= 0 ~ WAGPALL
    ),
    ADJINC = as.numeric(ADJINC),
    #Adjust HINCP to 2013 Dollars
    HINCP = round((as.numeric(HINCP)*ADJINC)*(342.5/376.5), 0), #ADD INFLATION ADJUSTMENT
    # Break wages into wage groups
    WAGPALLC =
      cut(
        WAGPALL,
        breaks = c(-Inf, 0, 10000, 25000, 50000, 75000, 125000, Inf),
        labels = c(seq(0, 6))
      ),
    #UrbanSim Income breakdowns
    #NOTE: Categories are (1) 0-35,000, (2) 35001-75000, (3) 75001-125000, (4) 125001-225000, (5) 225001+
    WAGPALLC_US =
      cut(
        HINCP,
        breaks = c(-Inf, 35000, 75000, 125000, 225000, Inf),
        labels = c("1","2","3","4","5")
      ),
    WAGPALLC_US2 = case_when(
      HINCP < 35000 ~ "1",
      HINCP >= 35000 & HINCP < 75000 ~ "2",
      HINCP >= 75000 & HINCP < 125000 ~ "3",
      HINCP >= 125000 & HINCP < 225000 ~ "4",
      HINCP >= 225000 ~ "5"
    ),
    
    # Determine if person is head of household
    HousHder = case_when(SPORDER == 1 ~ 1,
                         SPORDER > 1 ~ 2),
    
    # Determine if case counts as person
    person = case_when(SPORDER != 0 ~ 1,
                       SPORDER == 0 ~ 0),
    
    # Determine if case counts as child
    child = case_when(AGEP < 18 ~ 1,
                      AGEP >= 18 ~ 0)
  ) %>%
  
  #Generating Household Variables
  dplyr::group_by(SERIALNO) %>%
  mutate(
    Worker_Total = sum(worker),
    Person_Total = sum(person),
    Child_Total = sum(child)
  ) %>% 
  ungroup() %>% 
  
  # Categorizing household sizes by number of persons (1) one person, (2) two persons, (3) three persons, (4) four or more persons
  mutate(  
    HHSize = case_when(
      Person_Total == 1 ~ 1,
      Person_Total == 2 ~ 2,
      Person_Total == 3 ~ 3,
      Person_Total >= 4 ~ 4
    ),
    # Categorizing worker categories (0) no workers, (1) one worker, (2) two workers, (3) three or more workers
    WRKHH = case_when(
      Worker_Total == 0 ~ 0,
      Worker_Total == 1 ~ 1,
      Worker_Total == 2 ~ 2,
      Worker_Total >= 3 ~ 3
    ),
    
    # Determine if person is not hhder vs hhder of 0- wrk, 1wrk, 2 wrk, 3+wrk
    HHder = case_when(
      SPORDER == 1 & WRKHH == 0 ~ 0,
      SPORDER == 1 & WRKHH == 1 ~ 1,
      SPORDER == 1 & WRKHH == 2 ~ 2,
      SPORDER == 1 & WRKHH == 3 ~ 3,
      SPORDER > 1 ~ 99
    ),
    # Determine what type of household the person is heading or not heading (1) head child, (2) child, (3) head not child, (4) not head no child, (5) head single
    HHtype = case_when(
      Child_Total >= 1 & Person_Total > 1 & HHder != 99 ~ 1,
      Child_Total >= 1 & Person_Total > 1 & HHder == 99 ~ 2,
      Child_Total == 0 & Person_Total > 1 & HHder != 99 ~ 3,
      Child_Total == 0 & Person_Total > 1 & HHder == 99 ~ 4,
      Person_Total == 1 & HHder != 99 ~ 5
    ),
    #Change the datatype of SPORDER to character.
    SPORDER = as.character(SPORDER),
    child_flag = case_when(
      Child_Total >= 1 ~ 1,
      Child_Total < 1 ~ 0
    )
  )  %>%
  ungroup() %>% 
  
  #Selecting for variables used in further analysis
  select(year, TYPE, SERIALNO, SPORDER, WGTP, PWGTP, PUMA, SEX, eduattn, PAGEC, PRACE, lf, WAGPALL, HINCP, WAGPALLC, HHtype, HHder, WRKHH, HHSize, AGEP, ageCAT4, WAGPALLC_US, child_flag)

#Testing for NAs in new variables
sapply(PUMS_data_5_d, function(x) sum(is.na(x)))

#3.2 Merges the cleaned PUMS data to the projection year weights
PUMS_data_5m <- left_join(PUMS_data_5_d, rw, by = c("SERIALNO", "SPORDER"))

#Check the total number of households do not deviate from targets based on
#errors induced by merging the reweighter outputs to the PUMS file.
total_hh_check <- PUMS_data_5m %>% 
  filter(
    SPORDER == 1
  ) %>% 
  group_by(
    RPA_flag
  ) %>% 
  summarise(
    hh_2010 = sum(WGTP_10),
    hh_2020 = sum(WGTP_20),
    hh_2030 = sum(WGTP_30),
    hh_2040 = sum(WGTP_40),
    hh_2050 = sum(WGTP_50)
  ) %>% 
  ungroup()

#Clean up the PUMS Data
PUMS_data_5m %<>%
  #Include observations in the household types were considering in the control
  #totals.
  filter(
    HHtype == 1 |
    HHtype == 3 |
    HHtype == 5
  ) %>% 
  mutate(
    #Someone <=18 who is a householder does not have a child who is not themselves
    #which is what is meant by the child flag. Changes the child flag to 0.
    child_flag = if_else(HHtype == 5 & child_flag == 1, 0, child_flag),
    #NAs induced for people under the age of 15. People under 15 are
    #assigned an ageCAT4 designation.
    ageCAT4 = replace_na(ageCAT4,"0")
  ) %>% 
  #Remove any records with NAs. After correcting the ageCAT4 NAs, there should be
  #no records removed.
  na.omit() %>%
  #rename the RPA flag.
  dplyr::rename(
    RPA = RPA_flag
  )

###
#4.1 Summarizing Projected Data into the decadal controls
#By the age of head of household (15-34,35-44,45-64,65-74,75+),
#Income group of household income (0-35K,35K-75K,75K-125K,125K-225K,>225K),
#Size of the household (1,2,3,4+),
#Presence of children (0,1)

#Household Controls 2010
Control_2010 <- PUMS_data_5m %>%
  group_by(
    RPA,
    ageCAT4,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = WGTP_10
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      ageCAT4 %in% c(1,2) ~ 15,
      ageCAT4 %in% c(3) ~ 35,
      ageCAT4 %in% c(4,5,6) ~ 45,
      ageCAT4 %in% c(7) ~ 65,
      ageCAT4 %in% c(8,9) ~ 75
    ),
    age_of_head_max = case_when(
      ageCAT4 %in% c(1,2) ~ 35,
      ageCAT4 %in% c(3) ~ 45,
      ageCAT4 %in% c(4,5,6) ~ 65,
      ageCAT4 %in% c(7) ~ 75,
      ageCAT4 %in% c(8,9) ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35000,
      WAGPALLC_US == 3 ~ 75000,
      WAGPALLC_US == 4 ~ 125000,
      WAGPALLC_US == 5 ~ 225000
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2010
  ) %>% 
  ungroup() %>% 
  select(
    -c(
      ageCAT4,
      WAGPALLC_US,
      child_flag,
      HHsize
    )
  ) %>% 
  group_by(
    RPA,
    year,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#Total Households Check
total_hh_check_C10 <- Control_2010 %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#Household Controls 2020  
Control_2020 <- PUMS_data_5m %>% 
  group_by(
    RPA,
    ageCAT4,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = WGTP_20
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      ageCAT4 %in% c(1,2) ~ 15,
      ageCAT4 %in% c(3) ~ 35,
      ageCAT4 %in% c(4,5,6) ~ 45,
      ageCAT4 %in% c(7) ~ 65,
      ageCAT4 %in% c(8,9) ~ 75
    ),
    age_of_head_max = case_when(
      ageCAT4 %in% c(1,2) ~ 35,
      ageCAT4 %in% c(3) ~ 45,
      ageCAT4 %in% c(4,5,6) ~ 65,
      ageCAT4 %in% c(7) ~ 75,
      ageCAT4 %in% c(8,9) ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35000,
      WAGPALLC_US == 3 ~ 75000,
      WAGPALLC_US == 4 ~ 125000,
      WAGPALLC_US == 5 ~ 225000
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2020
  ) %>% 
  ungroup() %>% 
  ungroup() %>% 
  select(
    -c(
      ageCAT4,
      WAGPALLC_US,
      child_flag,
      HHsize
    )
  ) %>% 
  group_by(
    RPA,
    year,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#2020 Total Household Check
total_hh_check_C20 <- Control_2020 %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#Household Controls 2030
Control_2030 <- PUMS_data_5m %>% 
  group_by(
    RPA,
    ageCAT4,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = WGTP_30
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      ageCAT4 %in% c(1,2) ~ 15,
      ageCAT4 %in% c(3) ~ 35,
      ageCAT4 %in% c(4,5,6) ~ 45,
      ageCAT4 %in% c(7) ~ 65,
      ageCAT4 %in% c(8,9) ~ 75
    ),
    age_of_head_max = case_when(
      ageCAT4 %in% c(1,2) ~ 35,
      ageCAT4 %in% c(3) ~ 45,
      ageCAT4 %in% c(4,5,6) ~ 65,
      ageCAT4 %in% c(7) ~ 75,
      ageCAT4 %in% c(8,9) ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35000,
      WAGPALLC_US == 3 ~ 75000,
      WAGPALLC_US == 4 ~ 125000,
      WAGPALLC_US == 5 ~ 225000
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2030
  ) %>%
  ungroup() %>% 
  select(
    -c(
      ageCAT4,
      WAGPALLC_US,
      child_flag,
      HHsize
    )
  ) %>% 
  group_by(
    RPA,
    year,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#2030 Total Household Check
total_hh_check_C30 <- Control_2030 %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#Household Controls 2040
Control_2040 <- PUMS_data_5m %>% 
  group_by(
    RPA,
    ageCAT4,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = WGTP_40
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      ageCAT4 %in% c(1,2) ~ 15,
      ageCAT4 %in% c(3) ~ 35,
      ageCAT4 %in% c(4,5,6) ~ 45,
      ageCAT4 %in% c(7) ~ 65,
      ageCAT4 %in% c(8,9) ~ 75
    ),
    age_of_head_max = case_when(
      ageCAT4 %in% c(1,2) ~ 35,
      ageCAT4 %in% c(3) ~ 45,
      ageCAT4 %in% c(4,5,6) ~ 65,
      ageCAT4 %in% c(7) ~ 75,
      ageCAT4 %in% c(8,9) ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35000,
      WAGPALLC_US == 3 ~ 75000,
      WAGPALLC_US == 4 ~ 125000,
      WAGPALLC_US == 5 ~ 225000
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2040
  ) %>% 
  ungroup() %>% 
  select(
    -c(
      ageCAT4,
      WAGPALLC_US,
      child_flag,
      HHsize
    )
  ) %>% 
  group_by(
    RPA,
    year,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#2040 Total Household Check
total_hh_check_C40 <- Control_2040 %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#Household Control 2050
Control_2050 <- PUMS_data_5m %>% 
  group_by(
    RPA,
    ageCAT4,
    WAGPALLC_US,
    child_flag
  ) %>%
  count(
    var = HHSize,
    wt = WGTP_50
  ) %>% 
  ungroup() %>% 
  dplyr::rename(
    HHsize = var
  ) %>%
  mutate(
    age_of_head_min = case_when(
      ageCAT4 %in% c(1,2) ~ 15,
      ageCAT4 %in% c(3) ~ 35,
      ageCAT4 %in% c(4,5,6) ~ 45,
      ageCAT4 %in% c(7) ~ 65,
      ageCAT4 %in% c(8,9) ~ 75
    ),
    age_of_head_max = case_when(
      ageCAT4 %in% c(1,2) ~ 35,
      ageCAT4 %in% c(3) ~ 45,
      ageCAT4 %in% c(4,5,6) ~ 65,
      ageCAT4 %in% c(7) ~ 75,
      ageCAT4 %in% c(8,9) ~ -1
    ),
    income_min = case_when(
      WAGPALLC_US == 1 ~ 0,
      WAGPALLC_US == 2 ~ 35000,
      WAGPALLC_US == 3 ~ 75000,
      WAGPALLC_US == 4 ~ 125000,
      WAGPALLC_US == 5 ~ 225000
    ),
    income_max = case_when(
      WAGPALLC_US == 1 ~ 35000,
      WAGPALLC_US == 2 ~ 75000,
      WAGPALLC_US == 3 ~ 125000,
      WAGPALLC_US == 4 ~ 225000,
      WAGPALLC_US == 5 ~ -1
    ),
    persons_min = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ 4
    ),
    persons_max = case_when(
      HHsize == 1 ~ 1,
      HHsize == 2 ~ 2,
      HHsize == 3 ~ 3,
      HHsize == 4 ~ -1
    ),
    children_min = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ 1
    ),
    children_max = case_when(
      child_flag == 0 ~ 0,
      child_flag == 1 ~ -1
    ),
    year = 2050
  ) %>%  
  ungroup() %>% 
  select(
    -c(
      ageCAT4,
      WAGPALLC_US,
      child_flag,
      HHsize
    )
  ) %>% 
  group_by(
    RPA,
    year,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()

#2050 Total Household Check
total_hh_check_C50 <- Control_2050 %>% 
  group_by(
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup()
#

#Rowbind all of the control files to create a complete file
#Remove NAs, which are coerced into the control totals by the ageCAT4 population under 15
Control_all <- rbind(Control_2010, Control_2020, Control_2030, Control_2040, Control_2050) %>% na.omit()

#Again, check that total households in each RPA reflects the number produced by the
#outputs of the reweighter.
total_hh_check_all <- Control_all %>% 
  group_by(
    year,
    RPA
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year,
    values_from = n
  )
#
#Removes control check files
rm(total_hh_check_C10,total_hh_check_C20,total_hh_check_C30,total_hh_check_C40,total_hh_check_C50)
#Removes control files for individual decades
rm(Control_2010,Control_2020,Control_2030,Control_2040,Control_2050)

###
#Takes the data structure of the control total file and expands it to include all
#inter-decadal years
Control_all_expand <- Control_all %>%
  select(
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max,
    year,
    RPA
  ) %>% 
  group_by(
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max,
    RPA
  ) %>% 
  mutate(
    year = as.double(year)
  ) %>% 
  tidyr::complete(year = tidyr::full_seq(2010:2050, 1)) %>% 
  ungroup()

#Joins the decadal data back to the expanded control total file.
Control_all_expand_m <- left_join(
  Control_all_expand,
  Control_all,
  by = c("age_of_head_min","age_of_head_max","income_min","income_max", "persons_min","persons_max","children_min","children_max","year","RPA"))

#Add in the "subregion code" variable which is a numerical representation of
#the RPAs. The numerical order of the RPAs in MAPC's nomenclature is different from
#that of UrbanSim's.
Control_all_expand_m <- left_join(Control_all_expand_m, subregion_xwalk, by = c("RPA"))

Control_all_expand_m %<>%
  ungroup() %>% 
  select(
    -RPA,
    -subregion_code_mapc
  ) %>% 
  dplyr::rename(
    subregion_code = subregion_code_urbansim
  )

#===============================================================================
###Next Steps
#1. Interpolation
#2. 2010-2020 Massbuilds alignment
#3. MAPCM Rebel towns adjustment
###
#Break the dataframe into 2010-2020 and 2021-2050 groups

Control_all_expand_m_2010_2020 <- Control_all_expand_m %>% 
  filter(
    year <= 2020
  )

Control_all_expand_m_2021_2050 <- Control_all_expand_m %>% 
  filter(
    year > 2020
  )

#Interpolate Baseline (read: unadjusted for MB and rebel towns) for 2010-2020 data

Full_Control_HHds_2010_2020_baseline <- Control_all_expand_m_2010_2020 %>% 
  group_by(
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max,
    subregion_code) %>%
  mutate(n = zoo::na.approx(n, na.rm = FALSE)) %>% 
  ungroup()

#Adjust the 2010-2020 statewide data for Massbuilds Projects

scaling_totals <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/massbuilds_unit_growth_factors.csv")

Full_Control_HHds_2010_2020_baseline %<>%
  mutate(
    mapc = case_when(
      subregion_code == 7 ~ 1,
      subregion_code != 7 ~ 0
    )
  )

setDT(Full_Control_HHds_2010_2020_baseline)
setDT(scaling_totals)

Full_Control_HHds_2010_2020_baseline[,grp:=.GRP,.(age_of_head_max,income_max,persons_min,persons_max,children_min,children_max)]
Full_Control_HHds_2010_2020_baseline[year==2010,h10:=sum(n),.(grp,subregion_code)]
Full_Control_HHds_2010_2020_baseline[,h10:=na.locf(h10),.(grp,subregion_code)]
Full_Control_HHds_2010_2020_baseline[year==2020,h20:=sum(n),.(grp,subregion_code)]
Full_Control_HHds_2010_2020_baseline[,h20:=na.locf(h20),.(grp,subregion_code)]
Full_Control_HHds_2010_2020_baseline[,diff:=h20-h10]
setkey(Full_Control_HHds_2010_2020_baseline,year,mapc)
Full_Control_HHds_2010_2020_baseline <- scaling_totals[Full_Control_HHds_2010_2020_baseline, on = .(year, mapc)]
Full_Control_HHds_2010_2020_baseline[,tot2:=shift(n),.(grp,subregion_code)]

Full_Control_HHds_2010_2020_baseline[year %in% 2011:2019,n:=round((diff*scaling_factor)+tot2,0),.(grp,subregion_code)]
Full_Control_HHds_2010_2020_baseline[,c("grp","h10","h20","diff","tot2","scaling_factor","mapc"):=NULL]

#Check the total number of households after adjusting for the
#Massbuilds scaling factors
total_hh_check_MB_adj <- Full_Control_HHds_2010_2020_baseline %>% 
  group_by(
    year,
    subregion_code
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  pivot_wider(
    names_from = year,
    values_from = n
  )

#Writes a temporary file
write.csv(
  Full_Control_HHds_2010_2020_baseline,
  "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/statewide_temp.csv",
  row.names = FALSE
)

#Read back in that temporary file
Full_Control_HHds_2010_2020_baseline <- read.csv("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/statewide_temp.csv")

#Add back 2021-2050 
Full_Control_HHds <- rbind(Full_Control_HHds_2010_2020_baseline, Control_all_expand_m_2021_2050)

##interpolate to fill in NAs for non-decadial years
Full_Control_HHds <- Full_Control_HHds %>% 
  group_by(
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max,
    subregion_code) %>%
  mutate(n = zoo::na.approx(n, na.rm = FALSE)) %>% 
  ungroup()

  
#Clean Some of the data up
Full_Control_HHds %<>%
  dplyr::rename(
    total_number_of_households = n
  )  %>% 
  mutate(
    total_number_of_households = round(total_number_of_households, 0)
  ) %>% 
  relocate(
    year,
    subregion_code,
    total_number_of_households,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  )

#Last check of the total households for each RPA

SWM_total_hh_check <- Full_Control_HHds %>% 
  filter(
    year %in% c(2010,2020,2030,2040,2050)
  ) %>% 
  group_by(
    year,
    subregion_code
  ) %>% 
  summarise(
    total_number_of_households = sum(total_number_of_households)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year,
    values_from = total_number_of_households
  )


#Remote
#write.csv(Full_Control_HHds,
          #"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Reweighter/Output Files/Statewide_Control_HHds_Projections2050_MB_v3.csv",
          #row.names = FALSE)

#In-office
write.csv(Full_Control_HHds,
          "K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Output_Files/Statewide_Control_HHds_Projections2050_MB_v03.23.23.csv",
          row.names = FALSE)

###
#NOW MAPC
#Bring in the Rebel Towns adjustment file
rebel_towns_adj <- read_excel("K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Input_Files/hhd_control_MAPC97to101_scaling_factors.xlsx")

#Filter just for the MAPC adjustment factors
rebel_towns_adj %<>%
  filter(
    subregion_code == 7
  ) %>% 
  mutate(
    scaling_factor = as.numeric(scaling_factor)
  )

#From the 2010-2020 fully interpolated, MB adjusted Household control file, filter
#for MAPC
MAPC_Full_Control_HHds_2010_2020_baseline <- Full_Control_HHds_2010_2020_baseline %>% 
  filter(
    subregion_code == 7
  )

#Do the same for 2021-2050
MAPC_Full_Control_HHds_2021_2050 <- Control_all_expand_m_2021_2050 %>% 
  filter(
    subregion_code == 7
  )

#Join the back
MAPC_Full_Control_HHds_2010_2050 <- rbind(MAPC_Full_Control_HHds_2010_2020_baseline, MAPC_Full_Control_HHds_2021_2050)

##interpolate to fill in NAs for non-decadial years
MAPC_Full_Control_HHds_2010_2050 <- MAPC_Full_Control_HHds_2010_2050 %>% 
  group_by(
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max,
    subregion_code) %>%
  mutate(n = zoo::na.approx(n, na.rm = FALSE))

#Lets do a check of total households
MAPC_total_hh_check <- MAPC_Full_Control_HHds_2010_2050 %>% 
  filter(
    year %in% c(2010,2020,2030,2040,2050)
  ) %>% 
  group_by(
    year,
    subregion_code
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year,
    values_from = n
  )

#Join the rebel towns adjustment factors to the MAPC projections
MAPC_Full_Control_HHds_2010_2050 <- left_join(MAPC_Full_Control_HHds_2010_2050, rebel_towns_adj, by = c("subregion_code", "year"))

#Calculate the adjusted number of households in each segment in the controls.
MAPC_Full_Control_HHds_2010_2050 <- MAPC_Full_Control_HHds_2010_2050 %>% 
  dplyr::mutate(
    #if the scaling factor is NA then set it to 1.
    scaling_factor = replace_na(scaling_factor, 1),
    #Multiply the number of households in each segment by the adjustment factor.
    adjusted_hhds = round(n*scaling_factor, 0)
  ) %>% 
  #Remove the unadjusted households and the scaling factor columns.
  select(
    -n,
    -scaling_factor
  ) %>% 
  #Rename the column representing the number of households in each segment.
  dplyr::rename(
    n = adjusted_hhds
  ) %>% 
  #Move the columns in the dataframe around.
  relocate(
    n,
    .before = age_of_head_min
  )

#Checking the total number of households
MAPC_total_hh_check <- MAPC_Full_Control_HHds_2010_2050 %>% 
  filter(
    year %in% c(2010,2020,2030,2040,2050)
  ) %>% 
  group_by(
    year,
    subregion_code
  ) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year,
    values_from = n
  )

#Clean up some of the data to fit the household control schema
#required by urbansim
MAPC_Full_Control_HHds_2010_2050 %<>%
  dplyr::rename(
    total_number_of_households = n
  ) %>% 
  relocate(
    year,
    subregion_code,
    total_number_of_households,
    age_of_head_min,
    age_of_head_max,
    income_min,
    income_max,
    persons_min,
    persons_max,
    children_min,
    children_max
  ) %>% 
  mutate(
    total_number_of_households = round(total_number_of_households, 0)
  )

#Remote
#write.csv(MAPC_Full_Control_HHds,
          #"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Reweighter/Output Files/MAPC_Control_HHds_Projections2050_MB_97to101_v1.csv",
          #row.names = FALSE)

#In-office
write.csv(MAPC_Full_Control_HHds_2010_2050,
"K:/DataServices/Projects/Current_Projects/Projections/Reweighter/Output_Files/MAPC_Control_HHds_Projections2050_MB_97to101_v03.23.23_check.csv",
row.names = FALSE)


#-------------------------------------------------------------------------------
### --- Checking the Outputs --- ###
#0.0 Set up

root <- "K:/DataServices/Projects/Current_Projects/Projections/"
#root <- ""

#Regional Controls
swm_reg_controls <- read.csv(paste0(root,"Reweighter/Output_Files/Statewide_Control_HHds_Projections2050_MB_v5.csv"))

mapcm_reg_controls <- read.csv(paste0(root,"Reweighter/Output_Files/MAPC_Control_HHds_Projections2050_MB_97to101_v5.csv"))

#Municipal Controls
swm_muni_controls <- read.csv(paste0(root,"Reweighter/Municipal_Controls_2010/Outputs2/SWM_municipal_control_totals_2010_v5.csv"))

mapcm_muni_controls <- read.csv(paste0(root,"Reweighter/Municipal_Controls_2010/Outputs2/MAPC101_municipal_control_totals_2010_v5.csv"))

#Regional Targeting Files

HH_Type_Age_Target_20 <- read.csv(paste0(root,"Reweighter/Input_Files/AgeHHder_HHtype_2020_V3.csv"))

Pop_Age_Target_20 <- read.csv(paste0(root,"Reweighter/Input_Files/Pop_Age_2020_V2.csv"))

#Crosswalks
#Subregion Definition Crosswalk

subregion_xwalk <- read_excel(paste0(root,"Data/Tabular/tProjections 2022 v2050/Inputs/subregions.xlsx"),
                              sheet = 1)

#1.0 Data Cleaning
#Regional Controls
swm_reg_controls <- left_join(
  swm_reg_controls,
  subregion_xwalk,
  by = c("subregion_code" = "subregion_code_urbansim")
  ) %>% 
  select(
    -c(
      subregion_code,
      subregion_code_mapc
    )
  )

mapcm_reg_controls <- left_join(
  mapcm_reg_controls,
  subregion_xwalk,
  by = c("subregion_code" = "subregion_code_urbansim")
) %>% 
  select(
    -c(
      subregion_code,
      subregion_code_mapc
    )
  )

#Municipal Controls
swm_muni_controls <- left_join(
  swm_muni_controls,
  subregion_xwalk,
  by = c("subregion_code" = "subregion_code_urbansim")
) %>% 
  select(
    -c(
      subregion_code,
      subregion_code_mapc,
      muni_id
    )
  )

mapcm_muni_controls <- left_join(
  mapcm_muni_controls,
  subregion_xwalk,
  by = c("subregion_code" = "subregion_code_urbansim")
) %>% 
  select(
    -c(
      subregion_code,
      subregion_code_mapc,
      muni_id
    )
  )

muni_controls <- bind_rows(swm_muni_controls,mapcm_muni_controls)
rm(swm_muni_controls,mapcm_muni_controls)

#Targets
#Households
#pre-aggregation sum
unag_hh_target <- sum(HH_Type_Age_Target_20$TARGET)

HH_Type_Age_Target_20 %<>%
  mutate(
    ageCAT3 = case_when(
      ageCAT3 %in% c("x == 1","x == 2","x == 3") ~ 0,
      ageCAT3 %in% c("x == 4","x == 5","x == 6","x == 7") ~ 1,
      ageCAT3 %in% c("x == 8","x == 9") ~ 2,
      ageCAT3 %in% c("x == 10","x == 11","x == 12","x == 13") ~ 3,
      ageCAT3 %in% c("x == 14","x == 15") ~ 4,
      ageCAT3 %in% c("x == 16","x == 17","x == 18") ~ 5,
    ),
    HHtype = case_when(
      HHtype == "x == 1" ~ 1,
      HHtype == "x == 3" ~ 3,
      HHtype == "x == 5" ~ 5
    )
  ) %>% 
  group_by(
    RPA,
    Year,
    ageCAT3,
    HHtype
  ) %>% 
  summarise(
    hh_target = sum(TARGET)
  ) %>% 
  select(
    -c(
      Year
    )
  )

#Check we didnt lose any information in the aggregation
print(sum(HH_Type_Age_Target_20$hh_target)/unag_hh_target)

#
#Population
#pre-aggregation sum
unag_pop_target = sum(Pop_Age_Target_20$TARGET)

Pop_Age_Target_20 %<>%
  mutate(
    ageCAT3 = case_when(
      ageCAT3 %in% c("x == 1","x == 2","x == 3") ~ 0,
      ageCAT3 %in% c("x == 4","x == 5","x == 6","x == 7") ~ 1,
      ageCAT3 %in% c("x == 8","x == 9") ~ 2,
      ageCAT3 %in% c("x == 10","x == 11","x == 12","x == 13") ~ 3,
      ageCAT3 %in% c("x == 14","x == 15") ~ 4,
      ageCAT3 %in% c("x == 16","x == 17","x == 18") ~ 5,
    )
  ) %>% 
  group_by(
    RPA,
    ageCAT3
  ) %>% 
  summarise(
    pop_target = sum(TARGET)
  )
#Check we didnt lose any information in the aggregation
print(sum(Pop_Age_Target_20$pop_target)/unag_pop_target)

#2.0 Analysis
#2.1 Regional Targets <-> Regional Controls
#2.1.1 Age of Householder

#2.1.2 Household Type

#2.2 Municipal Controls <-> Regional Controls 2010
#Household Age
HH_Age_Muni <- muni_controls %>% 
  group_by(
    RPA,
    age_of_head_min
  ) %>% 
  summarise(
    hh_muni_control = sum(total_number_of_households)
  )

HH_Age_Reg <- swm_reg_controls %>% 
  group_by(
    year,
    RPA,
    age_of_head_min
  ) %>% 
  summarise(
    hh_reg_control = sum(total_number_of_households)
  ) %>% 
  filter(
    year == 2011
  ) %>% 
  select(
    -year
  )

HH_Age_Reg_Muni <- left_join(HH_Age_Reg, HH_Age_Muni, by = c("RPA","age_of_head_min"))
HH_Age_Reg_Muni %<>%
  rowwise() %>% 
  mutate(
    diff = hh_reg_control - hh_muni_control
  )
print(sum(HH_Age_Reg_Muni$diff))
  
#HH Income
HH_Inc_Muni_Reg

HH_Children_Muni_Reg