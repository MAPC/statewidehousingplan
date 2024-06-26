#
# Code to read in the PUMS and create the two tables.
# Steps:
#   1. Create the functions needed: run the functions.R code.
#   2. Read in the PUMS files and create an extract that
#      appends the household information to each person.
#   3. Create the indexes for the tables
#   4. Output the extract
#

library(car)  # for recode
library(data.table)
library(AGHmatrix)
library(tidyverse)
options(scipen=999)

#setwd('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/reweighter_2020_umdi/')

# Read in PUMS files.
#Remote
#hfull<-read.csv("S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/pums_household_file_MA_2019_5yr.csv",na.strings = c("","N.A."))
#In-office
hfull<- read.csv("//data-001/public/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/pums_household_file_MA_2019_5yr.csv",na.strings = c("","N.A."))

#Remote
#Pfull<-read.csv("S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/pums_person_file_MA_2019_5yr.csv",na.strings = c("","N.A.")) %>%
  #rename_all(toupper)

#In-office
Pfull<-read.csv("//data-001/public/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/pums_person_file_MA_2019_5yr.csv",na.strings = c("","N.A.")) %>%
  rename_all(toupper)


#Remote
#pumas10 <- read.csv(
  #"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/PUMA_2010Geog.csv"
#)
#In-office
pumas10 <- read.csv(
  "//data-001/public/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/PUMA_2010Geog.csv"
)

#
# keep only housing units (TYPE==1), and non-vacant units (NP>0)
#
h<-hfull[hfull$TYPE==1 & hfull$NP>0,]
p<-Pfull
#
# keep only the columns needed
#
h_vars <- c("SERIALNO", "PUMA","WGTP","NP","TYPE","BDSP",
            "BLD","TEN","HINCP","HUPAC","ADJHSG","ADJINC",
            "FES","HHT","PARTNER")

p_vars <- c("SERIALNO","SPORDER","PWGTP","SEX","AGEP","MAR","RELSHIPP",
            "SCHL","WAGP","ESR","HISP","INDP","OCCP",
            "PERNP","PINCP","RAC1P","COW")
#
#H<-h[,(h_vars)]
H<-h %>% select(h_vars)
#P<-p[,(p_vars)]
P<-p %>% select(p_vars)
#

DF<-merge(H,P)
setDT(DF)
setorder(DF,SERIALNO,SPORDER)
DF[is.na(DF)] <- 0

#
# Calculate age, education, household size, household type, and ethnicity categories
#
DF$AgeCat<-sapply(floor(DF$AGEP/5)+1,min,18)
# DF$EduCat<-sapply(DF$SCHL,fEdcat)
DF$HHSize<-sapply(DF$NP,min,6)
DF$RaceHisp<-DF$RAC1P
DF$RaceHisp[DF$HISP>1] <- 10

#Categorizing educational attainment
DF[SCHL<=17, edu:= 1] #high school degree or less
DF[SCHL==18, edu:= 2] #associate's degree or some college
DF[SCHL==19, edu:= 2] #associate's degree or some college
DF[SCHL==20, edu:= 2] #associate's degree or some college
DF[SCHL==21, edu:= 3] #Bachelor's
DF[SCHL>=22, edu:= 4] #MA or higher
DF[is.na(SCHL), edu:= 1] #high school degree or less


#
#  Calcuate year. Needed for occupation codes, which depend on the year.
#
DF$Year <- floor(DF$SERIALNO/1000000000)
#
# Single occupation code
#
DF$Occupation <- 0
DF$Occupation[DF$Year==2008 | DF$Year==2009] <- DF$OCCP[DF$Year==2008 | DF$Year==2009]
DF$Occupation[DF$Year==2010 | DF$Year==2011] <- DF$OCCP[DF$Year==2010 | DF$Year==2011]
DF$Occupation[DF$Year==2012] <- DF$OCCP[DF$Year==2012]
#
# Function to make recodes character string argument for the recode function.
#
recodes<-function(x) {
  # x is a list of code cutoffs.
  # The object of x (the variable the recodes will be applied to)
  # must be an integer vector.
  # The first element in x is the bottom of the first category, could be "lo";
  # the second element in x is the bottom of the second category, etc.
  # The last element in x is the top of the last category, could be "hi".
  # All the elements of x should be integers except possibly
  # the first and last which could be "lo" and "hi" respectively.
  # Categories are numbered 1,2,3, etc.
  # 
  #returns a single character string to used as the
  #recodes argument to the recode function (package car).
  
  a<-as.character(x[[1]])
  
  k<-0
  for(i in 2:(length(x)-1)) {
    k<-k+1
    a<-paste(a,":",as.character(x[[i]]-1),"=",as.character(k),";",as.character(x[[i]]),sep="")
  }
  k<-k+1
  a<-paste(a,":",as.character(x[[length(x)]]),"=",as.character(k),sep="")
  a
}
#
#  Create occupation categories
#
occ_cuts <- list(10,500,800,1000,1300,1600,2000,2100,2200,2600,3000,3600,3700,4000,4200,4300,4700,5000,6000,6200,6800,7000,7700,9000,9800,9919)
DF$OccCat<-car::recode(DF$Occupation,recodes(occ_cuts))
#
#  The recode function won't recode the values not included in this list.
#  For occupation (and industry), these include the codes 0 and 9920, so
#  change 9920 to zero.
#
DF$OccCat[DF$OccCat==9920] <- 0
#
#  Create industry categories
#
ind_cuts <- list(170,370,570,770,1070,4070,4670,6070,6470,6870,7270,7860,7970,8370,8560,8770,9370,9670,9919)
ind_recodes <- recodes(ind_cuts)
DF$IndCat<-car::recode(DF$INDP,ind_recodes)
DF$IndCat[DF$IndCat==9920] <- 0
#
#Adjust HH Income into constant dollars 
DF$HHINC <- round(as.numeric(DF$ADJINC) * as.numeric(DF$HINCP)/1000000,0)
#
#Break household income into wage groups
DF$HHINCC <- cut(DF$HHINC, breaks=c(-Inf, 34999, 74999, 124999, 224999, Inf), labels=c("1","2","3","4", "5"))
#
# Adjust person income to a common year.
DF$PINCP<-DF$PINCP*(DF$ADJINC/1000000)
#
# # Multiply the adjusted person income to match inflation to 2017.
# DF$PINCP<-DF$PINCP*1.0669966
#
# Break the person income into wage groups.
DF$PINCPC<- cut(DF$PINCP, breaks=c(-Inf, 10000, 14999, 19999, 24999, 29999, 34999, 39999, 44999, 49999, 59999, 
                                   74999, 99999, 124999, 149999, 199999, Inf), 
                labels=c("1","2","3","4", "5","6","7","8",
                         "9","10","11","12","13","14","15","16"))
#
#
# Code for Household type (1 = single, 2 = adults w/ no children, 3 = adults with children)
# Children defined as ages 0-14 (AgeCat 1,2,3)
#determine if person is hhder
DF$HousHder[DF$SPORDER==1] = 1 
DF$HousHder[DF$SPORDER>1] = 2 

#counts as child
DF$child[DF$AGEP<18] = 1
DF$child[DF$AGEP>=18] = 0
#Aggregate children in HHds
PHchld13 <- DF[,c("SERIALNO", "child")]
#PHwrk12$WRKTOT <- ddply(PHwrk12, .(SERIALNO), .fun=summarize, .WRKTOT=sum(worker))
temp <- aggregate(DF$child, by=list(DF$SERIALNO), FUN=sum)
colnames(temp) <- c("SERIALNO", "CHILDTOT")
#regoin hh attribute (CHILDTOT) to person dataset
DF <- plyr::join(temp, DF, by = c("SERIALNO"), type = "left", match = "all")
# rm(PHchld13, temp)

#Create person categories-- 1 person, 2 person, 3 person, 4 plus person
DF$HHSize[DF$NP==1] = 1
DF$HHSize[DF$NP==2] = 2 
DF$HHSize[DF$NP==3] = 3 
DF$HHSize[DF$NP>=4] = 4 


#determine what type of household the person is heading or not heading (child, no child, or single)
DF$HHtype[DF$CHILDTOT>=1 & DF$HHSize>1 & DF$HousHder==1]=1 #headchild
DF$HHtype[DF$CHILDTOT>=1 & DF$HHSize>1 & DF$HousHder!=1]=2 #notheadchild
DF$HHtype[DF$CHILDTOT==0 & DF$HHSize>1 & DF$HousHder==1] =3 #headnochild
DF$HHtype[DF$CHILDTOT==0 & DF$HHSize>1 & DF$HousHder!=1] =4 #notheadnochild
DF$HHtype[DF$HHSize==1 & DF$HousHder==1]=5 #headsingle



# Recode labor force category
setDT(DF)
DF[ESR %in% 1:5,lf:=1]
DF[ESR==6,lf:=2]
DF[ESR=="b",lf:=2]

#
#  Subset and write out the extract file
#

pop19 <- DF[,.(SERIALNO,PUMA,WGTP,HHtype,SEX,edu,lf,SPORDER,PWGTP,AgeCat,NP)]
setnames(pop19,'AgeCat','ageCAT3')
setnames(pop19,'NP','PERSONTOT')
pop19[,WfACTOR:=1]

pop19 <- pop19 %>% 
  mutate(
    PUMA = as.character(PUMA),
    PUMA = str_pad(PUMA, 5, side = "left", pad = "0")
  )

#Cleans the PUMA file
#Creates a list of all RPAs
RPA <- c("MAPC", "OCPC", "SRPEDD", "MVC", "NPEDC", "NMCOG", "MVPC", "MRPC", 
         "CMRPC", "FRCOG", "PVPC", "BRPC", "CCC")

pumas10 <- pumas10 %>%
  select(name_t, all_of(RPA)) %>% #Selects names of relevant variables
  rename(PUMA = name_t) %>% #Renames variable representing distinct PUMAs
  mutate(PUMA = as.character(PUMA), #Changes the datatype to character
         PUMA = str_pad(PUMA, 5, side = "left", pad = "0")) %>% #Adds leading 0s to match strings with xwalk data
  pivot_longer(cols = all_of(RPA), names_to = "RPA", values_to = "in_RPA") %>%
  filter(in_RPA == 1) %>%
  select(-in_RPA) #Assigns a RPA to each PUMA

#join PUMS file with PUMA-RPA assignments

pop19_v1 <- left_join(pop19, pumas10, by = c("PUMA")) 

#Create PUMS configuration files for each RPA

pop19_BRPC <- pop19_v1 %>% 
  filter(
    RPA == "BRPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

write.csv(pop19_BRPC,
          "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_BRPC.csv",
          row.names = T)

pop19_CCC<- pop19_v1 %>% 
  filter(
    RPA == "CCC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_CCC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_CCC.csv",
       row.names = T)

pop19_CMRPC<- pop19_v1 %>% 
  filter(
    RPA == "CMRPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_CMRPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_CMRPC.csv",
       row.names = T)

pop19_FRCOG<- pop19_v1 %>% 
  filter(
    RPA == "FRCOG"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_FRCOG,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_FRCOG.csv",
       row.names = T)

pop19_MAPC<- pop19_v1 %>% 
  filter(
    RPA == "MAPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_MAPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_MAPC.csv",
       row.names = T)

pop19_MRPC<- pop19_v1 %>% 
  filter(
    RPA == "MRPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_MRPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_MRPC.csv",
       row.names = T)

pop19_MVC <- pop19_v1 %>% 
  filter(
    RPA == "MVC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_MVC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_MVC.csv",
       row.names = T)

pop19_MVPC<- pop19_v1 %>% 
  filter(
    RPA == "MVPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_MVPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_MVPC.csv",
       row.names = T)

pop19_NMCOG <- pop19_v1 %>% 
  filter(
    RPA == "NMCOG"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_NMCOG,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_NMCOG.csv",
       row.names = T)

pop19_NPEDC <- pop19_v1 %>% 
  filter(
    RPA == "NPEDC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_NPEDC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_NPEDC.csv",
       row.names = T)

pop19_OCPC<- pop19_v1 %>% 
  filter(
    RPA == "OCPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_OCPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_OCPC.csv",
       row.names = T)

pop19_PVPC<- pop19_v1 %>% 
  filter(
    RPA == "PVPC"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_PVPC,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_PVPC.csv",
       row.names = T)

pop19_SRPEDD<- pop19_v1 %>% 
  filter(
    RPA == "SRPEDD"
  ) %>% 
  select(
    -RPA,
    -PUMA
  )

fwrite(pop19_SRPEDD,
       "//data-001/public/DataServices/Projects/Current_Projects/Projections/Reweighter/Input Files/PUMS2019_SRPEDD.csv",
       row.names = T)


fwrite(pop19,"S:/Network Shares/NEW K Drive/DataServices/Projects/Current_Projects/Projections/Data/Tabular/tProjections 2022 v2050/Inputs/PUMS2019.csv",row.names = T)

# p3 <- unique(pop13,by=c('HHtype','edu','lf','ageCAT3','PWGTP','WGTP','PERSONTOT','SEX','SPORDER'))
# fwrite(p3,'PUMS2013_alt.csv',row.names=T)
#
#  Cleanup global environment
#
rm(occ_cuts)
rm(ind_cuts)
rm(ind_recodes)

#
#  END
# 
pop19 <- fread('S:/Network Shares/DS Projects/Current_Projects/LandUseAllocationModel/Data/Analysis/UrbanSim_Validation/UMDI_2020/reweighter_2020_umdi/PUMS2019.csv')
# #  Create targets
# #
# Age_by_HHT <- Hmatrix(DF,"AgeCat","HHType")
# NP <- Hvector(DF,"HHSize")
# HUPAC <- Hvector(DF,"HUPAC")
# Age_by_Sex <- Pmatrix(DF,"AgeCat","SEX")
# RaceHisp_by_Sex <- Pmatrix(DF,"RaceHisp","SEX")
# Educ_by_Sex <- Pmatrix(DF,"EduCat","SEX")
# Occ_by_Ind <- Pmatrix(DF,"OccCat","IndCat")
# #
# #  Output targets (without column or row names)
# #
# write.table(Age_by_HHT,file="Targets_Age_by_HHT.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(NP,file="Targets_NP.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(HUPAC,file="Targets_HUPAC.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(Age_by_Sex,file="Targets_Age_by_Sex.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(RaceHisp_by_Sex,file="Targets_RaceHisp_by_Sex.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(Educ_by_Sex,file="Targets_Educ_by_Sex.csv",sep=",",row.names = FALSE,col.names = FALSE)
# write.table(Occ_by_Ind,file="Targets_Occ_by_Ind.csv",sep=",",row.names = FALSE,col.names = FALSE)