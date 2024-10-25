### PUMS Analysis for Statewide Housing

#### 2024


__1. analysis scripts__  filenames from 00_ to 08_ contain the topics being studied.

__2. data acquisition__ pums_2021_import_var_list.R  (updated with new vars)

__3 source:__ PUMS API 

person table: via tidycensus

https://walker-data.com/tidycensus/reference/get_pums.html

household table: via ftp download 

https://www.census.gov/programs-surveys/acs/microdata/access.html


__4. unique identifiers__ 

each unique household has a serial number (SERIALNO)

each member of the household has a unique person ID (SPORDER)

unique_obs = SERIALNO + SPORDER 

note: for household table the SPORDER column was added with value = _0.

order by column [unique_obs] to see the household rows in order, with the hh preceding the persons in hh.


__5. output folder__: 

K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS


__6. 2005-09 PUMS__

6.1 see crosswalk complete table of PUMS 5yr 2009 variables: *pums_vars_from_data_2005-09.ods*

6.2 online data dictionary: https://usa.ipums.org/usa/resources/codebooks/DataDict0509.pdf

6.3 __columns:__

*over_vars:*  corresponding variables for "Overcrowding" analysis

*pers_vars:*  2009 variables in Person table

*hous_vars:*  2009 variables in Households table

*web_vars_list:*  2009 variables shown on Census PUMS download webpage 
https://data.census.gov/mdat/#/search?ds=ACSPUMS5Y2009

*Label:* 2009 variable description

*Number of Values:*  count of all possible items in controlled vocabulary

*Type:* Estimate or Recodes


### PUMS Analyses

## 0.8 Assigning Community Types to PUMAs

# Method for Assigning MAPC Community Type Designations to 2010 PUMAs
 
Each municipality has a community type assigned to it.
Each municipality has a total population.
Each municipality is a 2D polygon for which an area can be calculated.

PUMAs are U.S. Census geographies created for PUMS data to prevent re-identification of households or people.
Each PUMA is a 2D polygon for which an area can be calculated.

Each municipality intersects a PUMA geography. 

Assigning Community Type by Population.

For example, let it be that there is a PUMA of 100 people. Let it be that three (3) municipalities intersect said PUMA. Each municipality has a community type assigned to it. The population composition of the PUMA is as such: municipality 1 (M1), 50 people.; municipality 2 (M2), 30 people.; municipality 3 (M3) 20 people.. Group the total population by the community type of the intersecting municipalities. M1 is assigned "Rural", M2 is assigned "Rural", M3 is assigned "Developing Suburb." The PUMA is composed of two distinct community types: "Rural" and "Developing Suburb." The population of the community types which the PUMA is composed of are as such: "Rural", 80; "Developing Suburb", 20. The community type to which the PUMA belongs is the one which is assigned to the greatest combined municipal population. In this case, the PUMA will be assigned the "Rural" community types since 80 out of 100 people belong to municipalities which intersect the PUMA with the "Rural" community type designation. 

