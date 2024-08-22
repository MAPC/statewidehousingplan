### PUMS Analysis for Statewide Housing

#### 2024


__1. analysis scripts__  filenames from 00_ to 08_ contain the topics being studied.

__2. data acquisition__ pums_import_var_list.R

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

see crosswalk complete table of 2021 variables: pums_vars_from_data_2005-09.ods

online data dictionary: https://usa.ipums.org/usa/resources/codebooks/DataDict0509.pdf

__columns:__

*over_vars:*  corresponding variables for "Overcrowding" analysis

*pers_vars:*  2009 variables in Person table

*hous_vars:*  2009 variables in Households table

*web_vars_list:*  2009 variables shown on Census PUMS download webpage 
https://data.census.gov/mdat/#/search?ds=ACSPUMS5Y2009

*Label:* 2009 variable description

*Number of Values:*  count of all possible items in controlled vocabulary

*Type:* Estimate or Recodes

