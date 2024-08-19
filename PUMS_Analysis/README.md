README.md

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


__5. output folder__: 

K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/PUMS