### DataCommon tabular data


For selection of specific columns in DataCommon tables.

The R scripts will be named according to the ACS Table source.

This may include some non-DataCommon pre-processing, as described in the 

__Airtable tracking sheet__

https://airtable.com/appoiQOBDEYb6FoLF/tbl0874BAhS8q9tGx/viwEbLAGLCLTYB65z?blocks=hide


__Output folder for tabular data__

K:\DataServices\Projects\Current_Projects\Housing\StatewideHousingPlan\04_Analysis\Data\Working\DataCommon



__Template for query of single year csv from DataCommon__

the default sample R script for use with a new table is:  _SHoP_B25106.R_

set parameters for requested ACS year to import them directly to an R dataframe:

_0.1 REQUIRED:  INPUT ACS 5YR VALUE_
input_year = "2018-22"

_0.2 REQUIRED: SET PATH TO OUTPUT FOLDER_
#write table to exp_path
exp_path = "K:/DataServices/Projects/Current_Projects/Housing/StatewideHousingPlan/04_Analysis/Data/Working/DataCommon/"

_0.3 REQUIRED: set table name_
table_name = "b25106_costburden_by_income_acs_m"

__request sample__

get_b25106 <- read.csv(paste0("https://datacommon.mapc.org/csv?table=tabular.",table_name,"&database=ds&years=",input_year,"&year_col=acs_year"))
