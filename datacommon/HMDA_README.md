### SHoP: Update of HMDA mortgage denials for income > 120% HUD Income Limits AMI 

*script*: SHoP_hmda_processing_muni_120pct_aggr.R

*code folder*: K:\DataServices\Projects\Current_Projects\Housing\StatewideHousingPlan_SHoP_datacommon\

*import_path* = "K:/DataServices/Datasets/Housing/HMDA/Data/Raw/Tabular/"

*exp_path* = "K:/DataServices/Datasets/Housing/HMDA/Data/Modified/Tabular/[year]"

NOTE: because the script involves join with ACS 5yr data, the latest year available will be latest ACS 5 yr.


### Data loading and cleaning of HMDA files 2007-2017


#### PREVIOUS CODE EXPLAINED BELOW, FOR EARLIER YEARS USE THIS NEW VERSION:

 __SHoP_hmda_processing_muni_120pct_aggr_pre2018.R__ 


##### original code for 2007-17 is in this folder: 

*original code folder*: K:\DataServices\Datasets\Housing\HMDA\Code\2007-2017

*source file*: K:\DataServices\Datasets\Housing\HMDA\Data\Raw\Tabular\2007_2017\hmda_lar.csv

##### author:  Taylor Perez


__1_hmda_cleaning__:  the script above loads large data file and renames the columns.

The source data file (hmda_lar.csv) is too large [3.3 GB] for citrix VM

In the original code, the file was imported to df then a function was run to rename all columns. The  processing of the full matrix of a million + rows crashed on VM.

In the revised version of the script, raw data is filtered by year *before* any processing.

*project folder*: K:\DataServices\Datasets\Housing\HMDA\Code\datacommon\SHoP_hmda_processing_muni_120pct_aggr_pre2018.R

*output folder*: K:\DataServices\Datasets\Housing\HMDA\Data\Modified\Tabular\2007-2017\redo\

__2_hmda_geographies__:  this script joins an index of parent and related geographies

*index saved to \output folder*:  census_tracts_2000_munijoin_trim.csv


__3_hmda_subsetting_income__:  this script loops through the raw data (with geographies) to calculate income brackets


__4_hmda_subsetting_race__:  this script loops through the raw data (with geographies) to calculate applications and denials by race and ethnicity groups.


### output samples

*hmda_codes_cleaned_2017.csv*

*hmda_geog_2017.csv*

*hmda_income_2017.csv*

*hmda_re_2017.csv*