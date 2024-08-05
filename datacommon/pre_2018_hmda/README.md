### Data loading and cleaning of HMDA files 2007-2017


#### CODE 

##### adapted from the code in this folder 

*original code folder*: K:\DataServices\Datasets\Housing\HMDA\Code\2007-2017

*source file*: K:\DataServices\Datasets\Housing\HMDA\Data\Raw\Tabular\2007_2017\hmda_lar.csv

##### author:  Taylor Perez


__1_hmda_cleaning__:  this script loads large data file and renames the columns.

The source data file (hmda_lar.csv) is too large [3.3 GB] for citrix VM

In the original code, the file was imported to df then a function was run to rename all columns. The  processing of the full matrix of a million + rows crashed on VM.


To facilitate the script, a filter by year was added *before* the renaming columns function.  For each year, the processing time is very fast, and allows for checking the output by year.


*project folder*: K:\DataServices\Datasets\Housing\HMDA\Code\datacommon\pre_2018_hmda

*output folder*: K:\DataServices\Datasets\Housing\HMDA\Code\datacommon\pre_2018_hmda\output



__2_hmda_geographies__:  this script joins an index of parent and related geographies

*index saved to \output folder*:  census_tracts_2000_munijoin_trim.csv