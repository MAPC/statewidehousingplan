# Predicted Housing Unit Flood Inundation  

## Inputs 

### Housing Units 

MAPC imputes units from tax parcels based on address points and [other methodology]. While the number of units will differ from the number of units reported in the Census, we are able to estimate units at the parcel level which is a scale necessary for this analysis. 

### Flooding Extents 

The Massachusetts Coastal Flood Risk Model (MC-FRM) shows Percent Storm Exceedance Probability (0.1% annual chance) as extents for 2030, 2050, and 2070 and feet of inundation from 1 to 10 feet. This analysis relies on the Percent Strome Exceedance Probability extents to determine whether a parcel with housing units is likely to experience coastal flooding in the coming decades.  

## Limitations 

Housing units on each parcel are estimates and the location of the unit relative to the parcel is not taken into account. If the MC-FRM area intersects with the parcel, regardless of its proximity to the built structure, the units are counted as flooded.  

## Analysis 

### Procedure:  

    1. Filter (using st_filter() from the {sf} package) parcels that fall into the MC-FRM areas. 

    2. Group remaining parcels by land use (USE_CODE_GEOM) and municipality (CITY) and summarize: 

      a. Total Units (sum of imputed units)  

      b. Total Assessed Value (2023 Dollars)  
