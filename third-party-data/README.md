## Scripts for requests involving third party data

#### 2024

### Analayses
__1. units in coastal flood risk areas__  

__2. sales affordable to middle income buyers__ 

`home-sale-affordability` this script utilizes the mortgage_calculator() and affordable_sales() functions in the functions.R script to output tables showing
the count and percent of sales by municipality and fiscal year that were affordable at different low-income (30%, 50%, and 80% AMI) and middle_income (100% and 120% AMI) thresholds.


### Functions

`functions.R` contains functions written previously or for use in the statewide housing plan by MAPC staff that are utilized in the analyses listed above. 
The functions contained in this script are documented below, with more detailed comments included in the script itself.

`mortgage_calculator()`
inputs:
- a data frame that includes a property's sale price, assessed home value, sale date (year), and residence type
- down payment percentage entered as a decimal
- loan term in years
- cost of annual homeowners insurance
- monthly condo fee

output:
- the input data frame with additional columns for the estimated monthly mortgage principle, mortgage with insurance, and total monthly payment

assumptions:
for the statewide housing plan the mortgage calculator was run with the following assumptions
- a 20% downpayment
- a 30 year loan term
- $1,000 a year for homeowners insurance
- $300 a month for condo fees

methods:
1. use federal mortgage interest rates from Freddie Mac and calculate and average annual interest rate
2. use municipal property tax rates and calculate the average rate by municipality over the entire data period
3. filter input data frame to only show condos and single family homes
4. the monthly mortgage principle is calculated as (price - down payment)/(loan term * 12)
6. property tax is calculated using the average property tax rate for the municipality and the assessed value of the home
7. the final monthly payment is calculated by summing the monthly mortgage principle, mortgage interest, proerty tax, homeowners insurance, and where the property is a condo, the monthly condo fee


`affordable_sales()`
inputs:
- a dataframe that has gone through the mortgage calculator (must include fields for monthly_payment, number of bedrooms, month, year, and muncipal ID)
- 'count' or 'percent' depending on the output summary table wanted

output:
- a summary table by municipality, fiscal year, and household size that includes the total number of condos and single family properties
