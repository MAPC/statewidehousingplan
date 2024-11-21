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

assumptions:
- to avoid calculating affordability of homes where households would be overcrowded, a minimum number of bedrooms is set for each household size such that there is at least one bedroom for every two people in the household
- 'affordable' for each group is considered 30% of the monthly HUD income limit

methods:
1. HUD income limits are pulled from MAPC's database
2. 100% AMI is calculated for all household sizes by calculating twice the 50% limits
3. 120% AMI is then calculated as 1.2 times the 100% income limits
4. For all income groups (30%, 50%, 80%, 100%, and 120%) a monthly housing affordability threshold is calculated as 30% of monthly income ((annual income limit / 12 months)*.3)
The following steps occur in a loop that runs 8 times, once for each household size 1 through 8
5. the input data frame is filtered to exclude homes where a household of the given size would be overcrowded
6. new variables are created for 30%, 50%, 80%, 100%, and 120% AMI and the value in each is TRUE where the income limit is greater than the monthly payment estimated by the mortgage calculator
7. the new variables created in step 6 are summarized by municipality, fiscal year, and household size and reported as either counts or percents based on the function input

