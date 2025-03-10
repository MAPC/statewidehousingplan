### Functions

`functions.R` contains functions written previously or for use in the statewide housing plan by MAPC staff that are utilized in the analyses listed above. 
The functions contained in this script are documented below, with more detailed comments included in the script itself.

`mortgage_calculator()`

inputs:
- a data frame that includes a property's sale price, assessed home value, sale date (year), and residence type
- down payment percentage entered as a decimal
- loan term in years
- cost of annual homeowners insurance
- annual private mortgage insurance rate

output:
- the input data frame with additional columns for the estimated monthly mortgage principle, mortgage with insurance, and total monthly payment

assumptions:
for the statewide housing plan the mortgage calculator was run with the following assumptions
- a 10% down payment
- a 30 year loan term
- $1,000 a year for homeowners insurance
- condo fees were estimated based on weighted averages from PUMS of condo fees in Massachusetts in 2000, 2010, and 2020

methods:
1. use federal mortgage interest rates from Freddie Mac to calculate an average annual interest rate for each calendar year
2. join annual municipal property tax rates to data, calculate rate as percentage rather than dollar amount per $1000, assign 2003 tax rates to properties sold from 2000-2002 to supplement missing data
3. filter input data frame to only show condos and single family homes
4. the mortgage principle is calculated based on the input down payment percentage as price - (price*down_payment_p)
5. the loan term is calculated in months based on the loan term in years input to the function
6. the monthly interest rate is calculated for each calendar year by taking 1/12 of the averaged annual interest rate
7. the monthly mortgage payment is then calculated using the formula M= P ((r(1+r)^n)/((1+r)^n-1 where P is the mortgage principle, r is the monthly mortgage interest amount, and n is the loan term in months
8. property tax is calculated by multiplying the sale price of the home by the average property tax rate for the municipality and then diving by 12
9. the final monthly payment is derived by summing the monthly mortgage payment, property tax, homeowners' insurance, and where the property is a condo, the monthly condo fee


`affordable_sales()`

inputs:
- a dataframe that has gone through the mortgage calculator (must include fields for monthly_payment, number of bedrooms, month, year, and municipal ID)
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

