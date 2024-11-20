
# Function to estimate monthly homeownership cost
## the sale price of the home comes from the input dataframe (we use warren group data)
## the function pulls in average weekly mortgage insurance rates from freddie mac and municipal property tax rates to include in the monthly cost estimate alongside the inputs below
## inputs: down payment (%), loan term (years), homeowners inusrance ($), condo fee ($)
## output: a dataframe that appends fields estimated the monthly mortgage principle (mortgage_principle_m), monthly mortgage with interest (mortgage_interest_m), 
### and the total monthly payment including all variables outlined above

mortgage_calculator <- function(df, down_payment, loan_term, ho_insurance, condo_fee){
  ## MAKE VARIABLES 
  # data paths
  calc_data_path <- "K:/DataServices/Datasets/Housing/Warren Group - Home Sales/attainable_housing/calculator_data/"
  # start and end year for data
  start_year = min(df$year)
  end_year = max(df$year)
  
  ## MORTGAGE INTERST RATES TABLE
  # these are national mortgage rates downloaded from Freddie Mac here: https://www.freddiemac.com/pmms
  # we calculate and average annual mortgage rate to use in our cost estimate
  fmrate = read_csv("https://www.freddiemac.com/pmms/docs/PMMS_history.csv") |>
    mutate(
      date_clean = mdy(date),
      year = year(date_clean)
    ) |> 
    group_by(year) |> 
    summarize(mortgage_rate_30 = mean(pmms30))
  
  ## PROPERTY TAX RATE TABLE - THIS TABLE WILL NEED TO BE UPDATED WITH FUTURE YEARS AS DATA BECOMES AVAILABLE!
  prop_taxrate = read_csv(paste0(calc_data_path, "prop_taxrates_ma_2024.csv")) |> 
    # filter table to only include years represented in the data frame
    filter(fyear >= start_year & fyear <= end_year) |>
    group_by(Municipality) |> 
    # calculate the average property tax rate by municipality in the given time frame
    summarise(proptaxrate = mean(Residential)/1000) |>
    rename(municipal = Municipality)
  
  ## MORTGAGE CALCULATOR
  mortgage_df <- df |> 
    # joining average property tax rate to warren data by municipality
    left_join(prop_taxrate, by = "municipal") |> 
    # joining mortgage rates
    left_join(fmrate, by = 'year') |> 
    # calculate estimated monthly payment
    mutate(
    # get monthly principle cost
    mortgage_principle_m = (price - down_payment)/(loan_term*12),
    # adding mortgage interest
    mortgage_interest_m = mortgage_principle_m*(1+(mortgage_rate_30/100)),
    # get monthly property tax based on assessed value of home at date of sale
    property_tax_m = (assdvaltot*proptaxrate)/12,
    # adding property tax
    mortgage_interest_tax = mortgage_interest_m + property_tax_m, 
    # add other potential monthly costs
    monthly_payment = ifelse(proptype == 'RCD', 
                             mortgage_interest_tax + ho_insurance + condo_fee,
                             mortgage_interest_tax + ho_insurance)
    ) |> 
    # remove interim fields
    select(-c(proptaxrate, mortgage_rate_30, property_tax_m, mortgage_interest_tax))
    
  return(mortgage_df)
}
