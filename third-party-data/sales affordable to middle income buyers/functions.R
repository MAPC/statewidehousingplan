
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
    # fix muni names where needed
    mutate(
      Municipality = case_when(
        `DOR Code` == "166" ~ "Manchester",
        TRUE ~ Municipality
      )
    ) |> 
    # property tax rate is reported as amount per $1000 of assessed value
    # calculate this ratio
    mutate(proptaxrate = Residential/1000) |> 
    # select only necessary columns and then rename
    select(Municipality, fyear, proptaxrate) |> 
    `colnames<-` (c('municipal', 'fy_year', 'proptaxrate')) |> 
    # need to create rows for property taxes from 2000-2002
    group_by(municipal) |>
    complete(fy_year = full_seq(2000:2024, 1)) |> 
    # applying 2003 tax rates back to 2000 through 2002
    fill(proptaxrate, .direction = "up") 
  

  
  ## MORTGAGE CALCULATOR
  mortgage_df <- df |> 
    # filter to only include sales of sinlge family homes and condos
    filter(restype %in% c("R1F", "CON")) |> 
    # make clean fiscal year field
    mutate(
      fy_year = ifelse(month <= 6, year, year+1),
      # fix muni names where needed
      municipal = case_when(
        muni_id == 166 ~ "Manchester",
        muni_id == 16 ~ "Attleboro",
        TRUE ~ municipal)
    ) |> 
    # joining average property tax rate to warren data by municipality
    left_join(prop_taxrate, by = c("municipal", "fy_year")) |> 
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

# Function to compare monthly homeownership costs to HUD income levels
## input 1: a table that has gone through the mortgage calculator and includes fields for muni_id, fiscal year, number of bedrooms, and estimated monthly payment 
## input 2: the output wanted in the summary table - either 'count' or 'percent'
## output: a summary table with either counts or percents of homes affordable to buyers at different income levels by municipality, fiscal year, and household size

affordable_sales <- function(df, output_type) {

  ### read in table with HUD income limits from database
  # Set the driver
  drv = dbDriver("PostgreSQL")
  # Prompt database connection 
  db_connection <- dbConnect(drv, host = Sys.getenv("host"), port = Sys.getenv("port"), dbname = Sys.getenv("dbname"), 
                             user = Sys.getenv("user"), password = Sys.getenv("password"))
  
  ami_table <- dbGetQuery(db_connection, "SELECT * FROM tabular.hous_section8_income_limits_by_year_m") |> 
    mutate(
    # fix muni names where needed
      municipal = case_when(
        muni_id == 166 ~ "Manchester",
        TRUE ~ municipal
      ), 
    # create income limits for 100% and 120% AMI
      # 100% AMI
      il_100_1 = il_50_1*2,
      il_100_2 = il_50_2*2,
      il_100_3 = il_50_3*2,
      il_100_4 = il_50_4*2,
      il_100_5 = il_50_5*2,
      il_100_6 = il_50_6*2,
      il_100_7 = il_50_7*2,
      il_100_8 = il_50_8*2,
      #120% AMI
      il_120_1 = il_100_1*1.2,
      il_120_2 = il_100_2*1.2,
      il_120_3 = il_100_3*1.2,
      il_120_4 = il_100_4*1.2,
      il_120_5 = il_100_5*1.2,
      il_120_6 = il_100_6*1.2,
      il_120_7 = il_100_7*1.2,
      il_120_8 = il_100_8*1.2,
      # mutate columns so they show the upper limit of 1/3 of monthly income rather than annual income
      across(.cols = -c(seq_id, muni_id, municipal, countyname, areaname, fy_year, median), function(x){(x/12)*0.3})
    ) 
  #close db connection
  dbDisconnect(db_connection)
  
  # create blank output table to write to
  output <- NULL
  
  ### loop through household sizes 1-8
  for (hh_size in 1:8){
    # choose only columns for current household size from ami table and rename columns
    ami_table_filtered <- ami_table |> 
      select(muni_id, municipal, fy_year, ends_with(as.character(hh_size))) |> 
      `colnames<-`(c("muni_id", "municipal", "fy_year", "il50","il30","il80","il100", "il120"))
      
    # create variable for bed size
    bed_size = case_when(
      hh_size <= 2 ~ 0,
      hh_size <= 4 ~ 2,
      hh_size <= 6 ~ 3, 
      hh_size > 6 ~ 4 
    )  
    
    # build output table for current hh size
    output[[hh_size]] <- df |> 
      # select only needed columns
      select(muni_id, bedrooms, month, year, fy_year, monthly_payment) |> 
      # filter to minimum number of bedrooms for household size
      filter(bedrooms >= bed_size) |> 
      # join in ami table by hh size
      left_join(ami_table_filtered, by = c("muni_id", "fy_year")) |>
      # create fields to track affordability at different income limits
      mutate(
        hh_size = hh_size,
        affordable_30 = ifelse(monthly_payment <= il30, TRUE, FALSE),
        affordable_50 = ifelse(monthly_payment <= il50, TRUE, FALSE),
        affordable_80 = ifelse(monthly_payment <= il80, TRUE, FALSE),
        affordable_100 = ifelse(monthly_payment <= il100, TRUE, FALSE),
        affordable_120 = ifelse(monthly_payment <= il120, TRUE, FALSE)
        ) |>
      arrange(fy_year, municipal) |> 
      group_by(municipal, fy_year, hh_size) |> 
      # remove 2024 fiscal year while we have incomplete data & sales with missing municipalities
      filter(fy_year != 2024 & !is.na(municipal))
      
    # if else statement to determine what summary table to output based on function input
    if(output_type == 'count'){
      output[[hh_size]] <- mutate(output[[hh_size]],
        # get total transactions that were available to a hh at the given size
        total_transactions = n(),
        # get count of affordable transactions at different income levels
        across(starts_with('affordable_'), sum)
      ) |> 
        select(municipal, fy_year, hh_size, total_transactions, affordable_30, affordable_50, affordable_80, affordable_100,
               affordable_120) |> 
        distinct()
    } else if (output_type == 'percent'){
      output[[hh_size]] <- mutate(output[[hh_size]],
        # get total transactions that were available to a hh at the given size
        total_transactions = n(),
        # get percent of affordable transactions at different income levels
        across(starts_with('affordable_'), function(x){round(sum(x)/total_transactions, digits = 3)})
      ) |> 
        select(municipal, fy_year, hh_size, total_transactions, affordable_30, affordable_50, affordable_80, affordable_100,
               affordable_120) |> 
        distinct()
    } else{
      print("Please specify either count or percent as an output")
    }
  }
  output_all <- rbindlist(output)
  return(output_all)
}

