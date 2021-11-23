library(tidyverse)
library(sf)
library(here)
library(summarizeNHTS)
library(roll)
source(here("acs-api.R"))
source(here("helper_functions", "lookup_tables.R"))

readin_data <- function(transit_var) {
  
  # create quoted variable for transit variable  
  transit_var_s <- as_label(enquo(transit_var))
  
  # Print message for user re: which variable is being used
  message(glue::glue("Generating {transit_var_s}"))
  
  # -COVID----------------------------------------------------------------------
  # This provides columns as days
  cv <- read_csv(here("Data/COVID-19/csse_covid_19_data/csse_covid_19_time_series","time_series_covid19_confirmed_US.csv"),
                 col_types = cols(
                   .default = col_double(),
                   iso2 = col_character(),
                   iso3 = col_character(),
                   Admin2 = col_character(),
                   Province_State = col_character(),
                   Country_Region = col_character(),
                   Combined_Key = col_character()
                 ))
  
  # Read in the COVID-19 Data
  cv_tidy <- cv %>% 
    select(UID:`6/30/20`) %>% # Only select data up until 6/30/20 (May go from 10 cases to peak
    pivot_longer(`1/22/20`:`6/30/20`, names_to = "Date",
                 values_to = "Cases")
  
  # Population-------------------------------------------------------------------
  
  # Read in CENSUS data
  census_data <- read_csv(here("Data/census", "co-est2019-alldata.csv"),
                          col_types = cols(
                            .default = col_double(),
                            STNAME = col_character(),
                            CTYNAME = col_character()
                          )) %>% 
    select(STATE, COUNTY, POPESTIMATE2019) %>%
    mutate(GEOID = STATE * 1000 + COUNTY) # Reformat the GEOID variable
  
  # Merge the Census data with the COVID-19 data
  cv_tidy_cen <- cv_tidy %>% 
    left_join(census_data, by = c("FIPS" = "GEOID")) %>% 
    filter(!is.na(POPESTIMATE2019)) # Remove Locations with No match
  
  # Locations with no match often mean the MSA has some area in a state but not
  # Attributed to the state or they have missing county. You can see some
  # examples below.
  
  # filter(cv_tidy_cen, is.na(POPESTIMATE2019)) %>% 
  # distinct(Combined_Key) %>% View("Unmatched Census")
  # 
  # filter(cv_tidy_cen, is.na(POPESTIMATE2019)) %>% 
  # distinct(Combined_Key, .keep_all = T) %>% 
  # filter(!grepl("Out of|Unassigne", Combined_Key)) %>%  View()
  # 
  # Places that need to be investigated:
  # KC, Missouri
  # Tricounty Utah
  # It looks like Jackson county is populated independent of KC, Missouri, so 
  # I shouldn't have to worry about the fact that KC, Missouri didn't match to 
  # Census file
  
  
  # CBSA Crosswalk--------------------------------------------------------------
  # We need the crosswalk because the COVID data is only present on the 
  # county level.
  
  # Read in the CBSA Data 
  us_cbsa <- st_read(here("Data/tl_2019_us_cbsa/","tl_2019_us_cbsa.shp"))
  
  # Check that the .shp file was read in correctly
  # [cbsa map](https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-metropolitan-statistical-area-micropolitan-statist)
  # us_cbsa %>% glimpse()
  # us_cbsa %>% st_drop_geometry() %>% View("CBSA")
  
  # [CBSA Crosswalk File](https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm)
  cbsa_cross <- read_csv(here("Data/us-bls","qcew-county-msa-csa-crosswalk-csv.csv"),
                         col_types = "ncccccc")
  
  # NHTS-------------------------------------------------------------------------
  
  # Read in the NHTS Data (Using `summarizeNHTS::`)
  nhts_data <- read_data("2017", csv_path = "Data")
  
  # Read in data from the {{transit_var}} question from NHTS
  # Both the Train and Bus questions are household querstions
  response_table <- summarize_data(
    data = nhts_data,
    agg = "household_count",
    # by = c("HH_CBSA", "TRAIN"), # Variables to stratify
    by = c("HH_CBSA", transit_var_s), # Variables to stratify ("census region CENSUS_R")
    prop = TRUE, # Proportion reported instead of count
    prop_by = "HH_CBSA") %>% # Condition on which variable
    as_tibble() %>% 
    mutate(HH_CBSA_c = as.character(HH_CBSA))
  
  # Join the survey responses to the CBSA crosswalk
  response_table_cross <-  response_table %>% 
    left_join(cbsa_cross, by = c("HH_CBSA_c" = "MSA Title"))
  
  # Join Covariates to NHTS--------------------------------------------------
  
  # Function call reads the acs data into the current environment
  get_acs()
  
  # Clean the GEOID to match the format from the other data sets
  acs_clean <- acs %>% 
    mutate(`MSA Code` = substr(GEOID, 1, 4))
  
  # Join the response table to the ACS data
  response_table_cross_acs <- response_table_cross %>% 
    mutate(`MSA Code` = substr(`MSA Code`, 2, 5)) %>% 
    left_join(acs_clean)
  
  # Merging NHTS and covid-------------------------------------------------------
  
  # OK this merges well, but needs filtration, because we only contain
  # information from CBSA affiliated counties in the transportation data
  cv_responses <- cv_tidy_cen %>% 
    left_join(response_table_cross_acs, by = c("FIPS" = "County Code")) %>% 
    filter(!is.na(HH_CBSA)) %>% 
    # Poverty Data (DP03_119PE)
    # Occupants Per Room 1 or Less (DP04_0077PE)
    # Educational Attainment, Population 25 Years and older, High School grad(DP02_0062PE)
    rename(pct_poverty = DP03_0119PE,
           occupants_few_1 = DP04_0077PE,
           educ_hs_grad = DP02_0062PE)
  
  # The {{transit_var}} and Covid Data Are now together. The next step is to 
  # aggregate the Covid Data to the CBSA level
  
  # cv_responses %>% glimpse()
  
  # The Rader, 2020 Paper gave me the entropy idea
  entropy <- function(x) {
    
    x <- x / sum(x)
    
    # I chose 1E-6 because the minimum was
    return(-sum(x * log2(x + 1E-6)))
    
  }
  
  # Select relevant variables from covid-response data
  cv_responses_small <- cv_responses %>% 
    select(iso2, iso3, FIPS, Admin2, Province_State, HH_CBSA_c, 
            `MSA Type`, {{transit_var}}, W, E, S, Cases, POPESTIMATE2019, Date,
           pct_poverty, occupants_few_1, educ_hs_grad) %>% 
    mutate(Date_d = lubridate::mdy(Date)) 
  
  # Note: You may have to come back for some of the geographic information
  # for maps
  # glimpse(cv_responses_small)
  
  # Creates a daily case variable
  # Hudson NJ had a decrease in cumulative cases on 6/25
  cv_responses_agg <- cv_responses_small %>% # transformed the date
    arrange({{transit_var}}, FIPS, Date_d) %>% # Sort the observations
    group_by(HH_CBSA_c, FIPS, {{transit_var}}, W, E, S) %>% # Operate on grouped observations
    mutate(Daily_Cases = Cases - lag(Cases, default = 0), # Turn cumulative cases to daily cases
           Daily_Cases = ifelse(Daily_Cases < 0, 0, Daily_Cases), # Set negative cases to zero
           Total_Cases = sum(Daily_Cases), # Sum the daily cases within group to produce total cases
           Prob = Daily_Cases / sum(Daily_Cases), # Create probability value for entropy value
           Incidence = Daily_Cases / POPESTIMATE2019 * 100000, # Calculate incidence rate for each MSA
           Smooth_Inc = roll_mean(Incidence, width = 7, min_obs = 1), # Create smoothed incidence
           log2p= log2(Daily_Cases + 1E-6), # Create log2p value for analysis
           Entropy  = entropy(Daily_Cases), # Compute entropy
           Smooth_Entropy = entropy(Smooth_Inc)) %>% # Create entropy for smoothed values
    ungroup() %>% 
    mutate(P_New_Case = Daily_Cases / Total_Cases)
  
  # Aggregation to MSA-----------------------------------------------------------
  
  # Cases were originally reported on county level.
  # Here they are aggregated to a MSA level.
  # NOTE: The covariates were already on an MSA level.
  cv_responses_cbsa <- cv_responses_agg %>% 
    select(-iso2, -iso3, -FIPS, -Admin2, -Province_State) %>% 
    group_by(HH_CBSA_c, `MSA Type`, {{transit_var}}, Date_d,
             pct_poverty, occupants_few_1, educ_hs_grad) %>% 
    summarize(W = max(W), # Weights 
              E = max(E), # Estimate
              S = max(S), # Standard Error
              Cases = sum(Cases), # Number of Cases
              POPESTIMATE2019 = sum(POPESTIMATE2019), # Population of Counties
              .groups = "drop") %>% # Drop grouping after step
    rename(CBSA = HH_CBSA_c) %>%  # Rename CBSA variable
    arrange(CBSA, Date_d) # Sort observations
  
  # Same statistics as above aggregated by CBSA instead of county
  cv_responses_cbsa_agg <- cv_responses_cbsa %>% # transformed the date
    # Filter dates to only include information from before 5/1
    filter(Date_d < lubridate::make_date(2020,05,01)) %>% 
    group_by(CBSA, {{transit_var}}) %>% 
    mutate(Daily_Cases = Cases - lag(Cases, default = 0),
           Daily_Cases = ifelse(Daily_Cases < 0, 0, Daily_Cases),
           Total_Cases = sum(Daily_Cases),
           Prob = Daily_Cases / sum(Daily_Cases),
           Incidence = Daily_Cases / POPESTIMATE2019 * 100000,
           Smooth_Inc = roll_mean(Incidence, width = 7, min_obs = 1),
           log2p= log2(Daily_Cases + 1E-6),
           Entropy  = entropy(Daily_Cases), # Generate entropy for separate entropy analysis
           Smooth_Entropy = entropy(Smooth_Inc),
           Total_Incidence = Total_Cases / POPESTIMATE2019 * 100000) %>% # aggregated over the counties
    ungroup() %>% 
    mutate(P_New_Case = Daily_Cases / Total_Cases, # Create probability of cases for other analysis outcome
           across(c(pct_poverty, occupants_few_1, educ_hs_grad),
                  as.numeric)) # read ACS variables as numeric
  
  # Add on the Census Region variable
  cv_responses_cbsa_agg <- cv_responses_cbsa_agg %>% 
    mutate(Region = census_region_lookup(CBSA))
  
  return(cv_responses_cbsa_agg)

}