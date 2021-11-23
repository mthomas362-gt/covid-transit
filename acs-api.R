# api test

# https://www.census.gov/data/developers/data-sets.html
library(httr)

get_acs <- function(){
  
  ky <- read.table(here::here("Data", ".ENV"))[[1]]
  
  # Test Data
  # acs_response <- GET(glue::glue("https://api.census.gov/data/",
  #           "2019/acs/acs5/profile?",
  #           "get=NAME,DP02_0001E&",
  #           "for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:*",
  #           "&key={ky}"))
  # 
  # acs_text <- content(acs_response, as = "text")
  # 
  # jsonlite::fromJSON(acs_text) %>% View()
  
  # API Available Data Sets
  # https://www.census.gov/data/developers/data-sets.html
  # Examples
  # https://api.census.gov/data/2019/acs/acs5/profile/examples.html
  # Variables
  # https://api.census.gov/data/2019/acs/acs5/profile/variables.html
  
  # Variable Lookup
  # https://api.census.gov/data/2019/acs/acs5/profile/variables.html
  # Poverty Data (DP03_119PE)
  # Occupants Per Room 1 or Less (DP04_0077PE)
  # Educational Attainment, Population 25 Years and older, High School grad(DP02_0062PE)
  # Occupation (Still have to find that tbh)
  
  # You should consider (DP02_0067PE), it's more accurate because it's
  # "High school graduate or higher" as opposed to (DP02_0062PE) "High school 
  # graduate (includes equivalency)"
  acs_response <- GET(glue::glue("https://api.census.gov/data/",
                                 "2019/acs/acs5/profile?",
                                 "get=NAME,DP03_0119PE,DP04_0077PE,DP02_0062PE&",
                                 "for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:*",
                                 "&key={ky}"))
  
  acs_text <- content(acs_response, as = "text")
  
  acs_text_df <- jsonlite::fromJSON(acs_text)
  
  acs_text_df_data <- data.frame(acs_text_df[-1,])
  
  names(acs_text_df_data) <- acs_text_df[1,]
  
  acs <- acs_text_df_data %>% 
    rename(GEOID = `metropolitan statistical area/micropolitan statistical area`)
  
  acs <<- acs

}