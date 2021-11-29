# Map
library(here)
library(tidyverse)
source("readin-data.R")
library(sf)

cv_responses_cbsa_agg_tr <- readin_data(TRAIN, T)

glue::glue("You read in {count(distinct(cv_responses_cbsa_agg_tr, CBSA))} MSAs")

# > sum(cv_responses_cbsa_nd_bus$Total_Cases)
# [1] 837811
# > sum(cv_responses_cbsa_nd_tr$Total_Cases)
# [1] 747596


# One observation per MSA (Permanent Dataset unlike above)
cv_responses_cbsa_nd_tr <- cv_responses_cbsa_agg_tr %>% 
  filter(TRAIN %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, GEOID, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% 
  select(-TRAIN, -W, -E, -S) %>% # Remove columns that are being aggregated
  distinct(CBSA, .keep_all = T)  # Only keep unique values for each CBSA


# CBSA Crosswalk--------------------------------------------------------------
# We need the crosswalk because the COVID data is only present on the 
# county level.

# Read in the CBSA Data 
us_cbsa <- st_read(here("Data/tl_2019_us_cbsa/","tl_2019_us_cbsa.shp"))

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa_trans <- st_transform(usa, crs = st_crs(us_cbsa))

st_crs(usa_trans)
st_crs(us_cbsa)

map_data <- us_cbsa %>% 
  left_join(cv_responses_cbsa_nd_tr, by = "GEOID") %>% 
  filter(!is.na(Daily_or_Week))

plot(st_geometry(usa))
plot(st_geometry(map_data), add = T,
     col = "red")

# tm_shape(usa_trans) +
#   tm_boarders() +
#   tm_shape(map_data) +
#   tm_polygons(c("Total_Incidence", "pct_poverty", "occupants_few_1", "educ_hs_grad", "Daily_or_Week"))

# If you want all of them in the same map you have to pivot_longer

ggplot() +
  geom_sf(data = usa_trans) +
  geom_sf(data = map_data, mapping = aes(fill = "Total_Incidence"))
