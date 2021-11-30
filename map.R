# Map
library(here)
library(tidyverse)
source("readin-data.R")
library(sf)
library(tmap)

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
st_crs(us_cbsa_train)

map_data_train <- us_cbsa %>% 
  left_join(cv_responses_cbsa_nd_tr, by = "GEOID") %>% 
  filter(!is.na(Daily_or_Week))

# plot(st_geometry(usa))
# plot(st_geometry(map_data_train), add = T,
#      col = "red")

# tmap_options(check.and.fix = T)

# If you want all of them in the same map you have to pivot_longer
rescale <- function(x) (x - mean(x)) / sd(x)

map_data_plot_train <- map_data_train %>% 
  mutate(log_total_cases = log(Total_Cases),
         transit_prop_dayweek_train = Daily_or_Week) %>%
  mutate(across(c(log_total_cases, transit_prop_dayweek_train, pct_poverty, occupants_few_1, educ_hs_grad), rescale)) %>% 
  pivot_longer(c("log_total_cases", "transit_prop_dayweek_train", "pct_poverty", "occupants_few_1", "educ_hs_grad"),
               names_to = "Var", values_to = "Value")

# ggplot() +
#   geom_sf(data = usa_trans) +
#   geom_sf(data = map_data_plot_train, mapping = aes(fill = Value)) +
#   facet_wrap(~Var)

# BUS-----------------------------


cv_responses_cbsa_agg_bus <- readin_data(BUS, T)

glue::glue("You read in {count(distinct(cv_responses_cbsa_agg_tr, CBSA))} MSAs")

# > sum(cv_responses_cbsa_nd_bus$Total_Cases)
# [1] 837811
# > sum(cv_responses_cbsa_nd_tr$Total_Cases)
# [1] 747596


# One observation per MSA (Permanent Dataset unlike above)
cv_responses_cbsa_nd_bus <- cv_responses_cbsa_agg_bus %>% 
  filter(BUS %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, GEOID, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% 
  select(-BUS, -W, -E, -S) %>% # Remove columns that are being aggregated
  distinct(CBSA, .keep_all = T)  # Only keep unique values for each CBSA


# CBSA Crosswalk--------------------------------------------------------------
# We need the crosswalk because the COVID data is only present on the 
# county level.

# Read in the CBSA Data 
# us_cbsa <- st_read(here("Data/tl_2019_us_cbsa/","tl_2019_us_cbsa.shp"))
# 
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
# usa_trans <- st_transform(usa, crs = st_crs(us_cbsa))
# 
# st_crs(usa_trans)
# st_crs(us_cbsa)

map_data_bus <- us_cbsa %>% 
  left_join(cv_responses_cbsa_nd_bus, by = "GEOID") %>% 
  filter(!is.na(Daily_or_Week))

# plot(st_geometry(usa))
# plot(st_geometry(map_data), add = T,
#      col = "red")

# If you want all of them in the same map you have to pivot_longer
rescale <- function(x) (x - mean(x)) / sd(x)

map_data_plot_bus <- map_data_bus %>% 
  mutate(log_total_incidence = log(Total_Incidence),
         transit_prop_dayweek_bus = Daily_or_Week) %>%
  mutate(across(c(log_total_incidence, transit_prop_dayweek_bus, pct_poverty, occupants_few_1, educ_hs_grad), rescale)) %>% 
  pivot_longer(c("log_total_incidence", "transit_prop_dayweek_bus", "pct_poverty", "occupants_few_1", "educ_hs_grad"),
               names_to = "Var", values_to = "Value") %>% 
  bind_rows(filter(map_data_plot_train, Var == "transit_prop_dayweek_train"))


outlier <- function(x) {
  
  scale_top <- quantile(x, .75) + 1.5 * IQR(x)
  
  x <- ifelse(x > scale_top, scale_top, x)
  
  x_with_ceiling <- ifelse(x < -scale_top, -scale_top, x)
  
  return(x_with_ceiling)
  
  }

ggplot() +
  geom_sf(data = usa_trans, colour = "grey60") +
  geom_sf(data = map_data_plot_bus, mapping = aes(fill = outlier(Value)),
          colour = "grey60") +
  facet_wrap(~Var, ncol = 2) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        text = element_text(family = "serif"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_fill_continuous(name = "Scaled\nValues")
  # scale_fill_gradient2(limits=c(NA, outlier(map_data_plot_bus$Value)))

