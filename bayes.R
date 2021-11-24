library(cowplot)
library(here)
library(lme4)
library(corrplot)
library(brms)
source("readin-data.R")

cv_responses_cbsa_agg_tr <- readin_data(TRAIN)

glue::glue("You read in {count(distinct(cv_responses_cbsa_agg_tr, CBSA))} MSAs")

# > sum(cv_responses_cbsa_nd_bus$Total_Cases)
# [1] 837811
# > sum(cv_responses_cbsa_nd_tr$Total_Cases)
# [1] 747596

# One observation per MSA (Permanent Dataset unlike above)
cv_responses_cbsa_nd_tr <- cv_responses_cbsa_agg_tr %>% 
  filter(TRAIN %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% 
  select(-TRAIN, -W, -E, -S) %>% # Remove columns that are being aggregated
  distinct(CBSA, .keep_all = T)  # Only keep unique values for each CBSA

rescale <- function(x) (x - mean(x)) / sd(x)

cv_responses_cbsa_nd_tr_sc <- cv_responses_cbsa_nd_tr %>% 
  mutate(across(c(Daily_or_Week, pct_poverty, occupants_few_1, educ_hs_grad), 
                 .fns  = rescale)) 

# Full Model (Neg Binom)----------------

cov_data_tr <- transmute(cv_responses_cbsa_nd_tr_sc, CBSA, pct_poverty, occupants_few_1, 
          educ_hs_grad, incidence = Total_Cases / POPESTIMATE2019, Daily_or_Week)

model_beta_bayes <- brm(
  bf(incidence ~ Daily_or_Week + pct_poverty + occupants_few_1 +
     educ_hs_grad,
     phi ~ Daily_or_Week + pct_poverty + occupants_few_1 +
       educ_hs_grad),
  data = cov_data_tr,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4, seed = 2113, 
  backend = getOption("brms.backend", "rstan")
  # Use the cmdstanr backend for Stan because it's faster and more modern than
  # the default rstan You need to install the cmdstanr package first
  # (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan() to
  # install cmdstan on your computer.
  # backend = "cmdstanr"
)

model_beta_bayes
plot(model_beta_bayes)
