library(cowplot)
library(here)
library(lme4)
library(corrplot)
source("readin-data.R")

cv_responses_cbsa_agg_tr <- readin_data(TRAIN, T)

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

# Correlation Matrix---------------------

corr_mat <- cor(select(cv_responses_cbsa_nd_tr, 
           Daily_or_Week, Total_Incidence , pct_poverty, occupants_few_1, educ_hs_grad))

corrplot(corr_mat)


# Full Model-----------------------------
# Log linear model
log_model <- lm(data = cv_responses_cbsa_nd_tr, formula = log(Total_Incidence) ~ Daily_or_Week +
     pct_poverty + occupants_few_1 + educ_hs_grad)

summary(log_model)
AIC(log_model)

# GLM Poisson
poisson_model <- glm(data = cv_responses_cbsa_nd_tr, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad  + offset(log(POPESTIMATE2019)),
                     family = "poisson")

summary(poisson_model)

# GLM Poisson Without new york
cv_responses_cbsa_nd_tr_no_ny <- cv_responses_cbsa_nd_tr %>% filter(!grepl("New York", CBSA))

poisson_model_ny <- glm(data = cv_responses_cbsa_nd_tr_no_ny, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad  + offset(log(POPESTIMATE2019)),
                     family = "poisson")

summary(poisson_model_ny)


# Full Model (Multilevel)----------------

cv_responses_cbsa_nd_tr_sc <- cv_responses_cbsa_nd_tr %>% 
  mutate(pct_poverty = (pct_poverty - mean(pct_poverty)) / sd(pct_poverty),
         occupants_few_1 = (occupants_few_1 - mean(occupants_few_1)) / sd(occupants_few_1),
         educ_hs_grad = (educ_hs_grad - mean(educ_hs_grad)) / sd(educ_hs_grad))

poisson_model <- glmer(data = cv_responses_cbsa_nd_tr, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad  + (1|Region) + offset(log(POPESTIMATE2019)),
                     family = poisson(link = "log"))

summary(poisson_model)

# Full Model (Neg Binom)----------------

rescale <- function(x) (x - mean(x)) / sd(x)

cv_responses_cbsa_nd_tr_sc <- cv_responses_cbsa_nd_tr %>% 
  mutate(across(c(Daily_or_Week, pct_poverty, occupants_few_1, educ_hs_grad), rescale))# %>% 
# filter(!grepl("New York", CBSA))

nb_model <- glmer.nb(data = cv_responses_cbsa_nd_tr_sc, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019)))

summary(nb_model)

confint(nb_model)

exp(confint(nb_model))
exp(fixef(nb_model))
# Full Model (Neg Binom test interaction----
# 
# AIC(nb_model)
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_tr_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019))))
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_tr_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * occupants_few_1 + (1|Region) + offset(log(POPESTIMATE2019))))
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_tr_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * pct_poverty + (1|Region) + offset(log(POPESTIMATE2019))))

# These are the estimated negative binomial regression coefficients for the 
# model. Recall that the dependent variable is a count variable that is either 
# over- or under-dispersed, and the model models the log of the expected count 
# as a function of the predictor variables

# Full Model (Neg Binom ~NY)------------

rescale <- function(x) (x - mean(x)) / sd(x)

cv_responses_cbsa_nd_tr_sc_ny <- cv_responses_cbsa_nd_tr %>% 
  mutate(across(c(Daily_or_Week, pct_poverty, occupants_few_1, educ_hs_grad), rescale))  %>% 
  filter(!grepl("New York", CBSA))

nb_model <- glmer.nb(data = cv_responses_cbsa_nd_tr_sc_ny, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019)))

summary(nb_model)

confint(nb_model)

exp(confint(nb_model))

# BUS------------------------------------------------------------------------

cv_responses_cbsa_agg_bus <- readin_data(BUS)

glue::glue("You read in {count(distinct(cv_responses_cbsa_agg_bus, CBSA))} MSAs")

# One observation per MSA (Permanent Dataset unlike above)
cv_responses_cbsa_nd_bus <- cv_responses_cbsa_agg_bus %>% 
  filter(BUS %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% 
  select(-BUS, -W, -E, -S) %>% # Remove columns that are being aggregated
  distinct(CBSA, .keep_all = T)  # Only keep unique values for each CBSA

# Correlation Matrix---------------------

corr_mat <- cor(select(cv_responses_cbsa_nd_bus, 
                       Daily_or_Week, Total_Incidence , pct_poverty, occupants_few_1, educ_hs_grad))

corrplot(corr_mat)

# Full Model-----------------------------
log_model <- lm(data = cv_responses_cbsa_nd_bus, formula = log(Total_Incidence) ~ Daily_or_Week +
                  pct_poverty + occupants_few_1 + educ_hs_grad)

summary(log_model)
AIC(log_model)

cv_responses_cbsa_nd_bus_no_ny <- cv_responses_cbsa_nd_bus %>% filter(!grepl("New York", CBSA))

poisson_model <- glm(data = cv_responses_cbsa_nd_bus, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad  + offset(log(POPESTIMATE2019)),
                     family = "poisson")

summary(poisson_model)

poisson_model <- glm(data = cv_responses_cbsa_nd_bus_no_ny, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad  + offset(log(POPESTIMATE2019)),
                     family = "poisson")

summary(poisson_model)

# Full Model (Multilevel)----------------

cv_responses_cbsa_nd_bus_sc <- cv_responses_cbsa_nd_bus %>% 
  mutate(pct_poverty = (pct_poverty - mean(pct_poverty)) / sd(pct_poverty),
         occupants_few_1 = (occupants_few_1 - mean(occupants_few_1)) / sd(occupants_few_1),
         educ_hs_grad = (educ_hs_grad - mean(educ_hs_grad)) / sd(educ_hs_grad))

poisson_model <- glmer(data = cv_responses_cbsa_nd_bus, formula = Total_Cases ~  Daily_or_Week +
                         pct_poverty + occupants_few_1 + educ_hs_grad  + (1|Region) + offset(log(POPESTIMATE2019)),
                       family = poisson(link = "log"))

summary(poisson_model)

# Full Model (Neg Binom)----------------

rescale <- function(x) (x - mean(x)) / sd(x)

cv_responses_cbsa_nd_bus_sc <- cv_responses_cbsa_nd_bus %>% 
  mutate(across(c(Daily_or_Week, pct_poverty, occupants_few_1, educ_hs_grad), rescale))#  %>% 
   # filter(!grepl("New York", CBSA))

nb_model <- glmer.nb(data = cv_responses_cbsa_nd_bus_sc, formula = Total_Cases ~  Daily_or_Week +
                         pct_poverty + occupants_few_1 + educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019)))

summary(nb_model)

confint(nb_model)

exp(confint(nb_model))

# Full Model (Neg Binom ~NY)------------

rescale <- function(x) (x - mean(x)) / sd(x)

cv_responses_cbsa_nd_bus_sc_ny <- cv_responses_cbsa_nd_bus %>% 
  mutate(across(c(Daily_or_Week, pct_poverty, occupants_few_1, educ_hs_grad), rescale))  %>% 
  filter(!grepl("New York", CBSA))

nb_model <- glmer.nb(data = cv_responses_cbsa_nd_bus_sc_ny, formula = Total_Cases ~  Daily_or_Week +
                       pct_poverty + occupants_few_1 + educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019)))

summary(nb_model)

confint(nb_model)

exp(confint(nb_model))
exp(fixef(nb_model))
# Full Model (Neg Binom test interaction----
# 
# AIC(nb_model)
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_bus_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * educ_hs_grad + (1|Region) + offset(log(POPESTIMATE2019))))
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_bus_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * occupants_few_1 + (1|Region) + offset(log(POPESTIMATE2019))))
# 
# summary(glmer.nb(data = cv_responses_cbsa_nd_bus_sc, formula = Total_Cases ~  Daily_or_Week +
#            pct_poverty + occupants_few_1 + educ_hs_grad + Daily_or_Week * pct_poverty + (1|Region) + offset(log(POPESTIMATE2019))))

