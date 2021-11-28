library(cowplot)
library(here)
library(lme4)
source("readin-data.R")

cv_responses_cbsa_agg_tr <- readin_data(TRAIN, T)

glue::glue("You read in {count(distinct(cv_responses_cbsa_agg_tr, CBSA))} MSAs")

# Univariate Plot ------------------------------------------------------------

# Total Incidence----------
cv_responses_cbsa_agg_tr %>% 
  filter(TRAIN %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% distinct(CBSA, .keep_all = T) %>% 
  mutate(CBSA_Ordered = fct_reorder(as_factor(CBSA), Total_Incidence)) %>% 
  ggplot(aes(x = CBSA_Ordered, y = Total_Incidence)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(title = "Total Incidence Rate Ranking",
       y = "Incidence Rate per 100,000",
       x = "MSA")

# A few times a week, Daily
plt_bar <- cv_responses_cbsa_agg_tr %>% 
  filter(TRAIN %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% distinct(CBSA, .keep_all = T) %>% 
  mutate(CBSA_Ordered = fct_reorder(as_factor(CBSA), Daily_or_Week)) %>% 
  ggplot(aes(x = CBSA_Ordered, y = Daily_or_Week)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  labs(title = "Responses Stating Train Use",
       subtitle = "Either Daily or Weekly",
       y = "Percent Responses Train Daily or Weekly",
       x = "MSA")

# ggsave("images\\train-bar-plot.png", plot = plt_bar, device = png(), width = 6.5,
#        height = 4.52, units = "in")

# One observation per MSA (Permanent Dataset unlike above)
cv_responses_cbsa_nd_tr <- cv_responses_cbsa_agg_tr %>% 
  filter(TRAIN %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% 
  select(-TRAIN, -W, -E, -S) %>% # Remove columns that are being aggregated
  distinct(CBSA, .keep_all = T)  # Only keep unique values for each CBSA

# Bivariate Plot--------------------------------------------------------------

# Total Incidence ------------------------ 
p45 <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(span = .45) 

p65 <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(span = .55) 

p75 <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(span = .75) 

p95 <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(span = .95) 

plot_grid(p45, p65, p75, p95, labels = c('p45', 'p65', 'p75', 'p95'), label_size = 12)

# Total Incidence ------------------------ 
plt_biv_tr <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "Proportion Stating Train Daily or Weekly", y = "Total Incidence Rate per 100,000",
       title = "Total COVID-19 Incidence Vs Train Usage",
       subtitle = "Jan. 22, 2020 - May 1, 2020") +
  theme_bw()

plt_biv_tr

lm(data = cv_responses_cbsa_nd_tr, formula = Total_Incidence ~ Daily_or_Week)

# Total Incidence (Log)------------------
plt <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = log(Total_Incidence), label = CBSA)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Train Daily or Weekly", y = "Total Incidence",
       title = "Total Incidence Vs Daily or Weekly (Log Transform)")

plt

# plotly::ggplotly(plt)

# lm(data = cv_responses_cbsa_nd, formula = log(Total_Incidence) ~ Daily_or_Week)

# summary(lm(data = cv_responses_cbsa_nd, formula = log(Total_Incidence) ~ Daily_or_Week))

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

# Univariate Plot ------------------------------------------------------------

# Total Incidence
cv_responses_cbsa_agg_bus %>% 
  filter(BUS %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% distinct(CBSA, .keep_all = T) %>% 
  mutate(CBSA_Ordered = fct_reorder(as_factor(CBSA), Total_Incidence)) %>% 
  ggplot(aes(x = CBSA_Ordered, y = Total_Incidence)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(title = "Total Incidence Rate Ranking",
       y = "Incidence Rate",
       x = "MSA")

# A few times a week, Daily
plt_bus_bar <- cv_responses_cbsa_agg_bus %>% 
  filter(BUS %in% c("A few times a week", "Daily")) %>%
  group_by(CBSA, Date_d) %>% 
  mutate(Daily_or_Week = sum(W)) %>% 
  ungroup() %>% distinct(CBSA, .keep_all = T) %>% 
  mutate(CBSA_Ordered = fct_reorder(as_factor(CBSA), Daily_or_Week)) %>% 
  ggplot(aes(x = CBSA_Ordered, y = Daily_or_Week)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = c(0,.10,.20,.30), limits = c(0,.35), labels=scales::percent) +
  theme_bw() +
  labs(#title = "Responses Stating Bus Use",
    #subtitle = " Either Daily or Weekly",
    y = "Percent Responses Bus Daily or Weekly",
    x = "MSA")

ggsave("images\\bus-bar-plot.png", plot = plt_bus_bar, device = png(), width = 6.5,
       height = 5.82, units = "in")

# Bivariate Plot--------------------------------------------------------------

# Entropy----------------------------------
# plt <- cv_responses_cbsa_nd_bus %>% 
#   ggplot(aes(x = Daily_or_Week, y = Entropy, label = CBSA)) +
#   geom_point() +
#   geom_smooth(color  = "red") + 
#   labs(x = "Bus Daily or Weekly", y = "Entropy",
#        title = "Entropy Vs Daily or Weekly")
# 
# print(plt)

# plotly::ggplotly(plt)

# lm(data = cv_responses_cbsa_nd_bus, formula = Entropy ~ Daily_or_Week)
# summary(lm(data = cv_responses_cbsa_nd_bus, formula = Entropy ~ Daily_or_Week))


# Total Incidence ------------------------ 
plt_biv_bus <- cv_responses_cbsa_nd_bus %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "Proportion Stating Bus Daily or Weekly", y = "Total Incidence Rate per 100,000",
       title = "Total COVID-19 Incidence Vs Bus Usage",
       subtitle = "Jan. 22, 2020 - May 1, 2020") +
  theme_bw()

plt_biv_bus

# plotly::ggplotly(plt)

# lm(data = cv_responses_cbsa_nd_bus, formula = Total_Incidence ~ Daily_or_Week)

# Total Incidence (Log)------------------
# plt <- cv_responses_cbsa_nd_bus %>% 
#   ggplot(aes(x = Daily_or_Week, y = log(Total_Incidence), label = CBSA)) +
#   geom_point() +
#   geom_smooth(method = "lm") + 
#   labs(x = "Bus Daily or Weekly", y = "Total Incidence",
#        title = "Total Incidence Vs Daily or Weekly (Log Transform)")

# plt

# plotly::ggplotly(plt)

# summary(lm(data = cv_responses_cbsa_nd_bus, formula = log(Total_Incidence) ~ Daily_or_Week))
# Putting plots together
plt_biv_tr_clean <- cv_responses_cbsa_nd_tr %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "Proportion Stating Train Daily or Weekly", y = "Total Incidence Rate per 100,000",
       title = "Total COVID-19 Incidence Vs Train Usage",
       subtitle = "Jan. 22, 2020 - May 1, 2020") +
  theme_bw()

plt_biv_tr_clean

plt_biv_bus_clean <- cv_responses_cbsa_nd_bus %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(colour  = "red") + 
  labs(x = "Proportion Stating Bus Daily or Weekly", y = "Total Incidence Rate per 100,000",
       title = "Total COVID-19 Incidence Vs Bus Usage",
       subtitle = "Jan. 22, 2020 - May 1, 2020") +
  theme_bw()

plt_biv_bus_clean


plot_grid(plt_biv_tr_clean,
          plt_biv_bus_clean,
          labels = c('Fig B','Fig C'),
          label_x = 0.2,
          ncol = 2)

bind_rows(list(Train = cv_responses_cbsa_nd_tr, 
               Bus = cv_responses_cbsa_nd_bus), .id = "Mode") %>% 
  ggplot(aes(x = Daily_or_Week, y = Total_Incidence, label = CBSA)) +
  geom_point() +
  geom_smooth(color = "black") + 
  # scale_x_continuous(limits = c(0, .35))+
  labs(x = "Estimated Proportion of Respondents Stating Ride Frequency (Daily/Weekly) by Metropolitan Statistical Area", y = "Total COVID-19 Incidence Rate (per 100,000)",
       title = "Total COVID-19 Incidence Vs Bus/Train Usage",
       subtitle = "Jan. 22, 2020 - May 1, 2020") +
  theme_bw() +
  theme(text = element_text(family = "serif"))+
  facet_grid(cols = vars(Mode), scales = "free_x")

# > sum(cv_responses_cbsa_nd_bus$Total_Cases)
# [1] 837811
# > sum(cv_responses_cbsa_nd_tr$Total_Cases)
# [1] 747596
