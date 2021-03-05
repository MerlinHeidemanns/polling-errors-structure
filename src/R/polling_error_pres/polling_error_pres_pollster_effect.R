## Libraries
library(cmdstanr)
library(tidyverse)
## Load data
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  group_by(pollName) %>%
  filter(n() >= 30 ) %>%
  arrange(pollName, t) %>%
  group_by(pollName, t) %>%
  mutate(pt = cur_group_id()) %>%
  group_by(pollName) %>%
  mutate(p = cur_group_id())
indexes <- read_csv("dta/clean_data/index_pres_prepared.csv")
## Model
m <- file.path("src/stan/polling_error_pres_pollster", "polling_error_pollster.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  P = df %>% pull(p) %>% max(),
  PT = df %>% pull(pt) %>% max(),
  p = df %>% pull(pt),
  #x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 100
)
## Save
##fit$save_object(file = "dta/model_output/polling_error_pres_simple.RDS")

beta <- fit$summary("beta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(pt = 1:n()) %>%
  left_join(df %>% distinct(pt, year, pollName)) %>%
  dplyr::select(-variable, -pt) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  mutate(year = factor(year, levels = as.character(seq(2000, 2020, 4))))

ggplot(data = beta, aes(x = year, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.75) +
  geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size = 0.5) +
  theme_light() +
  facet_wrap(pollName~.) +
  geom_hline(aes(yintercept = 0)) +
  labs(y = "Polling error (%, favoring D, election year average)",
       caption = "Median, 50%, 90%") +
  theme(axis.title.x = element_blank())






