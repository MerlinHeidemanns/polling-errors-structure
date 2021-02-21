library(tidyverse)
library(cmdstanr)
## Fit
fit <- readRDS("data/us_input/polling_error/polling_error_additive_predictors.RDS")
## Data
# Region indicators
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
# Polls
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()
df <- df %>%
  group_by(state, electionDate) %>%
  mutate(i = cur_group_id(),
         s = match(state, state_abb)) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id(),
         outcome = finalTwoPartyVSDemocratic / 100,
         n = floor((republican + democratic)/100 * numberOfRespondents),
         y = floor(democratic/100 * numberOfRespondents)) %>%
  left_join(us_regions, by = c("state" = "State Code")) %>%
  group_by(Region) %>%
  mutate(r = cur_group_id()) %>%
  group_by(Division) %>%
  mutate(d = cur_group_id()) %>%
  group_by(Region, year) %>%
  mutate(rt = cur_group_id()) %>%
  group_by(Division, year) %>%
  mutate(dt = cur_group_id()) %>%
  ungroup() %>%
  left_join(data.frame(x = 1:300,
                       t = sort(rep(1:6, 50)),
                       s = rep(1:50, 6)))
results <- read_csv("data/us_background/potus_results_76_20.csv")
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
results_2020 <- results %>%
  filter(year == 2020) %>%
  mutate(finalTwoPartyVSRepublican = rep/(dem + rep) * 100,
         finalTwoPartyVSDemocratic = dem/(dem + rep) * 100) %>%
  dplyr::select(-year) %>%
  left_join(df %>% distinct(state, State), by = c("state_po" = "state"))
states_2020_ordered_lower <- results_2020 %>% filter(!is.na(State)) %>%
  arrange(finalTwoPartyVSDemocratic) %>% pull(State)
state_abb_full <- results_2020 %>%
  pull(State)
turnout <- read_csv("data/us_background/potus_turnout.csv")
###############################################################################


beta <- fit$summary("beta_error", prob_gt_0 = ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(year = seq(2000, 2020, 4)) %>%
  dplyr::select(-variable, year) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`)

alpha <- fit$draws("alpha_error") %>%
  posterior::as_draws_df() %>%
  pull(alpha_error)
beta <- fit$draws("beta_error") %>%
  posterior::as_draws_df() %>%
  select(contains("beta_error")) %>%
  as.matrix()


pred6 <- alpha + t(t(beta[,6])) %*% t(seq(0, 1, 0.1))

pred6 %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "rep",
               values_to = "val",
               names_prefix = "V") %>%
  mutate(rep = seq(0, 1, 0.1)[as.integer(rep)]) %>%
  group_by(rep) %>%
  summarize(mean = mean(val))












