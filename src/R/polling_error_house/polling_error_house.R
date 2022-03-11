################################################################################
## US House elections
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
library(boot)
################################################################################
## Load data
df <- read_csv("dta/clean_data/polls_house_538.csv")
results <- read_csv("dta/potus_results_76_20.csv")
################################################################################
## Prepare
df_house <- df %>%
  group_by(race_id) %>%
  mutate(idx_e = as.integer(cur_group_id())) %>%
  ungroup() %>%
  select(idx_e, dem_respondents,
         two_party_respondents,
         finalTwoPartyVSDemocratic,
         finalTwoPartyVSRepublican,
         state,
         year) %>%
  mutate(last_general = (year - 1996 - 1)%/%4 * 4 + 1996) %>%
  filter(!is.na(two_party_respondents),
         !is.na(finalTwoPartyVSDemocratic)) %>%
  left_join(results, by = c("last_general" = "year",
                            "state" = "state_po")) %>%
  mutate(previous_dem_share = dem/(dem + rep),
         idx_t = as.integer(as.factor(year))) %>%
  arrange(state, year, finalTwoPartyVSDemocratic)


outcomes <- df_house %>%
  distinct(idx_e, idx_t,finalTwoPartyVSDemocratic) %>%
  arrange(idx_e)
################################################################################
## Model
m <- file.path("src/stan/polling_error_sen", "polling_error_sen.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df_house),
  E = outcomes %>% distinct(idx_e) %>% nrow(),
  T = outcomes %>% distinct(idx_t) %>% nrow(),
  idx_e = df_house %>% pull(idx_e),
  idx_t = df_house %>% pull(idx_t),
  idx_te = outcomes %>% pull(idx_t),
  y = df_house %>% pull(dem_respondents),
  n = df_house %>% pull(two_party_respondents),
  outcome = outcomes %>% pull(finalTwoPartyVSDemocratic)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)
################################################################################
## Transform output for plotting
epsilon <- fit$draws(c("epsilon")) %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "idx_e",
               names_pattern = "([\\d]+)",
               values_to = "epsilon")

house_error <- epsilon %>%
  mutate(idx_e = as.integer(idx_e)) %>%
  filter(!is.na(idx_e)) %>%
  group_by(idx_e) %>%
  left_join(df_house %>%
              distinct(idx_e,
                       state,
                       year,
                       finalTwoPartyVSDemocratic, previous_dem_share)) %>%
  mutate(year5 = ifelse(year <= 2000, "-2000",
                        ifelse(year <= 2004, "2001-2004",
                               ifelse(year <= 2008, "2005-2008",
                                      ifelse(year <= 2012, "2009-2012",
                                             ifelse(year <= 2016, "2013-2016",
                                                    ifelse(year <= 2020, "2017-2020",
                                                           NA)))))),
         presidential = ifelse((year %% 4) == 0, "presidential",
                               ifelse((year %% 2) == 0, "midterm",
                                      "neither")),
         evenyear = ifelse(year %% 2 == 1, year - 1, year)) %>%
  group_by(idx_e, year5, year, evenyear, presidential,
           finalTwoPartyVSDemocratic, previous_dem_share) %>%
  summarize(
    q50_epsilon = quantile(epsilon, 0.5),
    q25_epsilon = quantile(epsilon, 0.25),
    q75_epsilon = quantile(epsilon, 0.75),
    q10_epsilon = quantile(epsilon, 0.10),
    q90_epsilon = quantile(epsilon, 0.90)
  ) %>%
  filter((q50_epsilon < 0.12), (q50_epsilon > -0.12)) %>%
  arrange(year) %>%
  mutate(evenyear = as.factor(evenyear),
         year = as.factor(year))
################################################################################
write_csv(house_error,
          file = "dta/model_output/measurement_error/house_races_yhat_epsilon.Rds")
################################################################################