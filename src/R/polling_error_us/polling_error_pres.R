###############################################################################
## Polling error without adjustment
###############################################################################
## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
###############################################################################
## Load data
df <- read_csv("dta/clean_data/polls_pres_dataset_00_20.csv")
df_pres <- read_csv("dta/clean_data/polls_pres_national_dataset_00_20.csv") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())
results <- read_csv("dta/clean_data/potus_results_76_20.csv")
us_regions <- read_csv("dta/clean_data/us census bureau regions and divisions.csv")
###############################################################################
## Vectors
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)

state_abb <- df %>%
  pull(state) %>%
  c(., "DC") %>%
  unique() %>%
  sort()

index <- data.frame()
for (i in seq(2000, 2020, 4)){
  index <- bind_rows(index, us_regions %>%
                       mutate(year = i))
}
index <- index %>%
  mutate(s = match(`State Code`, state_abb)) %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  left_join(data.frame(x = 1:306,
                       t = sort(rep(1:6, 51)),
                       s = rep(1:51, 6)))
###############################################################################
## Mangling
df <- df %>%
  mutate(outcome = finalTwoPartyVSDemocratic / 100,
         n = floor((republican + democratic)/100 * numberOfRespondents),
         y = floor(democratic/100 * numberOfRespondents)) %>%
  left_join(index, by = c("state" = "State Code",
                          "year" = "year"))
## turnout weights
turnout_weights <- results %>%
  filter(year > 1996) %>%
  arrange(year, state_po) %>%
  group_by(year) %>%
  mutate(weight = totalvotes/sum(totalvotes)) %>%
  dplyr::select(weight, year, state_po) %>%
  pivot_wider(id_cols = year,
              names_from = state_po,
              values_from = weight) %>%
  ungroup() %>%
  dplyr::select(-year) %>%
  as.matrix()
## election results
voteshare <- results %>%
  filter(year > 1996) %>%
  arrange(year, state_po) %>%
  group_by(year) %>%
  mutate(dem_voteshare = dem/(dem + rep)) %>%
  dplyr::select(dem_voteshare, year, state_po) %>%
  pivot_wider(id_cols = year,
              names_from = state_po,
              values_from = dem_voteshare) %>%
  ungroup() %>%
  dplyr::select(-year) %>%
  as.matrix()
## confirm that the ordering is correct
if (all(colnames(voteshare) == state_abb) == FALSE){
  stop("Columns not correctly ordered.")
} else {
  national_voteshare = rep(NA, dim(voteshare)[1])
  for (i in 1:length(national_voteshare)){
    national_voteshare[i] = sum(voteshare[i, ] * turnout_weights[i, ])
  }
  voteshare = c(t(voteshare))
}
###############################################################################
## Prepare datalist for model
data_list <- list(
  N_state = nrow(df),
  N_national = nrow(df_pres),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  t_state = df %>% pull(t),
  t_national = df_pres %>% pull(t),
  x = df %>% pull(x),
  y_state = df %>% pull(y),
  n_state = df %>% pull(n),
  y_national = df_pres %>% pull(dem_respondents),
  n_national = df_pres %>% pull(two_party_respondents),
  outcome_state = voteshare,
  outcome_national = national_voteshare,
  turnout_weights = turnout_weights
)
## Model for scales
m <- file.path("src/stan/polling_error_pres",
               "polling_error_no_adjustment_by_year_only_states.stan")
mod <- cmdstan_model(m)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 2000,
  refresh = 500,
  init = 0.2
)

###############################################################################
## Output mangling

epsilon <- fit$draws("epsilon") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  group_by(t, s) %>%
  summarize(
    q50_epsilon = quantile(val, 0.5),
    q25_epsilon = quantile(val, 0.25),
    q75_epsilon = quantile(val, 0.75),
    q10_epsilon = quantile(val, 0.1),
    q90_epsilon = quantile(val, 0.9)
  )

yhat <- fit$draws("y_hat") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  group_by(t, s) %>%
  summarize(
    q50_yhat = quantile(val, 0.5),
    q25_yhat = quantile(val, 0.25),
    q75_yhat = quantile(val, 0.75),
    q10_yhat = quantile(val, 0.1),
    q90_yhat = quantile(val, 0.9)
  )

state_error <- left_join(yhat, epsilon, by = c("s", "t")) %>%
  mutate(t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  mutate(dem_share_past = dem/(dem + rep)) %>%
  dplyr::select(-dem, -rep, -totalvotes, -other, -state) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  mutate(dem_share_current = dem/(dem + rep)) %>%
  dplyr::select(-dem, -rep, -totalvotes, -other, -state)

write_csv(state_error, file = "dta/model_output/measurement_error/by_year_state_error_and_yhat.Rds")
###############################################################################
## Resample observations based on observed election outcome
polling_errors <- data.frame(
  s = rep(1:51, 6),
  t = sort(rep(1:6, 51))
) %>%
  mutate(error = rnorm(n(), 0, 0.12))
df_fake <- df %>%
  filter(!is.na(n),
         !is.na(finalTwoPartyVSDemocratic)) %>%
  left_join(polling_errors) %>%
  mutate(y_fake = rbinom(n(), n, inv.logit(logit(finalTwoPartyVSDemocratic/100) +
                           error)))


data_list <- list(
  N_state = nrow(df_fake),
  T = df_fake %>% pull(t) %>% max(),
  S = df_fake %>% pull(s) %>% max(),
  s = df_fake %>% pull(s),
  t_state = df_fake %>% pull(t),
  x = df_fake %>% pull(x),
  y_state = df_fake %>% pull(y_fake),
  n_state = df_fake %>% pull(n),
  outcome_state = voteshare
)
## Model for scales
m <- file.path("src/stan/polling_error_pres",
               "polling_error_no_adjustment_by_year_only_states.stan")
mod <- cmdstan_model(m)
fit_fake <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 2000,
  refresh = 500,
  init = 0.2
)


epsilon <- fit_fake$draws("epsilon") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  group_by(t, s) %>%
  summarize(
    q50_epsilon = quantile(val, 0.5),
    q25_epsilon = quantile(val, 0.25),
    q75_epsilon = quantile(val, 0.75),
    q10_epsilon = quantile(val, 0.1),
    q90_epsilon = quantile(val, 0.9)
  )

yhat <- fit_fake$draws("y_hat") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  group_by(t, s) %>%
  summarize(
    q50_yhat = quantile(val, 0.5),
    q25_yhat = quantile(val, 0.25),
    q75_yhat = quantile(val, 0.75),
    q10_yhat = quantile(val, 0.1),
    q90_yhat = quantile(val, 0.9)
  )

state_error <- left_join(yhat, epsilon, by = c("s", "t")) %>%
  mutate(t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  mutate(dem_share_past = dem/(dem + rep)) %>%
  dplyr::select(-dem, -rep, -totalvotes, -other, -state) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  mutate(dem_share_current = dem/(dem + rep)) %>%
  dplyr::select(-dem, -rep, -totalvotes, -other, -state)

write_csv(state_error, file = "dta/model_output/measurement_error/by_year_state_error_and_yhat_fake_resampled.Rds")






