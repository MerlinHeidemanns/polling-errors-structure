## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
## Load data
years <- c(2008, 2012, 2016, 2020)
df <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(pollName == "YouGov", year %in% years) %>%
  filter(state != "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())
df_pres <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(pollName == "YouGov", year %in% years) %>%
  filter(state == "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())
results <- read_csv("dta/clean_data/potus_results_76_20.csv")
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
us_regions <- read_csv("dta/clean_data/us census bureau regions and divisions.csv")
###############################################################################
state_abb <- df %>%
  pull(state) %>%
  c(., "DC") %>%
  unique() %>%
  sort()

index <- data.frame()
for (i in seq(2008, 2020, 4)){
  index <- bind_rows(index, us_regions %>%
                       mutate(year = i))
}
index <- index %>%
  mutate(s = match(`State Code`, state_abb)) %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  left_join(data.frame(x = 1:204,
                       t = sort(rep(1:4, 51)),
                       s = rep(1:51, 4)))
df <- df %>%
  left_join(index, by = c("state" = "State Code",
                          't' = 't'))
## turnout weights
turnout_weights <- results %>%
  filter(year > 2004) %>%
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
  filter(year > 2004) %>%
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
  y_state = df %>% pull(democratic),
  n_state = df %>% pull(numberOfRespondents),
  y_national = df_pres %>% pull(democratic),
  n_national = df_pres %>% pull(numberOfRespondents),
  outcome_state = voteshare,
  outcome_national = national_voteshare,
  turnout_weights = turnout_weights
)
## Model for scales
m <- file.path("src/stan/polling_error_measurement_error",
               "polling_error_no_adjustment_by_year.stan")
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
## Polling error
state_error <- fit$draws("epsilon") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  mutate(dem_share_past = dem/(dem + rep)) %>%
  dplyr::select(-dem, -rep, -totalvotes, -other, -state) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  mutate(dem_share_current = dem/(dem + rep)) %>%
  group_by(t, s, dem_share_past) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9)
  )
write_csv(state_error, file = "data/model_output/model_polls/components/polling_error_no_adjustment/by_year_state_error_yougov.Rds")


#### Plot for previous voteshare against polling error
## Load fit to generate plot
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_no_adjustment/by_year_state_error_yougov.Rds")

## Republican voteshare in previous election against polling error
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = dem_share_past, y = q50)) +
  #geom_point(size = 0.5) +
  geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Democratic voteshare (previous election)",
       y = "Polling error (percentage points)") +
  theme_light() +
  facet_wrap(t ~.)






## Mangle output to generate prediction to plot prediction against error


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

write_csv(state_error, file = "dta/model_output/measurement_error/by_year_state_error_and_yhat_yougov.Rds")





#### Plot for prediction against polling error
## Load fit to generate plot
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_no_adjustment/by_year_state_error_and_yhat_yougov.Rds")

## Republican voteshare in previous election against polling error
#### Labels
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = q50_yhat, y = q50_epsilon)) +
  #geom_point(size = 0.5) +
  geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Average prediction of Democratic two party vote share",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light() +
  facet_wrap(t ~.) +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)

#### Dots
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = q50_yhat, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Average prediction of Democratic two party vote share",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light() +
  facet_wrap(t ~.) +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)







