################################################################################
## Polling error and measurement error by methodology
################################################################################
## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
################################################################################
## Load and mangle data
results <- read_csv("dta/potus_results_76_20.csv")
state_abb <- results %>%
  pull(state_po) %>%
  unique() %>%
  sort()
index <- data.frame(x = 1:306,
           t = sort(rep(1:6, 51)),
           s = rep(1:51, 6))

methods <- c("IVR", "Live Phone", "Online")
df <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state != "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)),
         t = as.integer(factor(year, levels = seq(2000, 2020, 4))),
         s = as.integer(factor(state, levels = state_abb)),
         outcome = finalTwoPartyVSDemocratic,
         n = republican + democratic,
         y = democratic) %>%
  left_join(index)
df_pres <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state == "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)),
         t = as.integer(factor(year, levels = seq(2000, 2020, 4))),
         outcome = finalTwoPartyVSDemocratic,
         n = republican + democratic,
         y = democratic)

states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
###############################################################################

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
  mutate(state_po = factor(state_po, levels = state_abb)) %>%
  arrange(year, state_po) %>%
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
#
#
# ## Plotting
ggplot(df, aes(x = finalTwoPartyVSDemocratic,
               y = finalTwoPartyVSDemocratic - y/n,
               color = as.factor(methodology)
               )) +
  #geom_point(size = 0.8) +
  geom_text(aes(label = state), size = 2) +
  facet_wrap(year ~ .) +
  geom_smooth(method = "lm") +
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = 2) +
  geom_vline(aes(xintercept = 0.5), size = 0.5, linetype = 2)



data_list <- list(
  N_state = nrow(df),
  N_national = nrow(df_pres),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  M = length(methods),
  method_state = df %>% pull(m),
  method_national = df_pres %>% pull(m),
  t_state = df %>% pull(t),
  t_national = df_pres %>% pull(t),
  x = df %>% pull(x),
  y_state = df %>% pull(y),
  n_state = df %>% pull(n),
  y_national = df_pres %>% pull(y),
  n_national = df_pres %>% pull(n),
  outcome_state = voteshare,
  outcome_national = national_voteshare,
  turnout_weights = turnout_weights
)
## Model for scales
m <- file.path("src/stan/polling_error_measurement_error",
               "polling_error_measurement_error_by_method_v2.stan")
mod <- cmdstan_model(m)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)
lambda <- fit$draws("lambda") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "m",
               values_to = "draws",
               names_pattern = "(\\d+)") %>%
  mutate(method = methods[as.integer(m)]) %>%
  filter(!is.na(method)) %>%
  group_by(method) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.1),
    q90 = quantile(draws, 0.9)
  ) %>%
  arrange(q50) %>%
  mutate(method = factor(method, levels = method))
write_csv(lambda, file = "dta/model_output/measurement_error/lambda_by_methodology.Rds")


lambda <- lambda %>%
  arrange(q50) %>%
  mutate(method = factor(method, levels = method))

ggplot(lambda, aes(x = method, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
  theme_light() +
  lims(y = c(0, NA)) +
  labs(y = "Lambda ('share of noise')",
       x = "Method")











state_error <- fit$draws("zeta_matrix") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(dem_share = logit(dem/(dem + rep)),
         val = 100 * (inv.logit(dem_share + val) - inv.logit(dem_share))) %>%
  group_by(t, s, dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  )
write_csv(state_error, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_state_error.Rds")
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_state_error.Rds")
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = 1 - inv.logit(dem_share), y = q50)) +
  #geom_point(size = 0.5) +
  geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare (previous election)",
       y = "Polling error (%, favors Democrats)") +
  theme_light() +
  facet_wrap(t ~.)




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

yhat <- fit$draws("yhat") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (t - 1) * 4) %>%
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

state_error <- left_join(yhat, epsilon, by = c("s", "t"))

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








