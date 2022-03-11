################################################################################
## Polling error with adjustment term
################################################################################
## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
################################################################################
## Load data
df <- read_csv("dta/clean_data/polls_pres_dataset_00_20.csv")
df_pres <- read_csv("dta/clean_data/polls_pres_national_dataset_00_20.csv") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())
results <- read_csv("dta/clean_data/potus_results_76_20.csv")
us_regions <- read_csv("dta/clean_data/us census bureau regions and divisions.csv")
################################################################################
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
## Prepare data
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
  N_state_past = nrow(df %>% filter(t < 5)),
  N_state_current = nrow(df %>% filter(t == 5)),
  T = df %>% filter(t == 5) %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s_past = df %>% filter(t < 5) %>% pull(s),
  s_current = df %>% filter(t == 5) %>% pull(s),
  t_state_past = df %>% filter(t < 5) %>% pull(t),
  t_state_current = df %>% filter(t == 5) %>% pull(t),
  x_current = df %>% filter(t == 5) %>% pull(x),
  x_past = df %>% filter(t < 5) %>% pull(x),
  y_state_past = df %>% filter(t < 5) %>% pull(y),
  n_state_past = df %>% filter(t < 5) %>% pull(n),
  y_state_current = df %>% filter(t == 5) %>% pull(y),
  n_state_current = df %>% filter(t == 5) %>% pull(n),
  outcome_state = voteshare[1:(51 * 5)]
)

## Model for scales
m <- file.path("src/stan/polling_error_adjustment_term_forecast",
               "polling_error_adjustment_term_forecast_v3.stan")
mod <- cmdstan_model(m)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)


################################################################################
## PPD
fit$summary("lambda")
## lambda
lambda <- fit$draws("lambda") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "t",
               values_to = "draws",
               names_pattern = "(\\d+)") %>%
  mutate(t = 2000 + (as.integer(t) - 1) * 4) %>%
  filter(!is.na(t)) %>%
  group_by(t) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.1),
    q90 = quantile(draws, 0.9)
  )
write_csv(lambda, file = "dta/model_output/measurement_error/lambda_by_year.Rds")
lambda <- read_csv(file = "dta/model_output/measurement_error/lambda_by_year.Rds")

ggplot(lambda, aes(x = as.factor(t), y = q50)) +
    geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  labs(y = "Lambda ('share of noise')",
       x = "Election")


state_error <- fit$draws("epsilon") %>%
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
  mutate(dem_share = dem/(dem + rep)) %>%
  group_by(t, s, dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9)
  )

## Democratic voteshare in previous election against polling error
ggplot(data = state_error %>%
         filter(s != "DC", t == 2020), aes(x = dem_share, y = q50)) +
  #geom_point(size = 0.5) +
  geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Democratic voteshare (previous election)",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light()

difference <- fit$draws(c("pred_new")) %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "state",
               values_to = "pred_new",
               names_pattern = "(\\d+)") %>%
  left_join(
    fit$draws(c("pred_base")) %>%
      posterior::as_draws_df() %>%
      mutate(iter = 1:n()) %>%
      pivot_longer(c(-iter),
                   names_to = "state",
                   values_to = "pred_base",
                   names_pattern = "(\\d+)"),
    by = c("state", "iter")
  ) %>%
  left_join(
    fit$draws("lambda_new") %>%
      posterior::as_draws_df() %>%
      mutate(iter = 1:n()) %>%
      select(lambda_new, iter)
  ) %>%
  mutate(state_po = state_abb[as.integer(state)]) %>%
  left_join(results %>%
              filter(year == 2020) %>%
              mutate(dem_share = dem/(dem + rep)) %>%
            select(dem_share, state_po)) %>%
  filter(state_po != "DC") %>%
  mutate(pred_new_error = dem_share - pred_new,
         pred_base_error = dem_share - pred_base)

percentage_change <- difference %>%
  group_by(iter) %>%
  summarize(error_new =
              mean(abs(pred_new_error),
                   ),
            error_base =
              mean(abs(pred_base_error))) %>%
  ungroup() %>%
  mutate(change = error_new / error_base) %>%
  pull(change)
polling_error <- read_csv("dta/model_output/de_polling_error.csv")

write_csv(difference, file = "dta/model_output/forecast_error_adjustment.csv")
ggplot(data = difference %>%
       group_by(iter, lambda_new) %>%
       summarize(error_new =
                   mean(abs(pred_new_error)),
                 error_base =
                   mean(abs(pred_base_error)))
         %>% filter(abs(error_base - error_new) < 0.05 )) +
  geom_point(aes(x = error_base,
                 y = error_new,
                 color = lambda_new)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_light() +
  coord_equal() +
  lims(x = c(0.015, 0.03), y = c(0.015, 0.03)) +
  labs(x = "MAE before adjustment",
       y = "MAE after adjustment",
       color = "Lambda")


difference_new <- difference %>%
  group_by(lambda_new) %>%
  summarize(q50_pred_new = quantile(abs(dem_share - pred_new), 0.5)) %>%
  filter(lambda_new > 0, lambda_new < 1)

difference_base <- difference %>%
  summarize(q50_pred_base = quantile(abs(dem_share - pred_base), 0.5),
            q25_pred_base = quantile(abs(dem_share - pred_base), 0.25),
            q75_pred_base = quantile(abs(dem_share - pred_base), 0.75)) %>%
  full_join(data.frame(lambda_new = seq(0, 0.4, length.out = 100)),
            by = character())


ggplot() +
  geom_line(data = difference_base, aes(x = lambda_new, y = q50_pred_base)) +
  geom_ribbon(data = difference_base,
              aes(x = lambda_new, ymin = q25_pred_base,
                  ymax = q75_pred_base), alpha = 0.3, fill = "blue") +
  geom_point(data = difference_new, aes(x = lambda_new, y = q50_pred_new), alpha = 0.3) +
  theme_light()



ggplot(data = difference, aes(x = mean_beta, y = mean_beta_new)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1))

difference %>%
  mutate(diff = mean_beta - mean_beta_new) %>%
  pull(diff) %>%
  summary()

################################################################################
## Mangle output to generate prediction to plot prediction against error
yhat <- fit$draws("y_hat") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(yhat = 100 * val) %>%
  group_by(t, s) %>%
  summarize(
    q50_yhat = quantile(yhat, 0.5),
    q25_yhat = quantile(yhat, 0.25),
    q75_yhat = quantile(yhat, 0.75),
    q10_yhat = quantile(yhat, 0.1),
    q90_yhat = quantile(yhat, 0.9)
  )
epsilon <- fit$draws("epsilon") %>%
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
  mutate(dem_share = dem/(dem + rep)) %>%
  group_by(t, s, dem_share) %>%
  summarize(
    q50_epsilon = quantile(val, 0.5),
    q25_epsilon = quantile(val, 0.25),
    q75_epsilon = quantile(val, 0.75),
    q10_epsilon = quantile(val, 0.1),
    q90_epsilon = quantile(val, 0.9)
  )
state_error <- left_join(yhat, epsilon)
write_csv(state_error, file = "dta/model_output/measurement_error/lambda_by_year_yhat_and_epsilon.Rds")

#### Plot for previous voteshare against polling error
## Load fit to generate plot
state_error <- read_csv(file = "dta/model_output/measurement_error/lambda_by_year_yhat_and_epsilon.Rds")

## Republican voteshare in previous election against polling error
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = q50_yhat, y = q50_epsilon)) +
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



#### Plot for prediction against polling error
## Load fit to generate plot
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_year_state_error_and_yhat.Rds")

## Republican voteshare in previous election against polling error
ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = q50_yhat, y = q50_zeta)) +
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







