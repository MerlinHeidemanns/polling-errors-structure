## Libraries
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
## Data
df <- read_csv("dta/clean_data/polls_pres_dataset_00_20.csv")
df_pres <- read_csv("dta/clean_data/polls_pres_national_dataset_00_20.csv") %>%
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


df <- df %>%
  mutate(outcome = finalTwoPartyVSDemocratic / 100,
         n = floor((republican + democratic)/100 * numberOfRespondents),
         y = floor(democratic/100 * numberOfRespondents)) %>%
  left_join(index, by = c("state" = "State Code",
                          "year" = "year"))


turnout_weights <- results %>%
  filter(year >= 1996,
         year != 2020) %>%
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

df <- df %>%
  mutate(t_minus =  as.integer(mdy(electionDate) - mdy(endDate)))

df_pres <- df_pres %>%
  mutate(t_minus =  as.integer(mdy(electionDate) - mdy(endDate)))


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

data.frame(voteshare = voteshare,
           s = rep(seq(1, 51), 6),
           t = sort(rep(seq(1, 6), 51)))

data_list <- list(
  N_state = nrow(df),
  N_national = nrow(df_pres),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  t_state_before_election = df %>% pull(t_minus),
  t_national_before_election = df_pres %>% pull(t_minus),
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
# m <- file.path("code/stan/models/models_polls/components/polling_error_measurement_error",
#                "polling_error_measurement_error_by_day_before_election.stan")
# m <- file.path("code/stan/models/models_polls/components/polling_error_measurement_error",
#                "polling_error_measurement_error_by_day_before_election_year_intercept.stan")
# m <- file.path("code/stan/models/models_polls/components/polling_error_measurement_error",
#                "polling_error_measurement_error_by_day_before_election_individual.stan")
m <- file.path("src/stan/polling_error_measurement_error",
               "polling_error_measurement_error_by_day_before_election_coefficient.stan")
mod <- cmdstan_model(m)
data_list$seq <- 1:21
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  iter_sampling = 3000
)

lambda <- fit$summary("inv_logit_lambda", prob_gt = ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(t = 2000 + (as.integer(str_match(variable, ",(\\d+)")[,2]) - 1) * 4,
         d = as.integer(str_match(variable, "(\\d+),")[,2])) %>%
  dplyr::select(-variable) %>%
  pivot_longer(c(-t, -d),
               names_to = "q",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  pivot_wider(id_cols = c(t, d),
              names_from = q,
              names_prefix = "q",
              values_from = val)
plt <- ggplot(lambda, aes(x = d, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  theme_light() +
  facet_wrap(t~.) +
  scale_x_reverse(limits = c(21, 1),
                  breaks = c(20, 15, 10, 5, 1)) +
  labs(x = "Days until election",
       y = "Lambda")
ggsave("plt/polling_error_day_to_election/coefficient_lambda.png", plt, width = 10, height = 6)


write_csv(lambda, file = "dta/model_output/measurement_error/by_day_coefficient_lambda.Rds")

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
write_csv(state_error, file = "dta/model_output/measurement_error/by_day_coefficient_epsilon.Rds")
state_error <- read_csv(file = "dta/model_output/measurement_error/by_day_coefficient_epsilon.Rds")

## Republican voteshare in previous election against polling error
plt <- ggplot(data = state_error %>%
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

###############################################################################
## Random walk
m <- file.path("code/stan/models/models_polls/components/polling_error_measurement_error",
               "polling_error_measurement_error_by_day_before_election_individual.stan")
mod <- cmdstan_model(m)
data_list$seq <- 1:21
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  iter_sampling = 3000
)

alpha <- fit$summary("m1_inv_logit_alpha", prob_gt = ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(t = 2000 + (as.integer(str_match(variable, ",(\\d+)")[,2]) - 1) * 4,
         d = as.integer(str_match(variable, "(\\d+),")[,2])) %>%
  dplyr::select(-variable) %>%
  pivot_longer(c(-t, -d),
               names_to = "q",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  pivot_wider(id_cols = c(t, d),
              names_from = q,
              names_prefix = "q",
              values_from = val)
ggplot(alpha, aes(x = d, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin =q10, ymax = q90), alpha = 0.25) +
  theme_light() +
  facet_wrap(t~.) +
  scale_x_reverse()

write_csv(alpha, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_day_random_walk_model_alpha.Rds")

state_error <- fit$draws("mu_matrix") %>%
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
write_csv(state_error, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_day_random_walk_model_state_error.Rds")
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_day_random_walk_model_state_error.Rds")

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







