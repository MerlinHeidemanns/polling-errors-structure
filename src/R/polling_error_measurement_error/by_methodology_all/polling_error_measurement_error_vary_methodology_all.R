library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
df <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state != "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())
df_pres <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state == "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id())

methods <- sort(unique(c(df$methodology, df_pres$methodology)))

df <- df %>%
  mutate(m = as.integer(factor(methodology, levels = methods)))
df_pres <- df_pres %>%
  mutate(m = as.integer(factor(methodology, levels = methods)))

n_polls <- df %>%
  group_by(methodology) %>%
  summarize(n = n()) %>%
  mutate(methodology = as.integer(factor(methodology, levels = methods))) %>%
  arrange(methodology)


results <- read_csv("dta/potus_results_76_20.csv")
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
###############################################################################
state_abb <- df %>%
  pull(state) %>%
  c(., "DC") %>%
  unique() %>%
  sort()

index <- data.frame(x = 1:306,
                       t = sort(rep(1:6, 51)),
                       s = rep(1:51, 6)) %>%
  mutate(state = state_abb[s])


df <- df %>%
  mutate(outcome = finalTwoPartyVSDemocratic,
         n = republican + democratic,
         y = democratic) %>%
  left_join(index, by = c("state" = "state",
                          't' = 't'))



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
  M = length(methods),
  method_state = df %>% pull(m),
  method_national = df_pres %>% pull(m),
  t_state = df %>% pull(t),
  t_national = df_pres %>% pull(t),
  x = df %>% pull(x),
  y_state = df %>% pull(y),
  n_state = df %>% pull(n),
  y_national = df_pres %>% pull(democratic),
  n_national = df_pres %>% pull(numberOfRespondents),
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
#fit$save_object(file = "data/us_input/polling_error/polling_error_additive_national.RDS")
lambda <- fit$draws("lambda") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "m",
               values_to = "draws",
               names_pattern = "(\\d+)") %>%
  mutate(method = methods[as.integer(m)],
         n_polls = n_polls$n[as.integer(m)],
         draws = draws,
         method_n = paste(method," - ", n_polls, sep = "")) %>%
  filter(!is.na(method)) %>%
  group_by(method_n) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.1),
    q90 = quantile(draws, 0.9)
  )
write_csv(lambda, file = "dta/model_output/measurement_error/lambda_by_methodology_all.Rds")

alpha <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_all_alpha.Rds")
ggplot(alpha, aes(x = method_n, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
  theme_light() +
  labs(y = "1 - alpha ('share of noise')",
       x = "Method") +
  coord_flip()



## State error
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
write_csv(state_error, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_all_state_error.Rds")
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_all_state_error.Rds")
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









