library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
methods <- c("IVR", "Live Phone", "Online")
df <- read_csv("data/us_polls/polls_538_00_20.csv") %>%
  filter(state != "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)),
         mt = as.integer(paste(m, t, sep = "")))
df_pres <- read_csv("data/us_polls/polls_538_00_20.csv") %>%
  filter(state == "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)),
         mt = as.integer(paste(m, t, sep = "")))

mt_id_df <- data.frame(
  t = rep(seq(1, 6), 3),
  m = sort(rep(seq(1, 3), 6))
) %>%
  mutate(mt = as.integer(paste(m, t, sep = "")),
         method = methods[m],
         year = seq(2000, 2020, 4)[t])

mt_id <- mt_id_df %>% pull(mt)

df_pres <- df_pres %>%
  mutate(mt = as.integer(factor(mt, levels = mt_id)))
df <- df %>%
  mutate(mt = as.integer(factor(mt, levels = mt_id)))

mt_id_df <- mt_id_df %>%
  mutate(mt_factor = as.integer(as.factor(mt))) %>%
  left_join(data.frame(table(df$mt)) %>%
              rename(mt_factor = Var1, state_poll_count = Freq) %>%
              mutate(mt_factor = as.integer(as.character(mt_factor)))) %>%
  mutate(state_poll_count = ifelse(is.na(state_poll_count), 0, state_poll_count))


results <- read_csv("data/us_background/potus_results_76_20.csv")
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
  mutate(outcome = finalTwoPartyVSDemocratic,
         n = republican + democratic,
         y = democratic) %>%
  left_join(index, by = c("state" = "State Code",
                          "year" = "year",
                          't' = 't'))


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


data_list <- list(
  N_state = nrow(df),
  N_national = nrow(df_pres),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  M = 3,
  method_year_state = df %>% pull(mt),
  method_year_national = df_pres %>% pull(mt),
  t_state = df %>% pull(t),
  t_national = df_pres %>% pull(t),
  x = df %>% pull(x),
  y_state = df %>% pull(y),
  n_state = df %>% pull(n),
  y_national = df_pres %>% pull(democratic),
  n_national = df_pres %>% pull(numberOfRespondents),
  outcome_state = df %>% pull(outcome),
  outcome_national = df_pres %>% pull(finalTwoPartyVSDemocratic),
  turnout_weights = turnout_weights
)
## Model for scales
m <- file.path("code/stan/models/models_polls/components/polling_error_measurement_error",
               "polling_error_measurement_error_by_method_year_v2.stan")
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
alpha <- fit$draws("alpha") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "m",
               values_to = "draws",
               names_pattern = "(\\d+)") %>%
  mutate(method = mt_id_df$method[as.integer(m)],
         year = mt_id_df$year[as.integer(m)],
         state_polls = mt_id_df$state_poll_count[as.integer(m)],
         draws = 1 - draws) %>%
  filter(!is.na(method)) %>%
  group_by(method, year, state_polls) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.1),
    q90 = quantile(draws, 0.9)
  )
write_csv(alpha, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_year_alpha.Rds")
alpha <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_year_alpha.Rds")
ggplot(alpha, aes(x = method, y = q50)) +
  geom_point() +
  geom_text(aes(label = state_polls), size = 3, nudge_x = 0.3) +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
  theme_light() +
  facet_wrap(year~.) +
  labs(y = "1 - alpha ('share of noise')",
       x = "Method")
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
write_csv(state_error, file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_year_state_error.Rds")
state_error <- read_csv(file = "data/model_output/model_polls/components/polling_error_measurement_error/by_methodology_year_state_error.Rds")
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









