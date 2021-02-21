library(boot)
library(cmdstanr)
library(tidyverse)
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
results <- read_csv("data/us_background/potus_results_76_20.csv") %>%
  left_join(df %>% distinct(state, State), by = c("state_po" = "state"))
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
states_2020_ordered_full <- results_2020 %>%
  arrange(dem) %>%
  filter(!is.na(State)) %>%
  pull(State)
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
###############################################################################
## Model for scales
m <- file.path("code/stan/obs/input/polling_error/polling_error_additive_predict",
               "polling_error_additive_predict.stan")
mod <- cmdstan_model(m)
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()


prediction_df <- data.frame()
for (miss_t in seq(2000, 2020, 4)){
  print(miss_t)
  df_subset <- df %>%
    filter(year != miss_t) %>%
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

  rt_index <- df_subset %>% distinct(r, t, rt) %>% arrange(r, t)
  dt_index <- df_subset %>% distinct(d, t, dt) %>% arrange(d, t)
  r_index <- df_subset %>% distinct(s, r) %>% arrange(s) %>% pull(r)
  d_index <- df_subset %>% distinct(s, d) %>% arrange(s) %>% pull(d)
  indexes <- data.frame(x = 1:50,
                        s = 1:50
  ) %>%
    mutate(r = r_index[s],
           d = d_index[s])

  data_list <- list(
    N = nrow(df_subset),
    R = df_subset %>% pull(r) %>% max(),
    D = df_subset %>% pull(d) %>% max(),
    T = df_subset %>% pull(t) %>% max(),
    S = df_subset %>% pull(s) %>% max(),
    s = df_subset %>% pull(s),
    rt = df_subset %>% pull(rt),
    dt = df_subset %>% pull(dt),
    t = df_subset %>% pull(t),
    x = df_subset %>% pull(x),
    r = df_subset %>% pull(r),
    d = df_subset %>% pull(d),
    y = df_subset %>% pull(y),
    n = df_subset %>% pull(n),
    outcome = df_subset %>% pull(outcome),
    pred_r = indexes %>% pull(r),
    pred_d = indexes %>% pull(d)
  )
  fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 0,
    init = 0.2
  )

  # Clean prediction
  prediction_loo <- fit$draws("pred") %>%
    posterior::as_draws_df() %>%
    mutate(jj = 1:n()) %>%
    filter(jj <= 1000) %>%
    pivot_longer(everything(),
                 names_to = "s",
                 values_to = "val") %>%
    mutate(missing = miss_t)
  prediction_df <- bind_rows(prediction_df, prediction_loo)
}

prediction_df <- prediction_df %>%
  mutate(s = str_match(s,"(\\d+)")[,2]) %>%
  filter(!is.na(s))
## save
write_rds(prediction_df, file = "data/us_input/polling_error/polling_error_additive_predict_loo.RDS")

## Full
m <- file.path("code/stan/obs/input/polling_error", "polling_error_v5_additive.stan")
mod <- cmdstan_model(m)
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

rt_index <- df %>% distinct(r, t, rt) %>% arrange(r, t)
dt_index <- df %>% distinct(d, t, dt) %>% arrange(d, t)
r_index <- df %>% distinct(s, r) %>% arrange(s) %>% pull(r)
d_index <- df %>% distinct(s, d) %>% arrange(s) %>% pull(d)
indexes <- data.frame(x = 1:300,
                      t = sort(rep(1:6, 50)),
                      s = rep(1:50, 6)
) %>%
  mutate(r = r_index[s],
         d = d_index[s],
         dt = NA,
         rt = NA)
for (i in 1:nrow(indexes)){
  indexes$rt[i] <- rt_index %>% filter(r == indexes$r[i],
                                       t == indexes$t[i]) %>%
    pull(rt)
  indexes$dt[i] <- dt_index %>% filter(d == indexes$d[i],
                                       t == indexes$t[i]) %>%
    pull(dt)
}

data_list <- list(
  N = nrow(df),
  R = df %>% pull(r) %>% max(),
  D = df %>% pull(d) %>% max(),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  rt = df %>% pull(rt),
  dt = df %>% pull(dt),
  t = df %>% pull(t),
  x = df %>% pull(x),
  r = df %>% pull(r),
  d = df %>% pull(d),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
  corr_s = indexes %>% pull(s),
  corr_r = indexes %>% pull(r),
  corr_d = indexes %>% pull(d),
  corr_x = indexes %>% pull(x),
  corr_dt = indexes %>% pull(dt),
  corr_rt = indexes %>% pull(rt),
  corr_t = indexes %>% pull(t)
)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)
fit$save_object(file = "data/us_input/polling_error/polling_error_additive.RDS")

full_est <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(jj = 1:n()) %>%
  filter(jj <= 1000) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = as.integer(str_match(variable,"(\\d+).")[,2]),
         t = 2000 + (t - 1) * 4) %>%
  rename(full_dist = val) %>%
  select(-variable) %>%
  filter(!is.na(s)) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())

### Compare
loo_pred <- read_rds(file = "data/us_input/polling_error/polling_error_additive_predict_loo.RDS")
loo_pred <- loo_pred %>%
  as_tibble() %>%
  mutate(s = as.integer(s)) %>%
  rename(t = missing) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())

full_est <- full_est %>%
  left_join(loo_pred)

plt <- ggplot(full_est %>% filter(s <= 10, draw <= 500),
       aes(x = full_dist, y = val)) +
  geom_point(size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), size = 0.5) +
  facet_grid(state_abb[s]~t) +
  theme_light() +
  labs(x = "Full model",
       y = "Loo prediction")
ggsave("plots/input/polling_error/polling_error_additive_predict/prediction_hist_states.png",
       height = 6, width = 10)

plt <- ggplot(full_est,
       aes(x = 100 * (inv.logit(full_dist) - 0.5) - (inv.logit(val) - 0.5))) +
  geom_histogram(size = 0.5, bins = 100) +
  geom_vline(aes(xintercept = 0), size = 0.5) +
  facet_grid(t~.) +
  theme_light() +
  labs(x = "Full estimate - out of sample prediction from loo model",
       caption = "Positive: Underestimate
                  Negative: Overestimate")
ggsave("plots/input/polling_error/polling_error_additive_predict/prediction_diff_hist_year.png",
       height = 6, width = 10)

diff_sum <- full_est %>%
  mutate(diff = 100 * (inv.logit(full_dist) - 0.5) - (inv.logit(val) - 0.5),
         s = state_abb[s],
         s = factor(s, levels = states_2020_ordered)) %>%
  group_by(s, t) %>%
  summarize(
    q50 = quantile(diff, 0.5),
    q25 = quantile(diff, 0.25),
    q75 = quantile(diff, 0.75),
    q10 = quantile(diff, 0.10),
    q90 = quantile(diff, 0.90)
  )

plt <- ggplot(data = diff_sum, aes(x = s, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  facet_grid(.~t) +
  coord_flip() +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Polling error estimate error (percentage points)",
       caption = "Positive: Underestimate
                  Negative: Overestimate")
ggsave("plots/input/polling_error/polling_error_additive_predict/prediction_diff_lines_year_states.png",
       height = 6, width = 10)





