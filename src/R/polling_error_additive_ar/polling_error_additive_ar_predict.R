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
m <- file.path("code/stan/obs/input/polling_error/polling_error_additive_ar",
               "polling_error_additive_ar.stan")
mod <- cmdstan_model(m)
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()


prediction_df <- data.frame()
for (miss_t in seq(2008, 2020, 4)){
  print(miss_t)
  obs <- (miss_t - 2000)/4

  df_subset <- df %>%
    filter(year < miss_t) %>%
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
    ungroup()
  # This code adjusts for the edge case of Wyoming missing in 2000, 2004
  max_s <- df_subset %>%
    pull(s) %>%
    max()
  df_subset <- df_subset %>%
    left_join(data.frame(x = 1:(max_s * obs),
                         t = sort(rep(1:obs, max_s)),
                         s = rep(1:max_s, obs)))


  r_index <- df_subset %>% distinct(s, r) %>% arrange(s) %>% pull(r)
  d_index <- df_subset %>% distinct(s, d) %>% arrange(s) %>% pull(d)
  indexes <- data.frame(x = 1:max_s
  ) %>%
    mutate(r = r_index[x],
           d = d_index[x])

  data_list <- list(
    N = nrow(df_subset),
    R = df_subset %>% pull(r) %>% max(),
    D = df_subset %>% pull(d) %>% max(),
    T = df_subset %>% pull(t) %>% max(),
    S = df_subset %>% pull(s) %>% max(),
    rt = df_subset %>% pull(rt),
    dt = df_subset %>% pull(dt),
    t = df_subset %>% pull(t),
    x = df_subset %>% pull(x),
    y = df_subset %>% pull(y),
    n = df_subset %>% pull(n),
    outcome = df_subset %>% pull(outcome),
    pred_x = indexes %>% pull(x),
    pred_d = indexes %>% pull(d),
    pred_r = indexes %>% pull(r)
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
    mutate(missing = miss_t,
            s = as.integer(str_match(s,"(\\d+)")[,2])) %>%
    filter(!is.na(s))
  prediction_df <- bind_rows(prediction_df, prediction_loo)
}

## save
write_rds(prediction_df, file = "data/us_input/polling_error/polling_error_additive_ar_predict_loo.RDS")

## Full
fit <- read_rds(file = "data/us_input/polling_error/polling_error_abs_cov_mat.RDS")

full_est <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(jj = 1:n()) %>%
  filter(jj <= 1000) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (t - 1) * 4) %>%
  filter(t > 2004) %>%
  rename(full_dist = val) %>%
  select(-variable) %>%
  filter(!is.na(s)) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())

### Compare
loo_pred <- read_rds(file = "data/us_input/polling_error/polling_error_additive_ar_predict_loo.RDS")
loo_pred <- loo_pred %>%
  as_tibble() %>%
  mutate(s = state_abb[as.integer(s)]) %>%
  rename(t = missing) %>%
  group_by(s, t) %>%
  mutate(draw = 1:n())

full_est <- full_est %>%
  left_join(loo_pred) %>%
  mutate(full_dist = (inv.logit(full_dist) - 0.5) * 100,
         val = (inv.logit(val) - 0.5) * 100,)

plt <- ggplot(full_est %>% filter(s %in% state_abb[1:10], draw <= 500),
       aes(x = full_dist, y = val)) +
  geom_point(size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), size = 0.5) +
  facet_grid(s~t) +
  theme_light() +
  labs(x = "Full model",
       y = "Loo prediction")
ggsave("plots/input/polling_error/polling_error_additive_predict_ar/prediction_hist_states.png",
       height = 6, width = 10)

plt <- ggplot(full_est,
       aes(x = full_dist - val)) +
  geom_histogram(size = 0.5, bins = 100) +
  geom_vline(aes(xintercept = 0), size = 0.5) +
  facet_grid(t~.) +
  theme_light() +
  labs(x = "Full estimate - out of sample prediction from loo model",
       caption = "Positive: Underestimate
                  Negative: Overestimate")
ggsave("plots/input/polling_error/polling_error_additive_predict_ar/prediction_diff_hist_year.png",
       height = 6, width = 10)

diff_df <- full_est %>%
  mutate(diff = full_dist - val,
         s = factor(s, levels = states_2020_ordered)) %>%
  group_by(s, t) %>%
  summarize(
    q50 = quantile(diff, 0.5, na.rm = TRUE),
    q25 = quantile(diff, 0.25, na.rm = TRUE),
    q75 = quantile(diff, 0.75, na.rm = TRUE),
    q10 = quantile(diff, 0.10, na.rm = TRUE),
    q90 = quantile(diff, 0.90, na.rm = TRUE)
  )

plt <- ggplot(data = diff_df, aes(x = s, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  facet_grid(.~t) +
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = 2) +
  coord_flip() +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Polling error estimate error (percentage points)",
       caption = "Positive: Underestimate
                  Negative: Overestimate")
ggsave("plots/input/polling_error/polling_error_additive_predict_ar/prediction_diff_lines_year_states.png",
       plt, height = 6, width = 10)

## Calibration
within_interval <- function(x, x_rep, lower, upper){
  upper_q <- quantile(x_rep, upper)
  lower_q <- quantile(x_rep, lower)
  return(as.integer((x > lower_q) & (x < upper_q)))
}
quantile_df <- lapply(seq(0.01, 0.49, 0.01), function(x){
  df <- full_est %>%
    mutate(diff = full_dist - val) %>%
    filter(!is.na(diff)) %>%
    group_by(t, s) %>%
    summarize(q50 = within_interval(0, diff, 0.5 - x, 0.5 + x)) %>%
    pivot_longer(cols = c(q50),
                 names_to = "quantile",
                 values_to = "in_interval") %>%
    mutate(quantile = x * 2)
  return(df)
}) %>%
  do.call("bind_rows",.)

quantile_df_sum <- quantile_df %>%
  group_by(t, quantile) %>%
  summarize(share_in = mean(in_interval))

plt <- ggplot(data = quantile_df_sum, aes(x = quantile, y = share_in, color = as.factor(t))) +
  geom_line() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(x = "Ideal share",
       y = "Observed share",
       caption = "Diff: Predicted v estimated polling error
                  Over: Overfit
                  Below: Underfit",
       color = "Election") +
  theme_light()
ggsave("plots/input/polling_error/polling_error_additive_predict_ar/calibration.png",
       plt ,
       height = 6, width = 10)






