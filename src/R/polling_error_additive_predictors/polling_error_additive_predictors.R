library(tidyverse)
library(cmdstanr)
library(boot)
## Data
# Region indicators
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
# Polls
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()
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
  dplyr::select(-year)
states_2020_ordered_lower <- results_2020 %>% filter(!is.na(State)) %>%
  arrange(finalTwoPartyVSDemocratic) %>% pull(State)
state_abb_full <- results_2020 %>%
  pull(State)
turnout <- read_csv("data/us_background/potus_turnout.csv") %>%
  mutate(s = match(state_po, state_abb),
         t = (year - 2000)/4 + 1) %>%
  left_join(results, by = c("year" = "year",
                            "state_full_lower" = "State",
                            "state_po" = "state_po"))
###############################################################################
## Model for scales
m <- file.path("code/stan/obs/input/polling_error/polling_error_additive_predictors",
               "polling_error_additive_predictors_same_model.stan")
mod <- cmdstan_model(m)

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

indexes <- indexes %>%
  left_join(turnout)

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
  corr_t = indexes %>% pull(t),
  Z = indexes %>%
    mutate(rep_share = rep/(dem + rep)) %>%
    pull(rep_share),
  outcome_ts = indexes %>%
    mutate(turnout = vep_highest_office/100,
           dem_share = dem/(dem + rep)) %>%
    pull(dem_share)
)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 1000,
  iter_warmup = 1000,
  refresh = 500
)
fit$save_object(file = "data/us_input/polling_error/polling_error_additive_predictors.RDS")




###############################################################################
## Correlations
omega <- fit$summary("Omega") %>%
  mutate(state2 = state_abb_full[as.integer(str_match(variable,",(\\d+)")[,2])],
         state1 = state_abb_full[as.integer(str_match(variable,"(\\d+).")[,2])]) %>%
  dplyr::select(mean, state1, state2)
omega_corr <- omega %>%
  pivot_wider(id_cols = state1,
              names_from = state2,
              values_from = mean) %>%
  dplyr::select(-state1) %>%
  as.matrix()
rownames(omega_corr) <- colnames(omega_corr)
omega_corr <- round(omega_corr, 3)
ggcorrplot(omega_corr)
ggsave("plots/input/polling_error/v5_correlations.jpeg",
       ggcorrplot(omega_corr[states_2020_ordered_lower, states_2020_ordered_lower]),
       width = 10, height = 10)


###############################################################################
mu_corr <- fit$summary("mu_matrix", "mean") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])]) %>%
  dplyr::select(mean, s, t) %>%
  pivot_wider(id_cols = t,
              names_from = s,
              values_from = mean) %>%
  dplyr::select(-t) %>%
  as.matrix() %>%
  cor()
ggcorrplot::ggcorrplot(mu_corr[states_2020_ordered[-51], states_2020_ordered[-51]])

ggcorrplot(mu_corr,hc.order = TRUE)

xi <- fit$summary("xi", "mean") %>%
  mutate(x = 1:300) %>%
  left_join(indexes) %>%
  mutate(s = state_abb_full[s]) %>%
  dplyr::select(mean, t, s)  %>%
  pivot_wider(id_cols = t,
              names_from = s,
              values_from = mean) %>%
  dplyr::select(-t) %>%
  as.matrix() %>%
  cor()
ggcorrplot(xi[states_2020_ordered_lower, states_2020_ordered_lower])



rownames(mu) <- colnames(mu) <- results_2020 %>%
  filter(state_po != "DC") %>%
  pull(State)
mu <- round(mu, 3)
ggcorrplot(mu[states_2020_ordered_lower, states_2020_ordered_lower])
ggsave("plots/input/polling_error/v5_correlations_implied.jpeg",
       ggcorrplot(omega_corr[states_2020_ordered_lower, states_2020_ordered_lower]),
       width = 10, height = 10)








###############################################################################
corr <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(draw = 1:n()) %>%
  filter(draw < 500) %>%
  pivot_longer(c(-draw),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(dem_share = dem/(dem + rep),
         val = 100 * (inv.logit(dem_share + val) - inv.logit(dem_share))) %>%
  dplyr::select(draw, val, s, t) %>%
  filter(t == 2016) %>%
  pivot_wider(id_cols = draw,
              values_from = val,
              names_from = s)
ggcorrplot(cor(corr)[states_2020_ordered[-51], states_2020_ordered[-51]], hc.order = TRUE)


mu_corr <- fit$summary("mu_matrix", "mean") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb_full[as.integer(str_match(variable,"(\\d+).")[,2])]) %>%
  dplyr::select(mean, s, t) %>%
  pivot_wider(id_cols = t,
              names_from = s,
              values_from = mean) %>%
  dplyr::select(-t) %>%
  as.matrix() %>%
  cor()
ggcorrplot(mu_corr[states_2020_ordered_lower, states_2020_ordered_lower])


