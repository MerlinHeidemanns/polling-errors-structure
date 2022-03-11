###############################################################################
library(boot)
library(cmdstanr)
library(tidyverse)
library(lubridate)
methods <- c("IVR", "Live Phone", "Online")
df <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state != "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)))
df_pres <- read_csv("dta/clean_data/polls_538_00_20.csv") %>%
  filter(state == "US") %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id()) %>%
  filter(methodology %in% methods) %>%
  mutate(m = as.integer(factor(methodology, levels = methods)))

results <- read_csv("dta/clean_data/potus_results_76_20.csv")
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
  mutate(year = seq(2000, 2020, 4)[t],
         state = state_abb[s] )

df <- df %>%
  mutate(outcome = finalTwoPartyVSDemocratic,
         n = republican + democratic,
         y = democratic) %>%
  left_join(index, by = c("state" = "state",
                          "year" = "year",
                          't' = 't'))

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
  voteshare = c(t(voteshare))
}


out_epsilon <- data.frame()
for (jj in 1:length(methods)){
  df_subset <- df %>%
    filter(m == jj)
  data_list <- list(
    N_state = nrow(df_subset),
    T = df_subset %>% pull(t) %>% max(),
    S = df_subset %>% pull(s) %>% max(),
    s = df_subset %>% pull(s),
    t_state = df_subset %>% pull(t),
    x = df_subset %>% pull(x),
    y_state = df_subset %>% pull(y),
    n_state = df_subset %>% pull(n),
    outcome_state = c(voteshare)
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
    ) %>%
    mutate(method = methods[jj]) %>%
    mutate(t_lagged = t - 4) %>%
    left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
    mutate(dem_share_past = dem/(dem + rep)) %>%
    dplyr::select(-dem, -rep, -totalvotes, -other, -state) %>%
    left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
    mutate(dem_share_current = dem/(dem + rep)) %>%
    dplyr::select(-dem, -rep, -totalvotes, -other, -state)

  out_epsilon <- bind_rows(
    out_epsilon,
    epsilon
  )
}

write_csv(out_epsilon, "dta/model_output/epsilon_mode.csv")










