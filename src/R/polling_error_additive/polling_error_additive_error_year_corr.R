library(tidyverse)
library(cmdstanr)
## Fit
fit <- readRDS("data/us_input/polling_error/polling_error_abs_cov_mat.RDS")
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
results <- read_csv("data/us_background/potus_results_76_20.csv")
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
###############################################################################
xi_hat <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(draw = 1:n()) %>%
  pivot_longer(c(-draw),
               names_to = "par",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(par,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(par,"(\\d+).")[,2])]) %>%
  select(-par) %>%
  filter(!is.na(s)) %>%
  pivot_wider(id_cols = c(draw, s),
              names_from = t,
              values_from = val)
cor_df <- data.frame()
for (i in 1:2000){
  print(i)
  tmp <- xi_hat %>%
    filter(draw == i) %>%
    select(-draw, -s) %>%
    as.matrix() %>%
    cor() %>%
    as_tibble() %>%
    rownames_to_column(var = "t1") %>%
    mutate(t1 = 2000 + (as.integer(t1) - 1) * 4) %>%
    pivot_longer(c(-t1),
                 names_to = "t2",
                 values_to = "corr") %>%
    mutate(t2 = 2000 + (as.integer(t2) - 1) * 4)
  cor_df <- bind_rows(cor_df, tmp)
}
cor_df_sum <- cor_df %>%
  group_by(t1, t2) %>%
  summarize(
    q50 = quantile(corr, 0.5),
    q25 = quantile(corr, 0.25),
    q75 = quantile(corr, 0.75),
    q10 = quantile(corr, 0.10),
    q90 = quantile(corr, 0.90),
  )

ggplot(cor_df %>% filter(t1 != t2), aes(x = corr)) +
  geom_histogram(bins = 60) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_grid(t1~t2) +
  theme_light() +
  labs(x = "rho",
       title = "Additive model") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())









