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
m <- file.path("code/stan/obs/input/polling_error/polling_error_additive_ar",
               "polling_error_additive_ar.stan")
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
  rt = df %>% pull(rt),
  dt = df %>% pull(dt),
  t = df %>% pull(t),
  x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
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
  iter_sampling = 1000,
  iter_warmup = 1000,
  refresh = 500
)
fit$save_object(file = "data/us_input/polling_error/polling_error_additive_ar.RDS")

###############################################################################
## Polling error against observed outcome
state_error <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = 2000 + (as.integer(t) - 2) * 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
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

plt <- ggplot(data = state_error, aes(x = 1 - inv.logit(dem_share), y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare",
       y = "Polling error (%, favors Republicans)") +
  theme_light()



