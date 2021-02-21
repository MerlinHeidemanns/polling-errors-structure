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
turnout <- read_csv("data/us_background/potus_turnout.csv")
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
         t_lagged = t - 4) %>%
  left_join(turnout %>%
              mutate(vap_highest_office_curr = vap_highest_office) %>%
              select(state_po, year, vap_highest_office_curr), by = c("s" = "state_po", "t" = "year")) %>%
  left_join(turnout %>%
              mutate(vap_highest_office_prev = vap_highest_office) %>%
              select(state_po, year, vap_highest_office_prev), by = c("s" = "state_po", "t_lagged" = "year")) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(dem_share = logit(dem/(dem + rep)),
         val = 100 * (inv.logit(dem_share + val) - inv.logit(dem_share))) %>%
  group_by(t, s, dem_share, vap_highest_office_prev, vap_highest_office_curr) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  ) %>%
  group_by(t) %>%
  mutate(vap_highest_office_std = (vap_highest_office_curr - mean(vap_highest_office_curr, na.rm = TRUE))/sd(vap_highest_office_curr, na.rm = TRUE),
         turnout_diff = vap_highest_office_curr - vap_highest_office_prev)


plt <- ggplot(data = state_error, aes(x = vap_highest_office_std, y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Turnout (standardized by year)",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
ggsave("plots/input/polling_error/v5_polling_error_against_turnout.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(data = state_error, aes(x = turnout_diff, y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Turnout change from prev. election (% points)",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
ggsave("plots/input/polling_error/v5_polling_error_against_turnout_change.jpeg",
       plt,
       width = 10, height = 6)

