library(tidyverse)
library(cmdstanr)
## Fit
fit <- readRDS("data/us_input/polling_error/polling_error_additive_predictors.RDS")
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
## Polling error against observed outcome
state_error <- fit$draws("est_polling_error") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  mutate(variable = as.integer(variable),
         val = 100 * val) %>%
  left_join(indexes %>% select(t,s,x), by = c("variable" = "x")) %>%
  mutate(s = state_abb[s],
         t = 2000 + (as.integer(t) - 1) * 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  group_by(t, s, dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  )

plt <- ggplot(data = state_error, aes(x = 1 - dem_share, y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
ggsave("plots/input/polling_error/v5_polling_error_against_outcome.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(data = state_error, aes(x = 1 - dem_share, y = q50, color = as.factor(t))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare",
       y = "Polling error (%, favors Republicans)") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom")
ggsave("plots/input/polling_error/v5_polling_error_against_outcome_by_year.jpeg",
       plt,
       width = 10, height = 6)



state_error_lagged <- fit$draws("mu_matrix") %>%
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
  mutate(dem_share = dem/(dem + rep),
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

## Lagged Republican vote share
plt <- ggplot(data = state_error_lagged, aes(x = 1 - dem_share, y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare (previous election)",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
ggsave("plots/input/polling_error/v5_polling_error_against_outcome_lagged.jpeg",
       plt,
       width = 10, height = 6)

## Multiple comparison by year for lagged Republican vote share
plt <- ggplot(data = state_error_lagged, aes(x = 1 - dem_share, y = q50, color = as.factor(t))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare (previous election)",
       y = "Polling error (%, favors Republicans)") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("plots/input/polling_error/v5_polling_error_against_outcome_by_year_lagged.jpeg",
       plt,
       width = 10, height = 6)



## Difference from year to year
state_error_diff <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  left_join(results %>%
              mutate(dem_share_prev = dem/(dem + rep)) %>%
              select(state_po, year, dem_share_prev), by = c("s" = "state_po", "t_lagged" = "year")) %>%
  left_join(results %>%
              mutate(dem_share_curr = dem/(dem + rep)) %>%
              select(state_po, year, dem_share_curr), by = c("s" = "state_po", "t" = "year")) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(dem_diff = dem_share_curr - dem_share_prev,
         val = 100 * (inv.logit(dem_share_curr + val) - inv.logit(dem_share_curr))) %>%
  group_by(t, s, dem_diff) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  )

plt <- ggplot(data = state_error_diff, aes(x = dem_diff * (-1), y = q50)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican gain comp. prev. election",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
















###############################################################################
## Polling error against observed outcome
ggplot(data = state_error %>%
         mutate(s = factor(s, levels = states_2020_ordered),
                Underestimates = factor(ifelse(q50 > 0, "Reps",
                                               "Dems"), levels = c("Reps", "Dems"))),
       aes(x = t, y = q50, color = Underestimates, fill = Underestimates)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.75) +
  geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size= 0.5) +
  facet_wrap(~s) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Year",
       y = "Polling error (%, favors Republicans)")

ggplot(data = state_error %>%
         mutate(s = factor(s, levels = states_2020_ordered)),
       aes(x = t, y = q50, color = bigger0)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.75) +
  geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size= 0.5) +
  scale_color_gradient2(low = "blue", high = "red", mid = "grey", midpoint = 0.5) +
  facet_wrap(~s) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Year",
       y = "Polling error (%, favors Republicans)",
       color = "Pr(positive)")

ggplot(data = state_error %>%
         mutate(s = factor(s, levels = states_2020_ordered)),
       aes(x = t, y = q50, group = s)) +
  geom_line(size = 0.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(2000, 2020, 4)) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median",
       x = "Year",
       y = "Polling error (%, favors Republicans)") +
  theme_light()
geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.75) +
  geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size= 0.5) +
  scale_color_gradient2(low = "blue", high = "red", mid = "grey", midpoint = 0.5) +
  facet_wrap(~s) +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
