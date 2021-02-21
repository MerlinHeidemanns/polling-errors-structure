library(tidyverse)
library(cmdstanr)
library(boot)
library(rstanarm)
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
         val = 100 * (inv.logit(dem_share + val) - inv.logit(dem_share)),
         dem_share = inv.logit(dem_share),
         rep_share = 1 - dem_share) %>%
  group_by(t, s, dem_share, rep_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  )
###############################################################################

fit <- stan_glmer(q50 ~ (1 + rep_share | t), data = state_error, cores = 4)
new <- data.frame(
  t = sort(rep(seq(2000, 2020, 4), 100)),
  rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(t, rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = rep_share, y = q50), size = 0.5, data = state_error) +
  geom_errorbar(aes(x = rep_share, ymin = q25, ymax = q75), size = 0.75, data = state_error) +
  geom_errorbar(aes(x = rep_share, ymin = q10, ymax = q90), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(t~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican voteshare",
       y = "Polling error (%, favors Republicans)") +
  xlim(c(min(state_error$rep_share),max(state_error$rep_share)))

ggsave("plots/input/polling_error/polling_error_against_outcome_fitted_small_multiples.jpeg",
       plt,
       width = 10, height = 6)

