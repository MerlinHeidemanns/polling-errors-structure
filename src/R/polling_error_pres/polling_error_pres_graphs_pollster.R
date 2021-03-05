## Libraries
library(tidyverse)
library(cmdstanr)
library(boot)
library(rstanarm)
###############################################################################
## Rasmussen


## Fit
fit <- read_rds("dta/model_output/polling_error_pres_rasmussen.RDS")
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  filter(pollName == "Rasmussen Reports/Pulse Opinion Research") %>%
  distinct(state, t) %>%
  mutate(available = 1,
         t = 2000 + (t - 1) * 4)
## Data
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, state_po, dem_share)
## Polling error against observed outcome
state_error <- fit$draws("mu_matrix") %>%
  as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  rename(current_dem_share = dem_share) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  rename(previous_dem_share = dem_share) %>%
  left_join(df, by = c("s" = "state", "t" = "t")) %>%
  filter(available == 1) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(val = 100 * (inv.logit(logit(current_dem_share) + val) - current_dem_share),
         current_rep_share = 1 - current_dem_share,
         previous_rep_share = 1 - previous_dem_share,
  ) %>%
  group_by(t, s, current_dem_share, current_rep_share,
           previous_rep_share, previous_dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  )
## rstanarm fit small multiples
fit <- stan_glmer(q50 ~ (1 + previous_rep_share | t), data = state_error, cores = 4)
new <- data.frame(
  t = sort(rep(seq(2000, 2020, 4), 100)),
  previous_rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  filter(t != 2020) %>%
  group_by(t, previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(t~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "Rasmussen only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_general_rasmussen.png",
       plt,
       height = 6, width = 10)
## all
fit <- stan_glm(q50 ~ 1 + previous_rep_share, data = state_error, cores = 4)
new <- data.frame(
  previous_rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "Rasmussen only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_all_rasmussen.png",
       plt,
       height = 6, width = 10)





###############################################################################
## YouGov


## Fit
fit <- read_rds("dta/model_output/polling_error_pres_yougov.RDS")
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  filter(pollName == "YouGov") %>%
  distinct(state, t) %>%
  mutate(available = 1,
         t = 2000 + (t - 1) * 4)
## Data
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, state_po, dem_share)
## Polling error against observed outcome
state_error <- fit$draws("mu_matrix") %>%
  as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  rename(current_dem_share = dem_share) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  rename(previous_dem_share = dem_share) %>%
  left_join(df, by = c("s" = "state", "t" = "t")) %>%
  filter(available == 1,
         t != 2016,
         t != 2020) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(val = 100 * (inv.logit(logit(current_dem_share) + val) - current_dem_share),
         current_rep_share = 1 - current_dem_share,
         previous_rep_share = 1 - previous_dem_share,
  ) %>%
  group_by(t, s, current_dem_share, current_rep_share,
           previous_rep_share, previous_dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  ) %>%
  mutate(t = as.factor(t))
## rstanarm fit small multiples
fit <- stan_glm(q50 ~ previous_rep_share * t,
                data = state_error)
new <- data.frame(
  t = as.factor(rep(c(2008, 2012), 100)),
  previous_rep_share = rep(seq(0, 1, length.out = 100), 2),
  i = as.character(1:200)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  filter(!(t %in% c(2000, 2004, 2016, 2000))) %>%
  group_by(t, previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(t~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "YouGov only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_general_yougov.png",
       plt,
       height = 6, width = 10)
## all
fit <- stan_glm(q50 ~ 1 + previous_rep_share, data = state_error)
new <- data.frame(
  previous_rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "YouGov only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_all_yougov.png",
       plt,
       height = 6, width = 10)





###############################################################################
## Survey USA

## Fit
fit <- read_rds("dta/model_output/polling_error_pres_SurveyUSA.RDS")
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  filter(pollName == "SurveyUSA") %>%
  distinct(state, t) %>%
  mutate(available = 1,
         t = 2000 + (t - 1) * 4)
## Data
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, state_po, dem_share)
## Polling error against observed outcome
state_error <- fit$draws("mu_matrix") %>%
  as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(variable,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(variable,"(\\d+).")[,2])],
         t = 2000 + (as.integer(t) - 1) * 4,
         t_lagged = t - 4) %>%
  left_join(results, by = c("s" = "state_po", "t" = "year")) %>%
  rename(current_dem_share = dem_share) %>%
  left_join(results, by = c("s" = "state_po", "t_lagged" = "year")) %>%
  rename(previous_dem_share = dem_share) %>%
  left_join(df, by = c("s" = "state", "t" = "t")) %>%
  filter(available == 1) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(t), !is.na(s)) %>%
  mutate(val = 100 * (inv.logit(logit(current_dem_share) + val) - current_dem_share),
         current_rep_share = 1 - current_dem_share,
         previous_rep_share = 1 - previous_dem_share,
  ) %>%
  group_by(t, s, current_dem_share, current_rep_share,
           previous_rep_share, previous_dem_share) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75),
    q10 = quantile(val, 0.1),
    q90 = quantile(val, 0.9),
    bigger0 = mean(val > 0)
  ) %>%
  mutate(t = as.factor(t))
## rstanarm fit small multiples
fit <- stan_glm(q50 ~ previous_rep_share * t,
                data = state_error)
new <- data.frame(
  t = as.factor(rep(seq(2000, 2020, 4), 100)),
  previous_rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(t, previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(t~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "SurveyUSA only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_general_surveyUSA.png",
       plt,
       height = 6, width = 10)
## all
fit <- stan_glm(q50 ~ 1 + previous_rep_share, data = state_error)
new <- data.frame(
  previous_rep_share = rep(seq(0, 1, length.out = 100), 6),
  i = as.character(1:600)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = previous_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "SurveyUSA only (state-state covariance matrix)") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_all_SurveyUSA.png",
       plt,
       height = 6, width = 10)



