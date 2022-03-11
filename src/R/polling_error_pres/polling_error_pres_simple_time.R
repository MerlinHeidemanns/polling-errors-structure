## Libraries
library(cmdstanr)
library(tidyverse)
## Load data
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  mutate(y = floor(democratic/100 * numberOfRespondents),
         n = floor((democratic + republican)/100 * numberOfRespondents),
         t_to_election = as.integer(electionDate - endDate),
         t_to_election = ifelse(t_to_election > 14, 1,
                         ifelse(t_to_election > 7, 2, 3)))
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(rep_share = rep/(dem + rep)) %>%
  select(year, state_po, rep_share)
indexes <- read_csv("dta/clean_data/index_pres_prepared.csv")
## Model
m <- file.path("src/stan/polling_error_pres", "polling_error_simple_days.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df),
  T = df %>% pull(t) %>% max(),
  T_days = df %>% pull(t_to_election) %>% max(),
  t = df %>% pull(t_to_election),
  S = df %>% pull(s) %>% max(),
  x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
  xi_index = indexes %>% pull(t)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)
## Save
fit$save_object(file = "dta/model_output/polling_error_pres_simple_time.RDS")


draws <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "xt",
               values_to = "draw")
state_abb <- df %>%
  distinct(state, s) %>%
  arrange(s) %>%
  pull(state)
draws <- draws %>%
  mutate(year = 2000 + 4 * (as.integer(str_match(xt,",(\\d+)\\]")[,2]) - 1),
         state = state_abb[as.integer(str_match(xt,",(\\d+),")[,2])],
         t = as.integer(str_match(xt,"\\[(\\d+),")[,2])
  ) %>%
  left_join(df %>%
              distinct(state, year,
                       outcome),
            by = c("year" = "year",
                   "state" = "state"))
state_error <- draws %>%
  filter(!is.na(outcome)) %>%
  mutate(error = 100 * (inv.logit(logit(outcome) + draw) -
                          outcome)) %>%
  group_by(t, year, state) %>%
  summarize(
    q50 = quantile(error, 0.5),
    q25 = quantile(error, 0.25),
    q75 = quantile(error, 0.75),
    q10 = quantile(error, 0.1),
    q90 = quantile(error, 0.9)
  ) %>%
  left_join(results %>%
              mutate(year = year + 4),
            by = c("year" = "year", "state" = "state_po")) %>%
  rename(prev_rep_share = rep_share) %>%
  mutate(year = as.factor(year),
         t = ifelse(t == 1, "3 weeks out",
             ifelse(t == 2, "2 weeks out", "1 week out")),
         t = factor(t, levels = c("3 weeks out", "2 weeks out", "1 week out")))



fit <- stan_glm(q50 ~ year * prev_rep_share * t, data = state_error)
new <- data.frame(
  year = as.factor(sort(rep(seq(2000, 2020, 4), 300))),
  prev_rep_share = rep(seq(0, 1, length.out = 100), 18),
  t = rep(sort(rep(c("3 weeks out", "2 weeks out", "1 week out"), 100)), 6),
  i = as.character(1:1800)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  mutate(t = factor(t, levels = c("3 weeks out", "2 weeks out", "1 week out"))) %>%
  group_by(t, prev_rep_share, year) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
write_csv(pred, file = )
plt <- ggplot(data = pred, aes(x = prev_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = prev_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_grid(year~t) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)") +
  xlim(c(min(state_error$prev_rep_share),max(state_error$prev_rep_share))) +
  ylim(c(min(state_error$q50),max(state_error$q50)))


plt <- ggplot(data = pred, aes(x = prev_rep_share, y = q50, fill = t, color = t)) +
  geom_line(size = 0.7) +
  geom_point(aes(x = prev_rep_share, y = q50), size = 0.5, data = state_error) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(year~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       color = "Weeks out",
       fill = "Weeks out") +
  xlim(c(min(state_error$prev_rep_share),max(state_error$prev_rep_share))) +
  ylim(c(min(state_error$q50),max(state_error$q50)))












