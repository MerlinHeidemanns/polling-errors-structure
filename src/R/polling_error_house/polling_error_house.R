## Libraries
library(tidyverse)
library(cmdstanr)
library(boot)
## Load data
df <- read_csv("dta/clean_data/polls_house_538.csv")
results <- read_csv("dta/potus_results_76_20.csv")
## Prepare
df_house <- df %>%
  group_by(race_id) %>%
  mutate(i = cur_group_id()) %>%
  ungroup() %>%
  select(i, dem_respondents,
         two_party_respondents,
         finalTwoPartyVSDemocratic,
         finalTwoPartyVSRepublican,
         state,
         year) %>%
  mutate(last_general = (year - 1996 - 1)%/%4 * 4 + 1996) %>%
  filter(!is.na(two_party_respondents),
         !is.na(finalTwoPartyVSDemocratic)) %>%
  left_join(results, by = c("last_general" = "year",
                            "state" = "state_po")) %>%
  mutate(last_dem_share = dem/(dem + rep))
## Model
m <- file.path("src/stan/polling_error_sen", "polling_error_sen_no_pooling.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df_house),
  R = df_house %>% pull(i) %>% max(),
  r = df_house %>% pull(i),
  y = df_house %>% pull(dem_respondents),
  n = df_house %>% pull(two_party_respondents),
  outcome = df_house %>% pull(finalTwoPartyVSDemocratic)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)
## Transform output for plotting
race_error <- fit$draws("pred") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "i",
               names_pattern = "([\\d]+)",
               values_to = "draw") %>%
  mutate(i = as.integer(i)) %>%
  filter(!is.na(i)) %>%
  group_by(i) %>%
  left_join(df_house %>%
              distinct(i,
                       finalTwoPartyVSDemocratic,
                       finalTwoPartyVSRepublican,
                       year,
                       last_dem_share)) %>%
  mutate(error = 100 * (inv.logit(logit(finalTwoPartyVSDemocratic) + draw) -
                          finalTwoPartyVSDemocratic),
         year5 = ifelse(year <= 2000, "-2000",
                 ifelse(year <= 2004, "2001-2004",
                 ifelse(year <= 2008, "2005-2008",
                 ifelse(year <= 2012, "2009-2012",
                 ifelse(year <= 2016, "2013-2016",
                 ifelse(year <= 2020, "2017-2020",
                                      NA)))))),
         year = ifelse(year %% 2 == 1, year - 1, year),
         presidential = ifelse(year %% 4, "presidential", "midterm"),
         previous_rep_share = 1 - last_dem_share,
         previous_dem_share = last_dem_share,
         year = as.factor(year)) %>%
  group_by(i, year5, year, presidential, previous_rep_share, previous_dem_share, finalTwoPartyVSDemocratic) %>%
  summarize(
    q50 = quantile(error, 0.5),
    q25 = quantile(error, 0.25),
    q75 = quantile(error, 0.75),
    q10 = quantile(error, 0.10),
    q90 = quantile(error, 0.90)
  ) %>%
  filter((q50 < 12), (q50 > -12))
## Plot
plt <- ggplot(data = race_error, aes(x = previous_rep_share, y = q50)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare in state in previous Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "House races") +
  theme_light() +
  ylim(c(-12, 12))
ggsave("plt/polling_error_voteshare/house_polling_error_pres_voteshare_all.png",
       plt,
       height = 6, width = 10)
plt_multiple <- ggplot(data = race_error, aes(x = previous_rep_share, y = q50)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare in state in previous Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "House races") +
  theme_light() +
  facet_wrap(year5~.) +
  ylim(c(-12, 12))
ggsave("plt/polling_error_voteshare/house_polling_error_pres_voteshare_year5.png",
       plt_multiple,
       height = 6, width = 10)

fit <- stan_glm(q50 ~ previous_rep_share * year, data = race_error,
                prior = normal(0, 10),
                prior_intercept = normal(0, 10))
new <- data.frame(
  year = as.factor(sort(rep(seq(1998, 2018, 2), 100))),
  previous_rep_share = rep(seq(min(race_error$previous_rep_share),
                               max(race_error$previous_rep_share),
                               length.out = 100), 11),
  i = as.character(1:1100)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(year, previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt_by_year <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_point(aes(x = previous_rep_share, y = q50),
             size = 0.5, data = race_error) +
  geom_line(size= 0.5) +
  geom_ribbon(aes(ymin = q25, ymax = q75), size= 0.5, alpha = 0.3) +
  geom_ribbon(aes(ymin = q10, ymax = q90), size= 0.5, alpha = 0.2) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%, 90% interval",
       x = "Republican voteshare in state in previous Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "House races") +
  theme_light() +
  facet_wrap(year~.) +
  ylim(c(-12, 12))
ggsave("plt/polling_error_voteshare/house_polling_error_pres_voteshare_yeareach.png",
       plt_by_year,
       height = 6, width = 10)

