## Libraries
library(tidyverse)
library(cmdstanr)
library(boot)
## Load data
df <- read_csv("dta/clean_data/polls_gov.csv")
results <- read_csv("dta/potus_results_76_20.csv")
## Prepare
df_gov <- df %>%
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
  filter(!is.na(dem_respondents),
         !is.na(two_party_respondents),
         !is.na(finalTwoPartyVSDemocratic)) %>%
  left_join(results, by = c("last_general" = "year",
                            "state" = "state_po")) %>%
  mutate(previous_dem_share = dem/(dem + rep),
         previous_rep_share = 1 - previous_dem_share)
## Model
m <- file.path("src/stan/polling_error_sen", "polling_error_sen_no_pooling.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df_gov),
  R = df_gov %>% pull(i) %>% max(),
  r = df_gov %>% pull(i),
  y = df_gov %>% pull(dem_respondents),
  n = df_gov %>% pull(two_party_respondents),
  outcome = df_gov %>% pull(finalTwoPartyVSDemocratic)
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
  left_join(df_gov %>%
              distinct(i,
                       finalTwoPartyVSDemocratic,
                       finalTwoPartyVSRepublican,
                       year,
                       previous_dem_share,
                       previous_rep_share)) %>%
  mutate(error = 100 * (inv.logit(logit(finalTwoPartyVSDemocratic) + draw) -
                          finalTwoPartyVSDemocratic),

         year5 = ifelse(year <= 2000, "-2000",
                 ifelse(year <= 2004, "2001-2004",
                 ifelse(year <= 2008, "2005-2008",
                 ifelse(year <= 2012, "2009-2012",
                 ifelse(year <= 2016, "2013-2016",
                 ifelse(year <= 2020, "2017-2020",
                                            NA)))))),
         presidential = ifelse((year %% 4) == 0, "presidential",
                        ifelse((year %% 2) == 0, "midterm",
                               "neither")),
         evenyear = ifelse(year %% 2 == 1, year - 1, year)) %>%
  group_by(i, year5, year, evenyear, presidential, previous_dem_share, previous_rep_share, finalTwoPartyVSDemocratic) %>%
  summarize(
    q50 = quantile(error, 0.5),
    q25 = quantile(error, 0.25),
    q75 = quantile(error, 0.75),
    q10 = quantile(error, 0.10),
    q90 = quantile(error, 0.90)
  ) %>%
  filter((q50 < 12), (q50 > -12)) %>%
  mutate(evenyear = as.factor(evenyear),
         year = as.factor(year))
## Plot
plt <- ggplot(data = race_error, aes(x = previous_rep_share  , y = q50)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval,
               mean at 0.5 = -0.399,
               slope (percentage point) = 0.025",
       x = "Republican voteshare in state in last Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "Governor races") +
  theme_light()
ggsave("plt/polling_error_voteshare/gov_polling_error_pres_voteshare_all.png",
       plt,
       height = 6, width = 10)
plt_multiple <- ggplot(data = race_error, aes(x = previous_rep_share, y = q50)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval",
       x = "Republican voteshare in state in last Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "Governor races") +
  facet_wrap(year5 ~.) +
  theme_light()
ggsave("plt/polling_error_voteshare/gov_polling_error_pres_voteshare_5years.png",
       plt_multiple,
       height = 6, width = 10)
## two year intervals
fit_evenyear <- stan_glm(q50 ~ previous_rep_share * evenyear, data = race_error,
                prior = normal(0, 10),
                prior_intercept = normal(0, 10))
new <- data.frame(
  evenyear = as.factor(sort(rep(seq(1998, 2020, 2), 100))),
  previous_rep_share = rep(seq(min(race_error$previous_rep_share),
                               max(race_error$previous_rep_share),
                               length.out = 100), 12),
  i = as.character(1:1200)
)
pred <- posterior_epred(fit_evenyear, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(evenyear, previous_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt_by_yeareven <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_point(aes(x = previous_rep_share, y = q50),
             size = 0.5, data = race_error) +
  geom_line(size= 0.5) +
  geom_ribbon(aes(ymin = q25, ymax = q75), size= 0.5, alpha = 0.3) +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50% interval
               Odd years assigned to previous year",
       x = "Republican voteshare in state in previous Presidential election",
       y = "Polling error (%, favors Democrats)",
       title = "Governor races") +
  theme_light() +
  facet_wrap(evenyear~.)
ggsave("plt/polling_error_voteshare/gov_polling_error_pres_voteshare_5years.png",
       plt_by_yeareven,
       height = 6, width = 10)










