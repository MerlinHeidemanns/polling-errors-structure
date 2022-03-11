################################################################################
## Polling error Governors election US
################################################################################
## Clean
rm(list = ls())
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
library(boot)
################################################################################
## Load data
df <- read_csv("dta/clean_data/polls_gov.csv")
results <- read_csv("dta/potus_results_76_20.csv")
################################################################################
## Prepare
df_gov <- df %>%
  group_by(race_id) %>%
  mutate(i = as.integer(cur_group_id())) %>%
  ungroup() %>%
  arrange(state, year) %>%
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
         previous_rep_share = 1 - previous_dem_share) %>%
  rename(idx_e = i) %>%
  mutate(idx_t = as.integer(as.factor(year)))
outcomes <- df_gov %>%
  distinct(idx_e, idx_t, finalTwoPartyVSDemocratic) %>%
  arrange(idx_e)
################################################################################
ggplot(df_gov %>%
         group_by(idx_e, idx_t) %>%
         summarize(result = mean(previous_dem_share),
                   y = sum(dem_respondents),
                   n = sum(two_party_respondents)),
       aes(x = result, y = result - y/n)) +
  geom_point() +
  geom_smooth(method = "lm")
################################################################################
## Model
m <- file.path("src/stan/polling_error_sen", "polling_error_sen.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df_gov),
  E = df_gov %>% pull(idx_e) %>% max(),
  T = df_gov %>% distinct(idx_t) %>% nrow(),
  idx_e = df_gov %>% pull(idx_e),
  idx_t = df_gov %>% pull(idx_t),
  idx_te = outcomes %>% pull(idx_t),
  y = df_gov %>% pull(dem_respondents),
  n = df_gov %>% pull(two_party_respondents),
  outcome = outcomes %>% pull(finalTwoPartyVSDemocratic)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)
################################################################################
## Transform output for plotting
epsilon <- fit$draws(c("epsilon")) %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "idx_e",
               names_pattern = "([\\d]+)",
               values_to = "epsilon")

governor_error <- epsilon %>%
  mutate(idx_e = as.integer(idx_e)) %>%
  filter(!is.na(idx_e)) %>%
  group_by(idx_e) %>%
  left_join(df_gov %>%
              distinct(idx_e,
                       state,
                       year,
                       finalTwoPartyVSDemocratic,
                       previous_dem_share)) %>%
  mutate(year5 = ifelse(year <= 2000, "-2000",
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
  group_by(idx_e, year5, year, evenyear, presidential,
           finalTwoPartyVSDemocratic,
           previous_dem_share) %>%
  summarize(
    q50_epsilon = quantile(epsilon, 0.5, na.rm = TRUE),
    q25_epsilon = quantile(epsilon, 0.25, na.rm = TRUE),
    q75_epsilon = quantile(epsilon, 0.75, na.rm = TRUE),
    q10_epsilon = quantile(epsilon, 0.10, na.rm = TRUE),
    q90_epsilon = quantile(epsilon, 0.90, na.rm = TRUE)
  ) %>%
  filter((q50_epsilon < 12), (q50_epsilon > -12)) %>%
  arrange(year) %>%
  mutate(evenyear = as.factor(evenyear),
         year = as.factor(year))

write_csv(governor_error,
          file = "dta/model_output/measurement_error/governor_races_yhat_epsilon.Rds")


## Plot
plt <- ggplot(data = governor_error,
              aes(x = previous_dem_share, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5) +
  labs(x = "Democratic vote share",
       y = "Polling error (percentage points, y - yhat)",
       title = "Governor races") +
  theme_light()
ggsave("plt/polling_error_voteshare/gov_polling_error_pres_voteshare_all.png",
       plt,
       height = 6, width = 10)

plt_multiple <- ggplot(data = governor_error, aes(x = finalTwoPartyVSDemocratic,
                                                  y = q50_epsilon)) +
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











