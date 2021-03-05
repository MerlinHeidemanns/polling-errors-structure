## Libraries
library(cmdstanr)
library(tidyverse)
library(boot)
library(rstanarm)
library(posterior)
## Load data
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, state_po, dem_share)
df <- read_csv("dta/clean_data/polls_pres_prepared.csv") %>%
  mutate(share_undecided = 1 - (democratic + republican)/100)
indexes <- read_csv("dta/clean_data/index_pres_prepared.csv")
df_exit <- read_csv("dta/clean_data/exit_polls_late_deciders_three_weeks_less.csv") %>%
  rename(dem_late_deciders = dem,
         rep_late_deciders = rep,
         state = state_po) %>%
  mutate(
    dem_late = dem_late_deciders/(dem_late_deciders + rep_late_deciders),
    rep_late = 1 - dem_late
    )
df <- df %>%
  left_join(df_exit, by = c("state" = "state",
                            "year" = "year")) %>%
  filter(year %in% seq(2004, 2016, 4)) %>%
  mutate(last_year = year - 4) %>%
  left_join(results, by = c("last_year" = "year",
                            "state" = "state_po")) %>%
  rename(prev_dem_share = dem_share) %>%
  mutate(diff = (rep_late - republican/(democratic + republican)),
         year = as.factor(year),
         prev_rep_share = 1 - prev_dem_share
  )
###############################################################################
## Fit
fit <- read_rds("dta/model_output/polling_error_pres.RDS")
## Data
results <- read_csv("dta/potus_results_76_20.csv") %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, state_po, dem_share)
df_exit <- read_csv("dta/clean_data/exit_polls_late_deciders_three_weeks_less.csv")
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
  left_join(df_exit, by = c("s" = "state",
                            "t" = "year")) %>%
  filter(t %in% seq(2004, 2016, 4)) %>%
  mutate(t = as.factor(t),
         rep_late_v_prev_rep = rep_late - previous_rep_share) %>%
  left_join(df %>%
              group_by(year, state) %>%
              summarize(diff = mean(diff),
                        share_undecided = mean(share_undecided)), by = c("s" = "state",
                            "t" = "year"))
## Fit
fit <- stan_glm(q50 ~ diff * previous_rep_share + previous_rep_share * t,
                data = state_error)

new <- data.frame(
  t = rep(as.factor(sort(rep(seq(2004, 2016, 4), 100))), 3),
  previous_rep_share = rep(rep(seq(0, 1, length.out = 100), 4), 3),
  diff = rep(sort(c(-0.25, 0, 0.25)), 400),
  i = as.character(1:1200)
) %>%
  mutate(diff = 0.5 - previous_rep_share)
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


ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.2) +
  geom_text(aes(x = previous_rep_share,
                 y = q50, color = to_local, label = s),
               data = state_error %>%
               mutate(to_local = ifelse(diff < 0,
                                        "More Democratic",
                                        "More Republican"),
                      diff = NA) %>%
               filter(!is.na(to_local)),
             size = 2) +
  scale_colour_manual(values = c("More Democratic" = "blue",
                                 "More Republican" = "red")) +
  facet_wrap(t ~. ) +
  theme_light() +
  labs(caption =
         "Positive polling errors underestimate Republican support
       Median, 50%/90% interval",
       color = "Rep. share late deciders - poll (two party)",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)"
       ) +
  theme(legend.position = "bottom") +
  xlim(c(min(state_error$previous_rep_share),max(state_error$previous_rep_share))) +
  ylim(c(-8, 8))

####################
# x = share undecided
# y = polling error
ggplot(data = state_error, aes(x = share_undecided, y = q50)) +
  geom_point(size = 0.8) +
  facet_wrap(t ~.) +
  theme_light() +
  labs(
    y = "Polling error (%, favors Democrats)",
    caption =
      "Positive polling errors underestimate Republican support
       Median, 50%/90% interval",
    x = "Share undecided/(1 - dem_share - rep_share)"
  )




















fit <- stan_glm(diff ~ year * prev_rep_share, data = df)
new <- data.frame(
  year = as.factor(sort(rep(seq(2004, 2016, 4), 100))),
  prev_rep_share = rep(seq(0, 1, length.out = 100), 4),
  i = as.character(1:400)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(year, prev_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
ggplot(data = pred, aes(x = prev_rep_share,
                        y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.2) +
  geom_point(aes(x = prev_rep_share, y = diff),
             size = 0.6, data = df) +
  facet_wrap(year ~ .) +
  theme_light() +
  labs(x = "Previous Republican vote share",
       y = "Republican two party share late - poll",
       caption = "A positive y indicates that late deciding voters were more
       Republican than the poll two party average") +
  xlim(c(min(df$prev_rep_share),max(df$prev_rep_share)))

ggplot(data = df, aes(x = finalTwoPartyVSRepublican,
                      y = 1 - (democratic + republican)/100)) +
  geom_point(size = 0.6) +
  facet_wrap(year ~ .) +
  theme_light() +
  labs(x = "Current Republican vote share",
       y = "Share not two-party respondents")

