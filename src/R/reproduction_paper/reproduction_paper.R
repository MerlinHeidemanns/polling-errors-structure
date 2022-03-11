###############################################################################
## Plot replication file
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
##
dot_size <- 0.25
###############################################################################
## German Laender elections
polling_error <- read_csv("dta/model_output/de_polling_error.csv")
plt_de <- ggplot(data = polling_error, aes(x = percent_election, y = `50%`)) +
  geom_point(size = dot_size) +
  # geom_errorbar(aes(ymin = `10%`,
  #                   ymax = `90%`, color = year)) +
  # geom_smooth(aes(color = year), method = "lm", se = 0) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "CDU/CSU two-party vote share",
       y = "Polling error",
       title = "Germany Regional") +
  theme_light() +
  theme(legend.title = element_blank()) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2)
ggsave("plt/paper2/germany_laender_elections.png", plot = plt_de)

###############################################################################
## US Senate Elections
senate_error <- read_csv(
    file = "dta/model_output/measurement_error/senate_races_yhat_epsilon.Rds")
## Plot
plt_sen <- ggplot(data = senate_error,
       aes(x = previous_dem_share, y = q50_epsilon)) +
  geom_point(size = dot_size) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Previous Democratic vote share",
       y = "Polling error",
       title = "US Senate")  +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  theme_light()
###############################################################################
## US Governors Elections
governor_error <- read_csv(
    file = "dta/model_output/measurement_error/governor_races_yhat_epsilon.Rds")
## Plot
plt_gov <- ggplot(data = governor_error,
              aes(x = finalTwoPartyVSDemocratic, y = q50_epsilon)) +
  geom_point(size = dot_size) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Previous Democratic vote share",
       y = "Polling error",
       title = "US Governor") +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  theme_light()
ggsave("plt/paper2/polling_error_us_governor.png", plot = plt_gov)
###############################################################################
## US House error
house_error <- read_csv(
  file = "dta/model_output/measurement_error/house_races_yhat_epsilon.Rds")
## Plot
plt_house <- ggplot(data = house_error, aes(x = previous_dem_share, y = q50_epsilon)) +
  geom_point(size = dot_size) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Previous Democratic vote share",
       y = "Polling error",
       title = "US House") +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  theme_light()
ggsave("plt/paper2/polling_error_us_house.png", plot = plt_house)
###############################################################################
## Canada
#' polling_error_canada_2019.R
df <- read_csv(file = "dta/model_output/measurement_error/canada_2019_polling_error.csv")

plt_can <- ggplot(df %>%
         filter(year == "conservative_voteshare_2015"),
       aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Previous Conservative vote share",
       y = "Polling error",
       title = "Canada Federal") +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  theme_light()
###############################################################################
## UK
df_joint_county_2019 <- read_csv("dta/uk/uk_polling_error_2019_county_bes.csv")
df_joint_county_2017 <- read_csv("dta/uk/uk_polling_error_2017_county_bes.csv")
df_joint_county_2015 <- read_csv("dta/uk/uk_polling_error_2015_county_bes.csv")
df_joint_county <- bind_rows(
  df_joint_county_2019,
  df_joint_county_2017,
  df_joint_county_2015
) %>%
  mutate(
    polling_error = share_conservative - share_conservative_poll
  ) %>%
  group_by(year) %>%
  mutate(polling_error_adjusted = polling_error - mean(polling_error, na.rm = TRUE))

plt_uk <- ggplot(df_joint_county, aes(x = share_conservative,
                                  y = polling_error_adjusted)) +
  geom_point(size = dot_size) +
  # geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
  #                   ymax = share_conservative - share_conservative_poll + sd_poll),
  #               width = 0, size = 0.2) +
  #geom_smooth(method = "lm", se = 0, size = 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  theme_light() +
  labs(x = "Previous Conservative vote share",
       y = "Polling error",
       title = "UK National") +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2)
###############################################################################
## Join
library(gridExtra)
plt_joint <- grid.arrange(plt_sen, plt_gov, plt_house, plt_de, plt_can,plt_uk,
             layout_matrix = rbind(c(1,1,2,2,5,5),
                                   c(1,1,2,2,5,5),
                                   c(3,3,4,4,6,6),
                                   c(3,3,4,4,6,6)))
ggsave("plt/paper2/polling_error_joint.png", plt_joint)
###############################################################################
## US General elections
state_error <- read_csv(file = "dta/model_output/measurement_error/by_year_state_error_and_yhat.Rds")
state_error_fake <- read_csv(file = "dta/model_output/measurement_error/by_year_state_error_and_yhat_fake_resampled.Rds")
#### Dots
plt1_df <- state_error %>%
  filter(s != "DC") %>%
  select(dem_share_current, q50_epsilon) %>%
  rename(q50 = q50_epsilon) %>%
  mutate(kind = "Polling error")
plt2_df <- state_error %>%
  filter(s != "DC") %>%
  select(dem_share_current, q50_yhat) %>%
  rename(q50 = q50_yhat) %>%
  mutate(kind = "Prediction")
plt3_df <- state_error_fake %>%
  filter(s != "DC") %>%
  select(dem_share_current, q50_epsilon) %>%
  rename(q50 = q50_epsilon) %>%
  mutate(kind = "Polling error")
plt4_df <- state_error_fake %>%
  filter(s != "DC") %>%
  select(dem_share_current, q50_yhat) %>%
  rename(q50 = q50_yhat) %>%
  mutate(kind = "Prediction")
plt1 <- ggplot(data = plt1_df, aes(x = dem_share_current, y = q50 / 100)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Democrativ vote share",
       y = "Polling error") +
  theme_light() +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)
plt2 <- ggplot(data = plt2_df, aes(x = q50, y = dem_share_current)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Polling average - mean shift",
       y = "Democratic vote share") +
  theme_light() +
  geom_abline(aes(intercept = 0, slope = 1), size = 0.4, linetype = 2)
plt3 <- ggplot(data = plt3_df, aes(x = dem_share_current, y = q50 / 100)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Democrativ vote share",
       y = "Polling error") +
  theme_light() +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)
plt4 <- ggplot(data = plt4_df, aes(x = q50, y = dem_share_current)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Polling average - mean shift",
       y = "Democratic vote share") +
  theme_light() +
  geom_abline(aes(intercept = 0, slope = 1), size = 0.4, linetype = 2)
plt <- grid.arrange(plt1, plt2, plt3, plt4, layout_matrix = rbind(c(1, 1, 2, 2),
                                                      c(1, 1, 2, 2),
                                                      c(3, 3, 4, 4),
                                                      c(3, 3, 4, 4)))
ggsave("plt/paper2/polling_error_voteshare_plt.png", plot = plt)
###############################################################################
## Undecided voters
pred <- read_csv(file = "dta/model_output/measurement_error/distribution_late_deciders.csv")
house_error <- read_cv(file = "dta/model_output/measurement_error/distribution_late_deciders_errors.csv")

plt <- ggplot(data = pred, aes(x = previous_rep_share, y = q50)) +
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
            size = 5) +
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
  xlim(c(0.3,0.8)) +
  ylim(c(-8, 8))
ggsave("plt/paper2/distribution_undecided_voters.png", plot = plt)
## Share undecided vs polling error
plt <- ggplot(data = state_error, aes(x = share_undecided, y = abs(q50))) +
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
ggsave("plt/paper2/distribution_undecided_voters_v_size.png", plot = plt)

################################################################################
## Polling error timing relative to the election day
pred <- read_csv(file = "dta/model_output/measurement_error/poll_error_timing.csv")
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
ggsave("plt/paper2/day_before_election_facet.png", plot = plt)

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
ggsave("plt/paper2/day_before_election_color.png", plot = plt)



################################################################################
## Attentiveness
df <- read_csv(file = "dta/model_output/can_polling_error_attentive.csv")

plt <- ggplot(df %>%
         filter(sd != 0), aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  # geom_errorbar(aes(ymin = polling_error - sd,
  #                   ymax = polling_error + sd),
  #               size = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  geom_smooth(method = "lm", se = 0, color = "blue") +
  facet_grid(year ~ kind) +
  theme_light() +
  labs(x = "Conservative two-party vote share",
       y = "Polling error")
ggsave("plt/paper2/canada_inattentiveness.png", plot = plt)

################################################################################
## Mode differentation
state_error <- read_csv("dta/model_output/epsilon_mode.csv") %>%
  filter(s != "DC")
plt <- ggplot(data = state_error, aes(x = dem_share_current, y = q50_epsilon / 100, color = method)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0) +
  labs(x = "Democrativ vote share",
       y = "Polling error") +
  theme_light() +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2) +
  theme(legend.title = element_blank())
ggsave("plt/paper2/mode_differentiation.png", plot = plt)


################################################################################
difference <- read_csv(file = "dta/model_output/forecast_error_adjustment.csv")
plt <- ggplot(data = difference %>%
         group_by(iter, lambda_new) %>%
         summarize(error_new =
                     mean(abs(pred_new_error)),
                   error_base =
                     mean(abs(pred_base_error)))
       %>% filter(abs(error_base - error_new) < 0.05 )) +
  geom_point(aes(x = error_base,
                 y = error_new,
                 color = lambda_new),
             size = 0.7, alpha = 0.8) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_light() +
  coord_equal() +
  lims(x = c(0.015, 0.03), y = c(0.015, 0.03)) +
  labs(x = "MAE before adjustment",
       y = "MAE after adjustment",
       color = "Lambda")
ggsave("plt/paper2/forecast_error_adjustment.png", plot = plt)
