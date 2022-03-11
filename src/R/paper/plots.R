## Generate plots for the paper
library(ggplot2)
library(tidyverse)

## Prediction against polling error
state_error <- read_csv(file = "dta/model_output/measurement_error/by_year_state_error_and_yhat.Rds")

## Labels
plt <- ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = q50_yhat, y = q50_epsilon)) +
  #geom_point(size = 0.5) +
  geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Democratic voteshare (polling average)",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light() +
  facet_wrap(t ~.) +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)
ggsave("plt/paper/by_election_year_yhat_epsilon_labels.png", plt, width = 10, height = 6)

#### Dots
plt <- ggplot(data = state_error %>%
         filter(s != "DC"), aes(x = dem_share_current, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Democratic voteshare (polling average)",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light() +
  facet_wrap(t ~.) +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)
ggsave("plt/paper/by_election_year_yhat_epsilon_dots.png", plt, width = 10, height = 6)

#### Dots - past voteshare
plt <- ggplot(data = state_error %>%
                filter(s != "DC"), aes(x = dem_share_current, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_text(aes(label = s), size = 2.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(caption = "Positive polling errors underestimate Democratic support
               Median, 50% interval",
       x = "Democratic voteshare (previous election)",
       y = "Polling error (percentage points, y - yhat)") +
  theme_light() +
  facet_wrap(t ~.) +
  geom_hline(aes(yintercept = 0), size = 0.4, linetype = 2)

ggsave("plt/paper/by_election_year_ylag_epsilon_dots.png", plt, width = 10, height = 6)


## Governor races
governor_error <- read_csv(file = "dta/model_output/measurement_error/governor_races_yhat_epsilon.Rds")

plt <- ggplot(data = governor_error,
              aes(x = q50_yhat, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Democratic voteshare (polling average)",
       y = "Polling error (percentage points, y - yhat)",
       title = "Governor races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/governor_yhat_epsilon_dots.png",
       plt,
       height = 6, width = 10)

plt <- ggplot(data = governor_error,
              aes(x = finalTwoPartyVSDemocratic, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, color = "black", se = 0) +
  labs(x = "Democratic voteshare (current election)",
       y = "Polling error (percentage points, y - yhat)",
       title = "Governor races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/governor_y_epsilon_dots.png",
       plt,
       height = 6, width = 10)

## House races
house_error <- read_csv(file = "dta/model_output/measurement_error/house_races_yhat_epsilon.Rds")

plt <- ggplot(data = house_error,
              aes(x = q50_yhat, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Democratic voteshare (polling average)",
       y = "Polling error (percentage points, y - yhat)",
       title = "House races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/house_yhat_epsilon_dots.png",
       plt,
       height = 6, width = 10)

plt <- ggplot(data = house_error,
              aes(x = finalTwoPartyVSDemocratic, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Democratic voteshare (current election)",
       y = "Polling error (percentage points, y - yhat)",
       title = "House races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/house_y_epsilon_dots.png",
       plt,
       height = 6, width = 10)



## Senate races
senate_error <- read_csv(file = "dta/model_output/measurement_error/senate_races_yhat_epsilon.Rds")

plt <- ggplot(data = senate_error,
              aes(x = q50_yhat, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Democratic voteshare (polling average)",
       y = "Polling error (percentage points, y - yhat)",
       title = "Senate races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/senate_yhat_epsilon_dots.png",
       plt,
       height = 6, width = 10)


plt <- ggplot(data = senate_error,
              aes(x = finalTwoPartyVSDemocratic, y = q50_epsilon)) +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size= 0.5) +
  geom_smooth(method = "lm", size = 0.5, se = 0, color = "black") +
  labs(x = "Democratic voteshare (current election)",
       y = "Polling error (percentage points, y - yhat)",
       title = "Senate races") +
  theme_light() +
  facet_wrap(.~evenyear)
ggsave("plt/paper/senate_y_epsilon_dots.png",
       plt,
       height = 6, width = 10)


## Lambda by election year
lambda <- read_csv(file = "dta/model_output/measurement_error/lambda_by_year.Rds")

plt <- ggplot(lambda, aes(x = as.factor(t), y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  lims(y = c(0, NA)) +
  labs(y = "Lambda ('share of noise')",
       x = "Election")
ggsave("plt/paper/by_election_year_lambda.png", plt, width = 10, height = 6)

## All methodologies
lambda <- read_csv(file = "dta/model_output/measurement_error/lambda_by_methodology_all.Rds") %>%
  arrange(q50) %>%
  mutate(method_n = factor(method_n, levels = method_n))
plt <- ggplot(lambda, aes(x = method_n, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
  theme_light() +
  labs(y = "Lambda ('share of noise')",
       x = "Method") +
  coord_flip() +
  lims(y = c(0, NA))
ggsave("plt/paper/by_methodologies_all_lambda.png", plt, width = 10, height = 6)

## IVR, online, phone
lambda <- read_csv(file = "dta/model_output/measurement_error/lambda_by_methodology.Rds") %>%
  arrange(q50) %>%
  mutate(method = factor(method, levels = method))

plt <- ggplot(lambda, aes(x = method, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
  theme_light() +
  lims(y = c(0, NA)) +
  labs(y = "Lambda ('share of noise')",
       x = "Method")
ggsave("plt/paper/by_methodologies_three_lambda.png", plt, width = 10, height = 6)



## Day to election, model with coefficient
lambda <- read_csv(file = "dta/model_output/measurement_error/by_day_coefficient_lambda.Rds")

plt <- ggplot(lambda, aes(x = d, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  theme_light() +
  facet_wrap(t~.) +
  scale_x_reverse(limits = c(21, 1),
                  breaks = c(20, 15, 10, 5, 1)) +
  labs(x = "Days until election",
       y = "Lambda ('share of noise')")
ggsave("plt/paper/coefficient_lambda.png", plt, width = 10, height = 6)


