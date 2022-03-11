################################################################################
## Polling error structure in the UK in 2019
################################################################################
## Libraries
library(tidyverse)
library(haven)
################################################################################
## Load data
df_joint_constituency_2019 <- read_csv("dta/uk/uk_polling_error_2019_countituency_bes.csv")
df_joint_county_2019 <- read_csv("dta/uk/uk_polling_error_2019_county_bes.csv")
df_joint_constituency_2017 <- read_csv("dta/uk/uk_polling_error_2017_countituency_bes.csv")
df_joint_county_2017 <- read_csv("dta/uk/uk_polling_error_2017_county_bes.csv")
df_joint_constituency_2015 <- read_csv("dta/uk/uk_polling_error_2015_countituency_bes.csv")
df_joint_county_2015 <- read_csv("dta/uk/uk_polling_error_2015_county_bes.csv")
################################################################################
## Combine data
df_joint_constituency <- bind_rows(
  df_joint_constituency_2019,
  df_joint_constituency_2017,
  df_joint_constituency_2015
)
df_joint_county <- bind_rows(
  df_joint_county_2019,
  df_joint_county_2017,
  df_joint_county_2015
)
################################################################################
## Plots constituency
plt_constituency <- ggplot(df_joint_constituency, aes(x = share_conservative,
                                  y = share_conservative - share_conservative_poll)) +
  geom_point(size = 0.5) +
  # geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
  #                   ymax = share_conservative - share_conservative_poll + sd_poll),
  #               width = 0, size = 0.2) +
  #geom_smooth(method = "lm", se = 0, size = 0.5) +
  geom_smooth(method = "lm", se = 0, size = 0.5) +
  theme_light() +
  labs(x = "Two-party voteshare Conservatives",
       y = "Two-party polling error",
       color = "Election") +
  facet_grid(. ~ year)
ggsave("plt/paper/bes_uk_constituency.png", plt_constituency)
## Plots county
plt_county <- ggplot(df_joint_county, aes(x = share_conservative,
                            y = share_conservative - share_conservative_poll,
                            color = as.character(year))) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
                    ymax = share_conservative - share_conservative_poll + sd_poll),
                width = 0, size = 0.2) +
  #geom_smooth(method = "lm", se = 0, size = 0.5) +
  geom_smooth(method = "loess", se = 0, size = 0.5) +
  theme_light() +
  labs(x = "Two-party voteshare Conservatives",
       y = "Two-party polling error",
       color = "Election")
ggsave("plt/paper/bes_uk_constituency.png", plt_county)





