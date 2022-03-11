################################################################################
## Polling error structure in the UK in 2019
################################################################################
## Libraries
library(tidyverse)
library(haven)
################################################################################
## Clean
rm(list = ls())
################################################################################
## Load data
df <- read_stata("dta/uk/bes2019")
df_results_2019 <- read_csv("dta/uk/results_2019.csv")
df_results_2017 <- read_csv("dta/uk/results_2017.csv")

################################################################################
## Note
# generalElectionVote: Vote intention for 2015
# p_pcon: Parliamentary Constituency
################################################################################
crosswalk_county_constituency <- df_results %>%
  distinct(county_name, constituency_name) %>%
  rename(pcon = constituency_name) %>%
  mutate(pcon = tolower(pcon))
################################################################################
# Result data
# * select columns
# * rename
# * look at con v lab
# * tolower for merge
df_results_constituency_2019 <- df_results_2019 %>%
  select(constituency_name, con, lab) %>%
  rename(pcon               = constituency_name,
         share_conservative_2019 = con,
         share_labor_2019        = lab) %>%
  mutate(share_joint_2019        = share_conservative_2019 + share_labor_2019,
         share_conservative_2019 = share_conservative_2019/share_joint_2019,
         share_labor_2019        = share_labor_2019/share_joint_2019,
         pcon               = tolower(pcon))
df_results_constituency_2017 <- df_results_2017 %>%
  select(constituency_name, con, lab, country_name, region_name) %>%
  rename(pcon               = constituency_name,
         share_conservative_2017 = con,
         share_labor_2017        = lab) %>%
  mutate(share_joint_2017        = share_conservative_2017 + share_labor_2017,
         share_conservative_2017 = share_conservative_2017/share_joint_2017,
         share_labor_2017        = share_labor_2017/share_joint_2017,
         pcon               = tolower(pcon))
# Poll data
df_poll_constituency <- df  %>%
  filter(sjlabelled::as_label(pcon) != "NOT in a 2010 Parliamentary Constituency",
         !is.na(generalElectionVote),
         !is.na(wt)) %>%
  group_by(pcon) %>%
  summarize(
    share_conservative_poll = sum((generalElectionVote == 1) * wt/sum(wt)),
    share_labor_poll = sum((generalElectionVote == 2) * wt/sum(wt)),
    N = sum((generalElectionVote == 1 | generalElectionVote == 2)),
    share_joint_poll = share_conservative_poll + share_labor_poll
  ) %>%
  ungroup() %>%
  mutate(share_conservative_poll = share_conservative_poll/share_joint_poll,
         share_labor_poll = share_labor_poll/share_joint_poll,
         pcon = sjlabelled::as_label(pcon),
         pcon = str_replace_all(pcon, ",\\s", " - "),
         pcon = tolower(pcon),
         sd_poll = sqrt(share_conservative_poll * (1 - share_conservative_poll)/N))

# Join polls and results
df_joint_constituency <- df_poll_constituency %>%
  left_join(df_results_constituency_2017) %>%
  left_join(df_results_constituency_2019) %>%
  select(pcon, share_conservative_poll, sd_poll, country_name,
         region_name, share_conservative_2019, share_conservative_2017) %>%
  mutate(polling_error = share_conservative_2019 - share_conservative_poll) %>%
  pivot_longer(c(share_conservative_2019, share_conservative_2017),
               names_to = "year",
               values_to = "voteshare")

ggplot(df_joint_constituency, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = polling_error - sd_poll,
                    ymax = polling_error + sd_poll),
                width = 0, size = 0.2) +
  geom_smooth(method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(year ~ .)


################################################################################
# Result data
# * select columns
# * rename
# * look at con v lab
# * tolower for merge
df_results_county <- df_results %>%
  select(county_name, con, lab) %>%
  rename(share_conservative = con,
         share_labor        = lab) %>%
  group_by(county_name) %>%
  summarize(share_joint = sum(share_conservative + share_labor),
            share_conservative = sum(share_conservative),
            share_labor = sum(share_labor)) %>%
  mutate(share_joint        = share_conservative + share_labor,
         share_conservative = share_conservative/share_joint,
         share_labor        = share_labor/share_joint)
# Poll data
df_poll_county <- df  %>%
  filter(sjlabelled::as_label(pcon) != "NOT in a 2010 Parliamentary Constituency",
         !is.na(generalElectionVote),
         !is.na(wt)) %>%
  mutate(pcon = sjlabelled::as_label(pcon),
         pcon = str_replace_all(pcon, ",\\s", " - "),
         pcon = tolower(pcon)) %>%
  left_join(crosswalk_county_constituency) %>%
  group_by(county_name) %>%
  summarize(
    share_conservative_poll = sum((generalElectionVote == 1) * wt/sum(wt)),
    share_labor_poll = sum((generalElectionVote == 2) * wt/sum(wt)),
    N = sum((generalElectionVote == 1 | generalElectionVote == 2)),
    share_joint_poll = share_conservative_poll + share_labor_poll
  ) %>%
  ungroup() %>%
  mutate(share_conservative_poll = share_conservative_poll/share_joint_poll,
         share_labor_poll = share_labor_poll/share_joint_poll,
         sd_poll = sqrt(share_conservative_poll * (1 - share_conservative_poll)/N))

# Join polls and results
df_joint_county <- df_poll_county %>%
  left_join(df_results_county)

# Plot
ggplot(df_joint_county, aes(x = share_conservative, y = share_conservative - share_conservative_poll)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
                    ymax = share_conservative - share_conservative_poll + sd_poll),
                width = 0, size = 0.2) +
  geom_smooth(method = "lm") +
  theme_light() +
  labs(y = "Polling error",
       x = "Share conservative")



## Save
write_csv(df_joint_constituency %>%
            mutate(year = 2019), "dta/uk/uk_polling_error_2019_countituency_bes.csv")
write_csv(df_joint_county %>%
            mutate(year = 2019), "dta/uk/uk_polling_error_2019_county_bes.csv")








