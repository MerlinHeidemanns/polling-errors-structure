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
df <- read_stata("dta/uk/bes2017")
df_results_2017 <- read_csv("dta/uk/results_2017.csv")
df_results_2015 <- read_csv("dta/uk/results_2015.csv")
################################################################################
## Note
# generalElectionVote: Vote intention for 2017
# p_pcon: Parliamentary Constituency
################################################################################
crosswalk_county_constituency <- df_results %>%
  distinct(county_name, constituency_name, region_name, country_name) %>%
  rename(pcon = constituency_name) %>%
  mutate(pcon = tolower(pcon))
################################################################################
# Result data
# * select columns
# * rename
# * look at con v lab
# * tolower for merge
df_results_constituency_2015 <- df_results_2015 %>%
  select(`Party abbreviation`, `Constituency Name`, `Share (%)`) %>%
  filter(`Party abbreviation` %in% c("Con", "Lab")) %>%
  pivot_wider(id_cols     = `Constituency Name`,
              values_from = `Share (%)`,
              names_from  = `Party abbreviation`) %>%
  rename(pcon               = `Constituency Name`,
         share_conservative_2015 = `Con`,
         share_labor_2015        = `Lab`) %>%
  select(pcon, share_conservative_2015, share_labor_2015) %>%
  mutate(share_joint_2015        = share_conservative_2015 + share_labor_2015,
         share_conservative_2015 = share_conservative_2015/share_joint_2015,
         share_labor_2015        = share_labor_2015/share_joint_2015,
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
    share_conservative_poll_unweighted = mean(generalElectionVote == 1),
    share_labor_poll_unweighted = mean(generalElectionVote == 2),
    share_labor_poll = sum((generalElectionVote == 2) * wt/sum(wt)),
    N = sum((generalElectionVote == 1 | generalElectionVote == 2)),
    share_joint_poll = share_conservative_poll + share_labor_poll,
    share_joint_poll_unweighted = share_conservative_poll_unweighted + share_labor_poll_unweighted
  ) %>%
  ungroup() %>%
  mutate(share_conservative_poll = share_conservative_poll/share_joint_poll,
         share_labor_poll = share_labor_poll/share_joint_poll,
         share_conservative_poll_unweighted = share_conservative_poll_unweighted/share_joint_poll_unweighted,
         share_labor_poll_unweighted = share_labor_poll_unweighted/share_joint_poll_unweighted,
         pcon = sjlabelled::as_label(pcon),
         pcon = str_replace_all(pcon, ",\\s", " - "),
         pcon = tolower(pcon),
         sd_poll = sqrt(share_conservative_poll * (1 - share_conservative_poll)/N))

# Join polls and results
df_joint_constituency <- df_poll_constituency %>%
  left_join(df_results_constituency_2015) %>%
  left_join(df_results_constituency_2017) %>%
  mutate(polling_error = share_conservative_2017 - share_conservative_poll) %>%
  select(pcon, sd_poll, share_conservative_2015, share_conservative_2017,
         share_conservative_poll, polling_error) %>%
  pivot_longer(c(share_conservative_2015, share_conservative_2017),
               names_to = "year",
               values_to = "voteshare")

ggplot(df_joint_constituency, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = polling_error - sd_poll,
                    ymax = polling_error + sd_poll),
                width = 0, size = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)

df_joint_constituency <- df_poll_constituency %>%
  left_join(df_results_constituency_2015) %>%
  left_join(df_results_constituency_2017)

ggplot(df_joint_constituency, aes(x = share_conservative, y = share_conservative - share_conservative_poll)) +
  geom_point(size = 0.5, aes(color = country_name)) +
  geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
                    ymax = share_conservative - share_conservative_poll + sd_poll,
                    color = country_name),
                width = 0, size = 0.2) +
  geom_smooth(method = "lm")

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
  group_by(county_name, region_name, country_name) %>%
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
ggplot(df_joint_county, aes(x = share_conservative,
                            y = share_conservative - share_conservative_poll,
                            color = country_name)) +
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
            mutate(year = 2017), "dta/uk/uk_polling_error_2017_countituency_bes.csv")
write_csv(df_joint_county %>%
            mutate(year = 2017), "dta/uk/uk_polling_error_2017_county_bes.csv")









