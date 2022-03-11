################################################################################
## Polling error structure in the UK
################################################################################
## Libraries
library(tidyverse)
library(haven)
################################################################################
## Clean
rm(list = ls())
################################################################################
## Load data
df <- read_stata("dta/uk/bes2015")
df_results <- read_csv("dta/uk/results_2015.csv")
################################################################################
## Note
# generalElectionVote: Vote intention for 2015
# p_pcon: Parliamentary Constituency
################################################################################
crosswalk_county_constituency <- df_results %>%
  distinct(County, `Constituency Name`) %>%
  rename(pcon = `Constituency Name`) %>%
  mutate(pcon = tolower(pcon))
################################################################################
df_results_constituency <- df_results %>%
  select(`Party abbreviation`, `Constituency Name`, `Share (%)`) %>%
  filter(`Party abbreviation` %in% c("Con", "Lab")) %>%
  pivot_wider(id_cols     = `Constituency Name`,
              values_from = `Share (%)`,
              names_from  = `Party abbreviation`) %>%
  rename(pcon               = `Constituency Name`,
         share_conservative = `Con`,
         share_labor        = `Lab`) %>%
  select(pcon, share_conservative, share_labor) %>%
  mutate(share_joint        = share_conservative + share_labor,
         share_conservative = share_conservative/share_joint,
         share_labor        = share_labor/share_joint,
         pcon               = tolower(pcon))
df_poll_constituency <- df %>%
  filter(!is.na(wt),
         !is.na(generalElectionVote)) %>%
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



## Join
df_joint_constituency <- df_poll_constituency %>%
  left_join(df_results_constituency)

## Plot
ggplot(df_joint_constituency, aes(x = share_conservative, y = share_conservative - share_conservative_poll)) +
  geom_point() +
  geom_errorbar(aes(ymin = share_conservative - share_conservative_poll - sd_poll,
                    ymax = share_conservative - share_conservative_poll + sd_poll),
                width = 0, size = 0.3) +
  geom_smooth(method = "lm")



################################################################################
## County
df_results_county <- df_results %>%
  select(`Party abbreviation`, `Constituency Name`, County, `Share (%)`, Votes) %>%
  filter(`Party abbreviation` %in% c("Con", "Lab")) %>%
  pivot_wider(id_cols     = c(`Constituency Name`, County),
              values_from = `Votes`,
              names_from  = `Party abbreviation`) %>%
  rename(votes_conservative = `Con`,
         votes_labor        = `Lab`) %>%
  select(County, votes_conservative, votes_labor) %>%
  group_by(County) %>%
  summarize(votes_joint        = sum(votes_conservative + votes_labor),
            votes_conservative = sum(votes_conservative),
            votes_labor        = sum(votes_labor)) %>%
  ungroup() %>%
  mutate(share_conservative = votes_conservative/votes_joint,
         share_labor        = votes_labor/votes_joint)

# Poll data
df_poll_county <- df  %>%
  filter(sjlabelled::as_label(pcon) != "NOT in a 2010 Parliamentary Constituency",
         !is.na(generalElectionVote),
         !is.na(wt)) %>%
  mutate(pcon = sjlabelled::as_label(pcon),
         pcon = str_replace_all(pcon, ",\\s", " - "),
         pcon = tolower(pcon)) %>%
  left_join(crosswalk_county_constituency) %>%
  group_by(County) %>%
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
            mutate(year = 2015), "dta/uk/uk_polling_error_2015_countituency_bes.csv")
write_csv(df_joint_county %>%
            mutate(year = 2015), "dta/uk/uk_polling_error_2015_county_bes.csv")






