## Libraries
library(tidyverse)
library(lubridate)
## Import categorization
names <- read_csv("dta/us census bureau regions and divisions.csv")
## Import data
df <- read_delim("dta/polls_main_dataset.tsv", delim = "\t")
df_sen_538 <- read_csv("dta/senate_polls.csv")
df_sen_538_outcomes <- read_csv("dta/need_outcome_sen.csv")
df_sen_538_pollster_ratings <- read_csv("dta/dirty_data/polls_538_pollster_ratings.csv")
## Manipulate 538 data
df_sen_538_2019 <- df_sen_538_pollster_ratings %>%
  filter((cand1_party == "DEM") & (cand2_party == "REP"),
         type_simple == "Sen-G",
         location != "US") %>%
  mutate(
    dem_party_share = cand1_pct/(cand1_pct + cand2_pct),
    two_party_respondents = samplesize * (cand1_pct + cand2_pct)/100,
    dem_respondents = floor(two_party_respondents * dem_party_share),
    rep_respondents = floor(two_party_respondents * (1 - dem_party_share)),
    state = location,
    election = "Sen",
    finalTwoPartyVSDemocratic = cand1_actual/(cand1_actual + cand2_actual),
    finalTwoPartyVSRepublican = 1 - finalTwoPartyVSDemocratic,
    pollName = pollster,
    endDate = mdy(polldate),
    startDate = NA,
    electionDate = mdy(electiondate)
  ) %>%
  left_join(names, by = c("state" = "State Code")) %>%
  select(state, pollName, year,
         startDate, endDate, finalTwoPartyVSDemocratic,
         finalTwoPartyVSRepublican, dem_respondents,
         rep_respondents, race_id,
         two_party_respondents,
         electionDate,
         Region, Division, PADD)

## manipulate Senate race
df_sen_538_cleaned <- df_sen_538 %>%
  left_join(names, by = c("state" = "State")) %>%
  filter(cycle == 2020) %>%
  group_by(poll_id, race_id) %>%
  mutate(candidate_party = ifelse(candidate_name %in% c("Angus S. King Jr.", "Bernard Sanders"),
                                  "DEM", candidate_party)) %>%
  filter(candidate_party %in% c("DEM", "REP"),
         !(race_id %in% c(97,6295, 130, 129, 6271, 7780))) %>%
  mutate(
         dem_pct_self = ifelse(candidate_party == "DEM", pct, NA),
         rep_pct_self = ifelse(candidate_party == "REP", pct, NA),
         dem_pct = max(dem_pct_self, na.rm = TRUE),
         rep_pct = max(rep_pct_self, na.rm = TRUE)) %>%
  filter(!(dem_pct_self != dem_pct)) %>%
  filter(candidate_party  %in% c("DEM")) %>%
  mutate(
         dem_party_share = dem_pct/(dem_pct + rep_pct),
         two_party_respondents = sample_size * (dem_pct + rep_pct)/100,
         dem_respondents = floor(two_party_respondents * dem_party_share),
         rep_respondents = floor(two_party_respondents * (1 - dem_party_share)),
         election = "Sen",
         state = `State Code`,
         startDate = mdy(start_date),
         endDate = mdy(end_date),
         electionDate = mdy(election_date),
         pollName = pollster_rating_name,
         year = cycle,
         election = "Sen"
         ) %>%
  filter((electionDate - endDate) <= 21) %>%
  select(year, dem_party_share,
           two_party_respondents,
           dem_respondents, rep_respondents,
           state, startDate, endDate,
           electionDate, pollName,
           race_id,
           candidate_name,
           Region, Division, PADD,
           election) %>%
  ungroup() %>%
  filter(!is.na(dem_party_share)) %>%
  left_join(df_sen_538_outcomes) %>%
  mutate(finalTwoPartyVSRepublican = 1 - finalTwoPartyVSDemocratic) %>%
  filter(!is.na(dem_votes))


df_joined <- bind_rows(df_sen_538_2019, df_sen_538_cleaned) %>%
  mutate(two_party_respondents = floor(two_party_respondents),
         dem_respondents = floor(dem_respondents),
         rep_respondents = floor(rep_respondents))
write_csv(df_joined, "dta/clean_data/polls_sen.csv")



