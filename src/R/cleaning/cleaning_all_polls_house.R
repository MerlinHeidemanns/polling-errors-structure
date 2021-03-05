## Libraries
library(tidyverse)
library(lubridate)
## Import categorization
names <- read_csv("dta/us census bureau regions and divisions.csv")
## Import data
df_house_538_pollster_ratings <- read_csv("dta/dirty_data/polls_538_pollster_ratings.csv")
## Manipulate 538 data
df_house_538 <- df_house_538_pollster_ratings %>%
  filter((cand1_party == "DEM") & (cand2_party == "REP"),
         type_simple == "House-G",
         location != "US") %>%
  mutate(
    dem_party_share = cand1_pct/(cand1_pct + cand2_pct),
    two_party_respondents = samplesize * (cand1_pct + cand2_pct)/100,
    dem_respondents = floor(two_party_respondents * dem_party_share),
    rep_respondents = floor(two_party_respondents * (1 - dem_party_share)),
    state = str_remove(location, "[^A-Z]+"),
    election = "House",
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
         Region, Division, PADD) %>%
  mutate(two_party_respondents = floor(two_party_respondents),
         dem_respondents = floor(dem_respondents),
         rep_respondents = floor(rep_respondents))
write_csv(df_house_538, "dta/clean_data/polls_house_538.csv")



