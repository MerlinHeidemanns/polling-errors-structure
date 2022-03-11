## Libraries
library(lubridate)
library(tidyverse)
## Load data
df <- read_csv("dta/dirty_data/polls_538_00_20_raw.csv")
results <- read_csv("dta/potus_results_76_20.csv")
state_abb_list <- c(unique(results$state_po), "US")
election_dates <- data.frame(
  year = seq(2000, 2020, 4),
  electionDate =
    mdy(
      c(
        "11-07-2000",
        "11-02-2004",
        "11-04-2008",
        "11-06-2012",
        "11-08-2016",
        "11-03-2020"
      )
    )
)
## Right format
df <- df %>%
  filter(grepl("Pres-G", type_detail),
         location %in% state_abb_list) %>%
  mutate(democratic = as.integer(cand1_pct/100 * samplesize),
         republican = as.integer(cand2_pct/100 * samplesize),
         numberOfRespondents = democratic + republican,
         finalTwoPartyVSDemocratic = cand1_actual/(cand1_actual + cand2_actual),
         finalTwoPartyVSRepublican = cand2_actual/(cand1_actual + cand2_actual)
  ) %>%
  rename(state = location,
         pollName = pollster,
         endDate = polldate,
         electionDate = electiondate) %>%
  dplyr::select(
    state, year, pollName, methodology,
    numberOfRespondents, republican, democratic,
    finalTwoPartyVSDemocratic, finalTwoPartyVSRepublican,
    endDate, electionDate
  )
## Save
write_csv(df, file = "dta/clean_data/polls_538_00_20.csv")
