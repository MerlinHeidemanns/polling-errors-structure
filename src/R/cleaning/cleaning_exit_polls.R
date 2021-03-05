## Libraries
library(tidyverse)
library(pollster)
## Load data
us2004 <- read_por("dta/dirty_data/exit_polls/us2004.por")
## Manipulate
us2004 <- us2004 %>%
  mutate(last_three_weeks = ifelse(
    TIMEPRES %in% c(1,2,3), "yes", "no"))
ct2004 <- crosstab_3way(us2004, STANUM, PRES04, last_three_weeks,
              weight = WGT) %>%
  filter(last_three_weeks == "yes") %>%
  rename(dem = Kerry,
         rep = Bush) %>%
  select(STANUM, dem, rep) %>%
  mutate(year = 2004)
## Load data
us2008 <- read_por("dta/dirty_data/exit_polls/us2008.por")
## Manipulate
us2008 <- us2008 %>%
  mutate(last_three_weeks = ifelse(
    TIME08 %in% c(1,2,3), "yes", "no"))
ct2008 <- crosstab_3way(us2008, STANUM, PRSUS08, last_three_weeks,
                        weight = WEIGHT) %>%
  filter(last_three_weeks == "yes") %>%
  rename(dem = `Barack Obama`,
         rep = `John McCain`) %>%
  select(STANUM, dem, rep) %>%
  mutate(year = 2008)
## Load data
us2012 <- read_por("dta/dirty_data/exit_polls/us2012.por")
## Manipulate
us2012 <- us2012 %>%
  mutate(last_three_weeks = ifelse(
    TIME12 %in% c(1,2,3), "yes", "no"))
ct2012 <- crosstab_3way(us2012, STANUM, PRES, last_three_weeks,
                        weight = WEIGHT) %>%
  filter(last_three_weeks == "yes") %>%
  rename(dem = `Barack Obama`,
         rep = `Mitt Romney`) %>%
  select(STANUM, dem, rep) %>%
  mutate(year = 2012)
## Load data
us2016 <- read_por("dta/dirty_data/exit_polls/us2016.por")
## Manipulate
us2016 <- us2016 %>%
  mutate(last_three_weeks = ifelse(
    TIME16 %in% c(1,2,3), "yes", "no"))
ct2016 <- crosstab_3way(us2016, STANUM, PRES, last_three_weeks,
                        weight = WEIGHT) %>%
  filter(last_three_weeks == "yes") %>%
  rename(dem = `Hillary Clinton`,
         rep = `Donald Trump`) %>%
  select(STANUM, dem, rep) %>%
  mutate(year = 2016)
## Combine
df <- bind_rows(ct2004, ct2008, ct2012, ct2016)
## regions for abbreviations
us_regions <- read_csv("dta/us census bureau regions and divisions.csv")
df <- df %>%
  left_join(us_regions, by = c("STANUM" = "State")) %>%
  rename(state_full = STANUM,
         state_po = `State Code`)
## Out
write.csv(df, "dta/clean_data/exit_polls_late_deciders_three_weeks_less.csv")








