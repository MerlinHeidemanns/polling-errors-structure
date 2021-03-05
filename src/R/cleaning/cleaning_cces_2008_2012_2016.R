## Libraries
library(tidyverse)
## Functions
is_true <- function(a, b, val){
  a[is.na(a)] <- 99
  b[is.na(b)] <- 99
  return(ifelse((a == val) | (b == val), 1, 0))
}
## Load data
df_2008 <- read_csv("dta/cces/cces_2008.csv")
df_2012 <- read_delim("dta/cces/cces_2012.tab", delim = "\t")
df_2016 <- read_delim("dta/cces/cces_2016", delim = "\t")
df_results <- read_csv("dta/potus_results_76_20.csv")
df_counties <- read_csv("dta/clean_data/county_census_rural.csv")
df_regions <- read_csv("dta/us census bureau regions and divisions.csv")
## Mutate
df_results <- df_results %>%
  mutate(dem_share = dem/(dem + rep)) %>%
  select(year, dem_share, state_po)
## Polls
fips_table <- sort(unique(df_2008$V206))
state_names <- sort(df_regions$State)
df_2008_adj <- df_2008 %>%
  mutate(weight = V201,
         State = state_names[match(V206, fips_table)],
         vote_dem = is_true(CC326b, CC327, 2),
         vote_rep = is_true(CC326b, CC327, 1),
         vote_two_party = ifelse((vote_dem == 1) | (vote_rep == 1), 1, 0)) %>%
  select(weight, State, vote_dem, vote_rep, vote_two_party) %>%
  group_by(State) %>%
  summarize(mean_dem_estimate = sum(weight * vote_dem)/sum(weight * vote_two_party),
            mean_rep_estimate = sum(weight * vote_rep)/sum(weight * vote_two_party),
            N = sum(vote_two_party)) %>%
  mutate(se = sqrt(mean_dem_estimate * mean_rep_estimate/N)) %>%
  left_join(df_regions) %>%
  mutate(year = 2008,
         prev_year = 2004)
## 2012
df_2012_adj <- df_2012 %>%
  mutate(weight = V103,
         State = state_names[match(inputstate, fips_table)],
         vote_dem = is_true(CC354b, CC354c, 2),
         vote_rep = is_true(CC354b, CC354c, 1),
         vote_two_party = ifelse((vote_dem == 1) | (vote_rep == 1), 1, 0)) %>%
  select(weight, State, vote_dem, vote_rep, vote_two_party) %>%
  group_by(State) %>%
  summarize(mean_dem_estimate = sum(weight * vote_dem)/sum(weight * vote_two_party),
            mean_rep_estimate = sum(weight * vote_rep)/sum(weight * vote_two_party),
            N = sum(vote_two_party)) %>%
  mutate(se = sqrt(mean_dem_estimate * mean_rep_estimate/N)) %>%
  left_join(df_regions) %>%
  mutate(year = 2012,
         prev_year = 2008)
## 2016
df_2016_adj <- df_2016 %>%
  mutate(weight = commonweight_vv,
         State = state_names[match(inputstate, fips_table)],
         vote_dem = is_true(CC16_364b, CC16_364c, 2),
         vote_rep = is_true(CC16_364b, CC16_364c, 1),
         vote_two_party = ifelse((vote_dem == 1) | (vote_rep == 1), 1, 0)) %>%
  select(weight, State, vote_dem, vote_rep, vote_two_party) %>%
  group_by(State) %>%
  summarize(mean_dem_estimate = sum(weight * vote_dem)/sum(weight * vote_two_party),
            mean_rep_estimate = sum(weight * vote_rep)/sum(weight * vote_two_party),
            N = sum(vote_two_party)) %>%
  mutate(se = sqrt(mean_dem_estimate * mean_rep_estimate/N)) %>%
  left_join(df_regions) %>%
  mutate(year = 2016,
         prev_year = 2012)
## Bind
df_cces <- df_2008_adj %>%
  bind_rows(df_2012_adj) %>%
  bind_rows(df_2016_adj) %>%
  rename(state_po = `State Code`) %>%
  left_join(df_results,
            by = c("state_po" = "state_po",
                   "prev_year" = "year")) %>%
  rename(prev_dem_share = dem_share) %>%
  left_join(df_results,
            by = c("state_po" = "state_po",
                   "year" = "year")) %>%
  rename(curr_dem_share = dem_share) %>%
  mutate(error_dem = mean_dem_estimate - curr_dem_share)
## Write
write_csv(df_cces, "dta/clean_data/cces_2008_2012_2016.csv")









df_2016_adj <- df_2016 %>%
  mutate(weight = commonweight_vv,
         State = state_names[match(inputstate, fips_table)],
         vote_dem = is_true(CC16_364b, CC16_364c, 2),
         vote_rep = is_true(CC16_364b, CC16_364c, 1),
         vote_any = any(is_true(CC16_364b, CC16_364c, 1),
                        is_true(CC16_364b, CC16_364c, 2),
                        is_true(CC16_364b, CC16_364c, 3),
                        is_true(CC16_364b, CC16_364c, 4),
                        is_true(CC16_364b, CC16_364c, 5),
                        is_true(CC16_364b, CC16_364c, 7)
         ) %>%
           filter(vote_any) %>%
           select(weight, State, vote_dem, vote_rep, vote_two_party) %>%
           group_by(State) %>%
           summarize(mean_dem_estimate = sum(weight * vote_dem)/sum(weight * vote_two_party),
                     mean_rep_estimate = sum(weight * vote_rep)/sum(weight * vote_two_party),
                     N = sum(vote_two_party)) %>%
           mutate(se = sqrt(mean_dem_estimate * mean_rep_estimate/N)) %>%
           left_join(df_regions) %>%
           mutate(year = 2016,
                  prev_year = 2012)




