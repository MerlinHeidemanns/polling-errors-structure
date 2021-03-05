## Libraries
library(tidyverse)
## Functions
is_true <- function(a, b, val){
  a[is.na(a)] <- 99
  b[is.na(b)] <- 99
  return(ifelse((a == val) | (b == val), 1, 0))
}
## Load data
df <- read_delim("dta/cces/cces_2016", delim = "\t")
df_counties <- read_csv("dta/clean_data/county_census_rural.csv")
df_returns <- read_csv("dta/clean_data/county_pres_2012.csv")
## Mutate
df_returns <- df_returns %>%
  left_join(df_counties %>% mutate(fips = as.integer(fips)),
            by = c("FIPS" = "fips")) %>%
  group_by(state_po, rural2) %>%
  summarize(dem_share = sum(dem_votes)/sum(two_party_votes))
df_sum <- df %>%
  mutate(vote_rep = is_true(CC16_364b, CC16_364c, 1),
         vote_dem = is_true(CC16_364b, CC16_364c, 2),
         vote_two_party = ifelse((vote_dem == 1) | (vote_rep == 1), 1, 0)) %>%
  left_join(df_counties, by = c("countyfips" = "fips")) %>%
  group_by(State, rural2) %>%
  summarize(mean_dem_estimate = sum(commonweight_vv * vote_dem)/sum(commonweight_vv * vote_two_party),
            mean_rep_estimate = sum(commonweight_vv * vote_rep)/sum(commonweight_vv * vote_two_party))
## Merge
df_out <- df_sum %>%
  left_join(df_returns, by = c("State" = "state_po",
                               "rural2" = "rural2"))
## Save
write_csv(df_out, "dta/clean_data/cces_2016_results_2012_urban2.csv")




