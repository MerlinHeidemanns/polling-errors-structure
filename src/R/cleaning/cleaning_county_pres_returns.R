## Libraries
library(tidyverse)
## Load data
df <- read_delim("dta/dirty_data/countypres_2000-2016.tab", "\t")
## Mangle
df <- df %>%
  filter(year == 2012,
         party %in% c("democrat", "republican")) %>%
  group_by(FIPS) %>%
  mutate(dem_share = candidatevotes/sum(candidatevotes),
         dem_votes = candidatevotes,
         two_party_votes = sum(candidatevotes)) %>%
  filter(party == "democrat") %>%
  select(year, state_po, FIPS, dem_share, dem_votes, two_party_votes)
## Save
write_csv(df, file = "dta/clean_data/county_pres_2012.csv")