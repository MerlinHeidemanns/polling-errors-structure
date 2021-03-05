## Libraries
library(tidyverse)
## Load data
df <- read_csv("dta/dirty_data/county_census_rural.csv")
## Mangle
df <- df %>% mutate(
  rural4 = ifelse(`2010 Census \nPercent Rural` < 25, 1,
           ifelse(`2010 Census \nPercent Rural` < 50, 2,
           ifelse(`2010 Census \nPercent Rural` < 75, 3, 4))),
  rural2 = ifelse(`2010 Census \nPercent Rural` < 50,
                  "Less rural (<50%)",
                  "More rural (>=50%)"),
  fips = `2015 GEOID`
) %>%
  select(fips, rural2, rural4, State)
## Write
write_csv(df, "dta/clean_data/county_census_rural.csv")