################################################################################
## ANES error
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
df <- read_csv("dta/anes/anes_timeseries/anes_timeseries.csv")
################################################################################
## Mangle
df %>%
  select(VCF0704, VCF0009z, VCF0901a, VCF0004) %>%
  mutate(
    vote_choice = case_when(
      VCF0704 == 1 ~ "Democrat",
      VCF0704 == 2 ~ "Republican",
      TRUE ~ "no vote or other"
    ),
    weight = VCF0009z,
    state = VCF0901a,
    year = VCF0004
  ) %>%
  select(vote_choice, weight, state, year)

