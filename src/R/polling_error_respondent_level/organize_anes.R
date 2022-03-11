################################################################################
## Title: Organize ANES data
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
df <- read_csv("dta/anes/anes2020/anes2020.csv")
################################################################################
## Recode
df <- df %>%
  mutate(
    mode = case_when(
      V200002 == 1 ~ "video",
      V200002 == 2 ~ "telephone",
      V200002 == 3 ~ "web"
    ),
    preg = case_when(
      V201018 == 1 ~ "dem",
      V201018 == 2 ~ "rep",
      V201018 == 4 ~ "ind",
      V201018 == 5 ~ "other",
      V201018 <  0 ~ "miss"
    ),
    primary_vote = case_when(
      V201021 %in% seq(1, 7) ~ "dem",
      V201021 %in% c(8,9) ~ "rep",
      V201021 < 0 ~ "miss",
      V201021 > 9 ~ "other"
    ),
    president_vote_already = case_when(
      V201029 == 1 ~ "dem",
      V201029 == 2 ~ "rep",
      V201029 < 0 ~ "miss",
      V201029 > 2 ~ "other"
    ),
    president_vote_intention = case_when(
      V201033 == 1 ~ "dem",
      V201033 == 2 ~ "rep",
      V201033 < 0 ~ "miss",
      V201033 > 2 ~ "other"
    ),
    president_vote = case_when(
      president_vote_already == "dem" | president_vote_intention == "dem" ~ "dem",
      president_vote_already == "rep" | president_vote_intention == "rep" ~ "rep",
      president_vote_already == "other" | president_vote_intention == "other" ~ "other",
      TRUE ~ "miss"
    ),
    pid = case_when(
      V201200 %in% c(1,2) ~ "dem",
      V201200 %in% c(6, 7) ~ "rep",
      V201200 == c(3,4,5) ~ "moderate",
      (V201200 < 0 | V201200 == -99) ~ "NA"
    ),
    difference = case_when(
      pid == "dem" & president_vote == "rep" ~ "dem_rep",
      pid == "rep" & president_vote == "dem" ~ "rep_dem",
      TRUE ~ "other"
    )
  )
table(df$difference, df$primary_vote)


