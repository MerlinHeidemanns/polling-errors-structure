## Libraries
library(boot)
library(tidyverse)
## Load data
df <- read_csv("dta/dirty_data/polls_pres_dataset_00_20.csv")
results <- read_csv("dta/potus_results_76_20.csv")
us_regions <- read_csv("dta/us census bureau regions and divisions.csv")
## Wrangle data
state_abb <- df %>%
  pull(state) %>%
  unique() %>%
  sort()
df <- df %>%
  group_by(state, electionDate) %>%
  mutate(i = cur_group_id(),
         s = match(state, state_abb)) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(t = cur_group_id(),
         outcome = finalTwoPartyVSDemocratic / 100,
         n = floor((republican + democratic)/100 * numberOfRespondents),
         y = floor(democratic/100 * numberOfRespondents)) %>%
  left_join(us_regions, by = c("state" = "State Code")) %>%
  group_by(Region) %>%
  mutate(r = cur_group_id()) %>%
  group_by(Division) %>%
  mutate(d = cur_group_id()) %>%
  group_by(Region, year) %>%
  mutate(rt = cur_group_id()) %>%
  group_by(Division, year) %>%
  mutate(dt = cur_group_id()) %>%
  ungroup() %>%
  left_join(data.frame(x = 1:300,
                       t = sort(rep(1:6, 50)),
                       s = rep(1:50, 6)))
## Indexes
rt_index <- df %>% distinct(r, t, rt) %>% arrange(r, t)
dt_index <- df %>% distinct(d, t, dt) %>% arrange(d, t)
r_index <- df %>% distinct(s, r) %>% arrange(s) %>% pull(r)
d_index <- df %>% distinct(s, d) %>% arrange(s) %>% pull(d)
indexes <- data.frame(x = 1:300,
                      t = sort(rep(1:6, 50)),
                      s = rep(1:50, 6)
) %>%
  mutate(r = r_index[s],
         d = d_index[s],
         dt = NA,
         rt = NA)
for (i in 1:nrow(indexes)){
  indexes$rt[i] <- rt_index %>% filter(r == indexes$r[i],
                                       t == indexes$t[i]) %>%
    pull(rt)
  indexes$dt[i] <- dt_index %>% filter(d == indexes$d[i],
                                       t == indexes$t[i]) %>%
    pull(dt)
}
## Write
write_csv(df, "dta/clean_data/polls_pres_prepared.csv")
write_csv(indexes, "dta/clean_data/index_pres_prepared.csv")







