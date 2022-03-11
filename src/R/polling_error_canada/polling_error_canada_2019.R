################################################################################
## Polling error in Canada for the election in 2019
################################################################################
## Load libraries
library(tidyverse)
library(assertthat)
library(readstata13)
################################################################################
## Load data
df_results_2019 <- read_csv("dta/can/results_2019.csv")
df_results_2015 <- read_csv("dta/can/results_2015.csv")
df_poll    <- read.dta13("dta/can/ces_2019")
################################################################################
## Check load
################################################################################
## Mangle results
## 2019
df_results_2019 <- df_results_2019 %>%
  mutate(party =
           case_when(grepl("Liberal", `Candidate/Candidat`) ~ "Liberal",
                     grepl("Conservative/Conservateur", `Candidate/Candidat`) ~ "Conservative")) %>%
  filter(!is.na(party)) %>%
  rename(percentage = `Percentage of Votes Obtained /Pourcentage des votes obtenus`,
         district_id = `Electoral District Number/Numéro de circonscription`) %>%
  select(district_id, percentage, party) %>%
  group_by(district_id) %>%
  mutate(
    percentage = as.numeric(percentage),
    percentage = percentage/sum(percentage)) %>%
  pivot_wider(id_cols = district_id, names_from = party, values_from = percentage) %>%
  rename(liberal_voteshare_2019 = Liberal,
         conservative_voteshare_2019 = Conservative)
## 2015
df_results_2015 <- df_results_2015 %>%
  mutate(party =
           case_when(grepl("Liberal", `Candidate/Candidat`) ~ "Liberal",
                     grepl("Conservative/Conservateur", `Candidate/Candidat`) ~ "Conservative")) %>%
  filter(!is.na(party)) %>%
  rename(percentage = `Percentage of Votes Obtained /Pourcentage des votes obtenus`,
         district_id = `Electoral District Number/Numéro de circonscription`) %>%
  select(district_id, percentage, party) %>%
  group_by(district_id) %>%
  mutate(
    percentage = as.numeric(percentage),
    percentage = percentage/sum(percentage)) %>%
  pivot_wider(id_cols = district_id, names_from = party, values_from = percentage) %>%
  rename(liberal_voteshare_2015 = Liberal,
         conservative_voteshare_2015 = Conservative)
## Mangle polls
df_poll_2019 <- df_poll %>%
  filter(!is.na(cps19_weight_general_restricted)) %>%
  mutate(vote_choice =
           case_when(cps19_votechoice == "Liberal Party" ~ "Liberal",
                     cps19_votechoice == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other")) %>%
  group_by(constituencynumber) %>%
  summarize(conservative_share_poll =
              sum((vote_choice == "Conservative") *
              cps19_weight_general_restricted)/sum(cps19_weight_general_restricted),
            liberal_share_poll =
              sum((vote_choice == "Liberal") *
              cps19_weight_general_restricted)/sum(cps19_weight_general_restricted),
            N = sum(vote_choice %in% c("Conservative", "Liberal"))) %>%
  ungroup() %>%
  mutate(joint_share_poll = conservative_share_poll + liberal_share_poll,
         liberal_share_poll = liberal_share_poll/joint_share_poll,
         conservative_share_poll = conservative_share_poll/joint_share_poll,
         sd = sqrt(conservative_share_poll * liberal_share_poll/N)) %>%
  rename(district_id = constituencynumber)
## Merge
df <- df_poll_2019 %>%
  left_join(df_results_2019) %>%
  left_join(df_results_2015) %>%
  select(-liberal_voteshare_2019, - liberal_voteshare_2015) %>%
  mutate(polling_error = conservative_voteshare_2019 - conservative_share_poll) %>%
  pivot_longer(c(conservative_voteshare_2015, conservative_voteshare_2019),
               names_to = "year",
               values_to = "voteshare")

write_csv(df, file = "dta/model_output/measurement_error/canada_2019_polling_error.csv")

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)







