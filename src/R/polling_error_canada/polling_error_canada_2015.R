################################################################################
## Polling error in Canada for the election in 2015
################################################################################
## Citations
# Fournier, Patrick, Fred Cutler, Stuart Soroka and Dietlind Stolle.
# 2015. The 2015 Canadian Election Study. [dataset]
################################################################################
## Load libraries
library(tidyverse)
library(assertthat)
################################################################################
## Load data
df_results <- read_csv("dta/can/results_2015.csv")
df_poll    <- read.dta13("dta/can/ces_2015")
################################################################################
## Mangle results
df_results_2015 <- df_results %>%
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
  rename(liberal_voteshare = Liberal,
         conservative_voteshare = Conservative)
## poll data
df_poll_2019 <- df_poll %>%
  filter(mode == 1)
  filter(!is.na(NatWgtv)) %>%
  mutate(vote_choice =
           case_when(vote_for == 1 | vote_lean == 1 ~ "Liberal",
                     vote_for == 2 | vote_lean == 2 ~ "Conservative",
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


