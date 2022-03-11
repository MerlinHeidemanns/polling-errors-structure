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
################################################################################
## Remove low quality and inattentive but pattern stays the same
## Mangle polls
df_poll_2019 <- df_poll %>%
  #filter(cps19_data_quality == 0, cps19_inattentive == 0) %>%
  filter(!is.na(cps19_weight_general_all)) %>%
  mutate(vote_choice =
           case_when(cps19_votechoice == "Liberal Party" ~ "Liberal",
                     cps19_votechoice == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other")) %>%
  group_by(constituencynumber) %>%
  summarize(
            conservative_share_poll =
              sum((vote_choice == "Conservative") *
                    cps19_weight_general_all)/sum(cps19_weight_general_all),
            liberal_share_poll =
              sum((vote_choice == "Liberal") *
                    cps19_weight_general_all)/sum(cps19_weight_general_all),
            # conservative_share_poll =
            #   mean(vote_choice == "Conservative"),
            # liberal_share_poll =
            #   mean(vote_choice == "Liberal"),
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

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)

################################################################################
## Varying likely voter threshold doesn't seem to do much either
## Mangle polls
categories <- c("Certain to vote", "Likely to vote", "Unlikely to vote", "Certain not to vote")
df <- lapply(1:4, function(x){
  df_poll_2019 <- df_poll %>%
    #filter(cps19_data_quality == 0, cps19_inattentive == 0) %>%
    filter(cps19_v_likely %in% categories[1:x]) %>%
    filter(!is.na(cps19_weight_general_all)) %>%
    mutate(vote_choice =
             case_when((cps19_votechoice == "Liberal Party" |
                        cps19_v_advance == "Liberal Party"  |
                        cps19_vote_unlikely == "Liberal Party") ~ "Liberal",
                       (cps19_votechoice == "Conservative Party" |
                        cps19_v_advance == "Conservative"        |
                        cps19_vote_unlikely == "Conservative") ~ "Conservative",
                       TRUE ~ "Other")) %>%
    filter(vote_choice %in% c("Liberal", "Conservative")) %>%
    group_by(constituencynumber, cps19_province) %>%
    summarize(conservative_share_poll =
                sum((vote_choice == "Conservative") *
                      cps19_weight_general_all)/sum(cps19_weight_general_all),
              liberal_share_poll =
                sum((vote_choice == "Liberal") *
                      cps19_weight_general_all)/sum(cps19_weight_general_all),
              N = sum(vote_choice %in% c("Conservative", "Liberal"))) %>%
    ungroup() %>%
    mutate(joint_share_poll = conservative_share_poll + liberal_share_poll,
           liberal_share_poll = liberal_share_poll/joint_share_poll,
           conservative_share_poll = conservative_share_poll/joint_share_poll,
           sd = sqrt(conservative_share_poll * liberal_share_poll/N)) %>%
    rename(district_id = constituencynumber) %>%
    filter(N > 5)
  ## Merge
  df <- df_poll_2019 %>%
    left_join(df_results_2019) %>%
    left_join(df_results_2015) %>%
    select(-liberal_voteshare_2019, - liberal_voteshare_2015) %>%
    mutate(polling_error = conservative_voteshare_2019 - conservative_share_poll) %>%
    pivot_longer(c(conservative_voteshare_2015, conservative_voteshare_2019),
                 names_to = "year",
                 values_to = "voteshare") %>%
    select(voteshare, polling_error, sd, year, cps19_province) %>%
    mutate(inclusion_up_to = categories[x])
  return(df)
}) %>%
  do.call("bind_rows", .)

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  #geom_smooth(method = "lm", aes(color = cps19_province), se = 0) +
  geom_smooth(method = "loess", se = 0) +
  # geom_smooth(method = "loess", aes(color = cps19_province), se = 0) +
  facet_grid(year ~ inclusion_up_to) +
  geom_abline(aes(intercept = -1, slope = 1)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  coord_cartesian()



################################################################################
## Post election vs pre-election
#' Reduction in slope and less distinguishable from zero
## Mangle polls
df_poll_2019 <- df_poll %>%
  filter(!is.na(pes19_weight_general_restricted)) %>%
  mutate(vote_choice =
           case_when(pes19_votechoice2019 == "Liberal Party" ~ "Liberal",
                     pes19_votechoice2019 == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other")) %>%
  group_by(constituencynumber) %>%
  summarize(conservative_share_poll =
              sum((vote_choice == "Conservative") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
            liberal_share_poll =
              sum((vote_choice == "Liberal") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
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
               values_to = "voteshare") %>%
  filter(sd != 0)

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)

summary(lm(polling_error ~ voteshare:year, data = df))




################################################################################
## Post election vs pre-election
#' Reduction in slope and less distinguishable from zero
## Mangle polls
df_poll_2019 <- df_poll %>%
  filter(!is.na(pes19_weight_general_restricted)) %>%
  filter(!pes19_votechoice2019 %in% c("Don't know / Prefer not to answer",
                                      "I spoiled my vote")) %>%
  mutate(vote_choice_post =
           case_when(pes19_votechoice2019 == "Liberal Party" ~ "Liberal",
                     pes19_votechoice2019 == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other"),
         vote_choice_pre =
           case_when(cps19_votechoice == "Liberal Party" ~ "Liberal",
                     cps19_votechoice == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other"),
         vote_choice_change = as.integer(vote_choice_post != vote_choice_pre)) %>%
  filter(vote_choice_post != "Other")
cat("Switch share:", mean(df_poll_2019$vote_choice_change))
df_poll_2019 %>%
  filter(vote_choice_post != "Other",
         vote_choice_pre != "Other") %>%
  group_by(vote_choice_pre, vote_choice_post) %>%
  summarize(n = n())

df_poll_2019 <- df_poll_2019 %>%
  filter(vote_choice_pre == vote_choice_post) %>%
  group_by(constituencynumber) %>%
  summarize(conservative_share_poll =
              sum((vote_choice_pre == "Conservative") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
            liberal_share_poll =
              sum((vote_choice_pre == "Liberal") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
            N = sum(vote_choice_pre %in% c("Conservative", "Liberal"))) %>%
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
               values_to = "voteshare") %>%
  filter(sd != 0)

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)








################################################################################
## Comparing inattentive to attentive respondents
################################################################################


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
## Mangle polls ---- ALL
df_poll_2019 <- df_poll %>%
  filter(!is.na(cps19_weight_general_all)) %>%
  mutate(vote_choice =
           case_when(cps19_votechoice == "Liberal Party" ~ "Liberal",
                     cps19_votechoice == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other")) %>%
  group_by(constituencynumber) %>%
  summarize(conservative_share_poll =
              sum((vote_choice == "Conservative") *
                    cps19_weight_general_all)/sum(cps19_weight_general_all),
            liberal_share_poll =
              sum((vote_choice == "Liberal") *
                    cps19_weight_general_all)/sum(cps19_weight_general_all),
            N = sum(vote_choice %in% c("Conservative", "Liberal"))) %>%
  ungroup() %>%
  mutate(joint_share_poll = conservative_share_poll + liberal_share_poll,
         liberal_share_poll = liberal_share_poll/joint_share_poll,
         conservative_share_poll = conservative_share_poll/joint_share_poll,
         sd = sqrt(conservative_share_poll * liberal_share_poll/N)) %>%
  rename(district_id = constituencynumber)
## Merge
df_all <- df_poll_2019 %>%
  left_join(df_results_2019) %>%
  left_join(df_results_2015) %>%
  select(-liberal_voteshare_2019, - liberal_voteshare_2015) %>%
  mutate(polling_error = conservative_voteshare_2019 - conservative_share_poll) %>%
  pivot_longer(c(conservative_voteshare_2015, conservative_voteshare_2019),
               names_to = "year",
               values_to = "voteshare") %>%
  mutate(kind = "All")


## Mangle polls ---- ATTENTIVE
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
df_attentive <- df_poll_2019 %>%
  left_join(df_results_2019) %>%
  left_join(df_results_2015) %>%
  select(-liberal_voteshare_2019, - liberal_voteshare_2015) %>%
  mutate(polling_error = conservative_voteshare_2019 - conservative_share_poll) %>%
  pivot_longer(c(conservative_voteshare_2015, conservative_voteshare_2019),
               names_to = "year",
               values_to = "voteshare") %>%
  mutate(kind = "Attentive")

df <- bind_rows(
  df_all,
  df_attentive
) %>%
  mutate(
    year = case_when(
      year == "conservative_voteshare_2015" ~ "Conservative vote share (2015)",
      year == "conservative_voteshare_2019" ~ "Conservative vote share (2019)"
    )
  )

write_csv(df, file = "dta/model_output/can_polling_error_attentive.csv")
ggplot(df %>%
         filter(sd != 0), aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  # geom_errorbar(aes(ymin = polling_error - sd,
  #                   ymax = polling_error + sd),
  #               size = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0.5), linetype = 2) +
  geom_smooth(method = "lm", se = 0, color = "blue") +
  facet_grid(year ~ kind) +
  theme_light() +
  labs(x = "Conservative two-party vote share",
       y = "Polling error")


################################################################################

## Post election vs pre-election
#' Reduction in slope and less distinguishable from zero
## Mangle polls
df_poll_2019 <- df_poll %>%
  filter(!is.na(pes19_weight_general_restricted)) %>%
  filter(!pes19_votechoice2019 %in% c("Don't know / Prefer not to answer",
                                      "I spoiled my vote")) %>%
  mutate(vote_choice_post =
           case_when(pes19_votechoice2019 == "Liberal Party" ~ "Liberal",
                     pes19_votechoice2019 == "Conservative Party" ~ "Conservative",
                     TRUE ~ "Other"))


df_poll_2019 <- df_poll_2019 %>%
  group_by(constituencynumber) %>%
  summarize(conservative_share_poll =
              sum((vote_choice_post == "Conservative") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
            liberal_share_poll =
              sum((vote_choice_post == "Liberal") *
                    pes19_weight_general_restricted)/sum(pes19_weight_general_restricted),
            N = sum(vote_choice_post %in% c("Conservative", "Liberal"))) %>%
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
  select(-liberal_voteshare_2019, -liberal_voteshare_2015) %>%
  mutate(polling_error = conservative_voteshare_2019 - conservative_share_poll) %>%
  pivot_longer(c(conservative_voteshare_2015, conservative_voteshare_2019),
               names_to = "year",
               values_to = "voteshare") %>%
  filter(sd != 0)

ggplot(df, aes(x = voteshare, y = polling_error)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = polling_error - sd,
                    ymax = polling_error + sd),
                size = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm") +
  facet_wrap(year ~ .)



