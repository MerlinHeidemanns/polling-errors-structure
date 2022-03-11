################################################################################
## Polling error in Germany under Proportional Representation
################################################################################
## Note
#' Consider that the error could be observed if voters of the less party
#' are disincentivized to vote beyond their self-reported statement as
#' to their likelihood of voting.
#' Question: Adjusting vote probability by past vote share of intended party,
#' does this fix it?
################################################################################
## Libraries
library(tidyverse)
library(boot)
library(cmdstanr)
################################################################################
## Flag
sample_size_add = TRUE
################################################################################
## Conditions under which if the previous voteshare affects one's likelhood to
## vote would create the pattern
N <- 1e5
G <- 100
g <- sample(1:G, N, replace = TRUE)
mu_g <- seq(0.01, 0.99, length.out = G)

y <- rbinom(N, 1, inv.logit(ifelse(rbinom(N, 1, mu_g[g]), rnorm(N, 2, 1), rnorm(N, -2, 1))))
df <- data.frame(y = y, g = g) %>%
  group_by(g) %>%
  mutate(mean_y = mean(y),
         vote = rbinom(n(), 1, ifelse(y == 1, mean_y, 1 - mean_y)),
         observed_vote = ifelse(vote, y, NA))
df %>%
  group_by(g) %>%
  summarize(sample_mean = mean(y),
            vote_mean = mean(observed_vote, na.rm = TRUE)) %>%
  ggplot(aes(x = vote_mean, y = vote_mean - sample_mean)) +
  geom_point()
################################################################################
## Germany nationally
print("Germany")
df_elections <- read_csv("dta/de/polls_states.csv", col_types = cols()) %>%
  select(-Sonstige, -X13, -`Auftrag-`) %>%
  filter(grepl("Bundestagswahl", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(-state, - Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
  # pivot_wider(id_cols = c(state, date),
  #             names_from = party,
  #             values_from = percent)
df_polls <- read_csv("dta/de/polls_states.csv", col_types = cols()) %>%
  filter(!grepl("Bundestagswahl", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", "")) %>%
  select(-Sonstige, -X13, -`Auftrag-`, -Datum, -Befragte) %>%
  pivot_longer(c(-state, -Institut, -date, -sample),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date) # %>%
  # pivot_wider(id_cols = c(state, Institut, date, sample),
  #             names_from = party,
  #             values_from = percent)

df <- df_polls %>%
  full_join(df_elections, by = c("state", "party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by(state, Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU")

ggplot(data = df, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
## Germany state by state
### Baden-Wuertemberg
df_bw_elections <- read_csv("dta/de/polls_baden_wuertemberg.csv", col_types = cols()) %>%
  select(-Sonstige, -X5, -Auftraggeber) %>%
  filter(grepl("Landtagswahl", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_bw_polls <- read_csv("dta/de/polls_baden_wuertemberg.csv", col_types = cols()) %>%
  filter(!grepl("Landtagswahl", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-Sonstige, -X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_bw <- df_bw_polls %>%
  full_join(df_bw_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Baden Wuertemberg")

ggplot(data = df_bw_polls, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

###############################################################################
### Schleswig Holstein
df_sh_elections <- read_csv("dta/de/polls_schleswig_holstein.csv", col_types = cols()) %>%
  select(-X5, -Auftraggeber) %>%
  filter(grepl("Landtagswahl", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_sh_polls <- read_csv("dta/de/polls_schleswig_holstein.csv", col_types = cols()) %>%
  filter(!grepl("Landtagswahl", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_sh <- df_sh_polls %>%
  full_join(df_sh_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Schleswig Holstein")

ggplot(data = df_sh, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")
###############################################################################
### Bayern
df_by_elections <- read_csv("dta/de/polls_bayern.csv", col_types = cols()) %>%
  select(-X5, -Auftraggeber) %>%
  filter(grepl("Landtagswahl", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_by_polls <- read_csv("dta/de/polls_bayern.csv", col_types = cols()) %>%
  filter(!grepl("Landtagswahl", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_by <- df_by_polls %>%
  full_join(df_by_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CSU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CSU") %>%
  mutate(state = "Bavaria")


ggplot(data = df_by, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")
###############################################################################
### Hessen
df_hs_elections <- read_csv("dta/de/polls_hessen.csv", col_types = cols()) %>%
  select(-X5, -Auftraggeber) %>%
  filter(grepl("Landtagswahl", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_hs_polls <- read_csv("dta/de/polls_hessen.csv", col_types = cols()) %>%
  filter(!grepl("Landtagswahl", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_hs <- df_hs_polls %>%
  full_join(df_hs_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Hessen")


ggplot(data = df_hs, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")
###############################################################################
### Hamburg
df_hh_elections <- read_csv("dta/de/polls_hh.csv", col_types = cols()) %>%
  select(-X12, -Auftraggeber, -X12) %>%
  filter(grepl("Bürgersc", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_hh_polls <- read_csv("dta/de/polls_hh.csv", col_types = cols()) %>%
  filter(!grepl("Bürgersc", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -X12, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_hh <- df_hh_polls %>%
  full_join(df_hh_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Hamburg")


ggplot(data = df_hh, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")
###############################################################################
### Bremen
df_bm_elections <- read_csv("dta/de/polls_bremen.csv", col_types = cols()) %>%
  select(-Auftraggeber) %>%
  filter(grepl("Bürgersc", Institut)) %>%
  select(-Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_bm_polls <- read_csv("dta/de/polls_bremen.csv", col_types = cols()) %>%
  filter(!grepl("Bürgersc", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_bm <- df_bm_polls %>%
  full_join(df_bm_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Bremen")


ggplot(data = df_bm, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")
###############################################################################
### Brandenburg
df_bb_elections <- read_csv("dta/de/polls_brandenburg.csv", col_types = cols()) %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_bb_polls <- read_csv("dta/de/polls_brandenburg.csv", col_types = cols()) %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_bb <- df_bb_polls %>%
  full_join(df_bm_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Brandenburg")

ggplot(data = df_bb, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
### Berlin
df_berlin_elections <- read_csv("dta/de/polls_berlin.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Abgeordnet", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_berlin_polls <- read_csv("dta/de/polls_berlin.csv") %>%
  filter(!grepl("Abgeordnet", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_berlin <- df_berlin_polls %>%
  full_join(df_berlin_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Berlin")

ggplot(data = df_berlin, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
### Thueringen
df_th_elections <- read_csv("dta/de/polls_thueringen.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_th_polls <- read_csv("dta/de/polls_thueringen.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_th <- df_th_polls %>%
  full_join(df_th_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Thueringen")

ggplot(data = df_th, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
### Thueringen
df_sa_elections <- read_csv("dta/de/polls_saarland.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_sa_polls <- read_csv("dta/de/polls_saarland.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_sa <- df_sa_polls %>%
  full_join(df_sa_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Sachsen Anhalt")

ggplot(data = df_sa, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

###############################################################################
### Sachsen
df_sachsen_elections <- read_csv("dta/de/polls_sachsen.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut, -X12, -X13)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_sachsen_polls <- read_csv("dta/de/polls_sachsen.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte, -X12, -X13) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_sachsen <- df_sachsen_polls %>%
  full_join(df_sachsen_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Sachsen")

ggplot(data = df_sachsen, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
### Rheinland-Pfalz
df_rp_elections <- read_csv("dta/de/polls_rheinland_pfalz.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_rp_polls <- read_csv("dta/de/polls_rheinland_pfalz.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample),
               names_to = "party",
               values_to = "percent", -mode) %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_rp <- df_rp_polls %>%
  full_join(df_rp_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Rheinland Pfalz")

ggplot(data = df_rp, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

###############################################################################
### Baden Wuertembergz
df_mv_elections <- read_csv("dta/de/polls_mv.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte, -X11) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_mv_polls <- read_csv("dta/de/polls_mv.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte, -X11) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_mv <- df_mv_polls %>%
  full_join(df_rp_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Mecklenburg Vorpommern")

ggplot(data = df_mv, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")


###############################################################################
### Niedersachsen
df_niedersachsen_elections <- read_csv("dta/de/polls_niedersachsen.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_niedersachsen_polls <- read_csv("dta/de/polls_niedersachsen.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte, -mode) %>%
  pivot_longer(c(-Institut, -date, -sample),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_niedersachsen <- df_niedersachsen_polls %>%
  full_join(df_niedersachsen_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Niedersachsen")

ggplot(data = df_niedersachsen, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

###############################################################################
### Saarland
df_saarland_elections <- read_csv("dta/de/polls_saarland.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_saarland_polls <- read_csv("dta/de/polls_saarland.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",
           grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "",
                         mean(sample, na.rm = TRUE),
                         sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte, -mode) %>%
  pivot_longer(c(-Institut, -date, -sample),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_saarland <- df_saarland_polls %>%
  full_join(df_saarland_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Saarland")

ggplot(data = df_saarland, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

###############################################################################
### Mecklenburg Vorpommern
print("Mecklenburg-Vorpommern")
df_nrw_elections <- read_csv("dta/de/polls_nrw.csv") %>%
  select(-Auftraggeber) %>%
  filter(grepl("Landtags", Institut)) %>%
  select(-X5, -Datum, -Befragte) %>%
  pivot_longer(c(- Institut),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = str_replace(percent, "\\s", "."),
         percent = str_replace(percent, "%", ""),
         percent = as.numeric(percent),
         date = as.Date(str_match(Institut, "am\\s(.+)")[,2], "%d.%m.%Y")) %>%
  select(-Institut) %>%
  rename(percent_election = percent,
         date_election = date)
df_nrw_polls <- read_csv("dta/de/polls_nrw.csv") %>%
  filter(!grepl("Landtags", Institut)) %>%
  mutate(Datum = str_replace_all(Datum, "\\(|\\)", ""),
         date = as.Date(Datum, "%d.%m.%Y"),
         sample = str_replace_all(Befragte, "([^\\d])", ""),
         mode = case_when(
           grepl("TOM", Befragte) ~ "TOM",
           grepl("O", Befragte) ~ "O",
           grepl("T", Befragte) ~ "T",            grepl("F", Befragte) ~ "F"
         ),
         sample = ifelse(sample_size_add & sample == "", mean(sample, na.rm = TRUE), sample)) %>%
  select(-X5, -Auftraggeber, -Datum, -Befragte) %>%
  pivot_longer(c(-Institut, -date, -sample, -mode),
               names_to = "party",
               values_to = "percent") %>%
  mutate(percent = as.numeric(str_match(percent, "([\\d]+)")[,2])) %>%
  rename(percent_polls = percent,
         date_polls = date)


df_nrw <- df_nrw_polls %>%
  full_join(df_nrw_elections, by = c("party")) %>%
  mutate(diff_time = difftime(date_election, date_polls)) %>%
  filter(diff_time > 0, diff_time < 60) %>%
  group_by( Institut, date_polls) %>%
  filter(party %in% c("SPD", "CDU")) %>%
  mutate(sample = as.numeric(sample) * sum(percent_polls)/100,
         percent_polls = percent_polls/sum(percent_polls),
         percent_election = percent_election/sum(percent_election)) %>%
  filter(party == "CDU") %>%
  mutate(state = "Nord Rhein Westphalen")

ggplot(data = df_nrw, aes(x = percent_election, y = percent_election - percent_polls)) +
  geom_point() +
  geom_smooth(method = "lm")

################################################################################
## Joint
df <- df_sh %>%
  bind_rows(
    df_bw,
    df_berlin,
    df_bb,
    df_nrw,
    df_hs,
    df_th,
    df_bm,
    df_nrw,
    df_by,
    df_sa,
    df_sachsen,
    df_hh,
    df_niedersachsen,
    df_saarland,
    df_mv,
    df_rp
  ) %>%
  mutate(polling_error = percent_election - percent_polls) %>%
  filter(abs(polling_error) < 0.2)
ggplot(data = df %>% filter(!is.na(mode)), aes(x = percent_election, y = percent_election - percent_polls,
                      color = mode)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(mode ~ .)

################################################################################
## Model
library(cmdstanr)

mod <- cmdstan_model("src/stan/polling_error_germany/polling_error_germany.stan")

df <- df %>%
  filter(!is.na(sample)) %>%
  group_by(state, date_election) %>%
  mutate(id = cur_group_id(),
         sample = as.integer(sample),
         year = format(date_polls, "%Y")) %>%
  ungroup() %>%
  mutate(
    year_id = as.integer(factor(year))
  )

df_election <- df %>%
  distinct(id, percent_election, year) %>%
  arrange(id)

data_list <- list(
  N = nrow(df),
  E = nrow(df_election),
  T = df$year %>% unique() %>% length(),
  n = df %>% pull(sample),
  y = df %>% mutate(y = as.integer(percent_polls * sample)) %>% pull(y),
  idx_e = df %>% pull(id),
  idx_t = df %>% pull(year_id),
  mu = df_election %>% pull(percent_election)
)

fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 1000,
  iter_warmup = 1000,
  parallel_chains = 6,
  refresh = 500,
  init = 0
)


polling_error <- fit$summary("polling_error", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(id = as.integer(str_match(variable, "([\\d]+)")[,2])) %>%
  left_join(df_election) %>%
  mutate(
    year = case_when(
      year < 2010 ~ "Before 2010",
      TRUE ~ "After 2010"
    )
  )

write_csv(polling_error, file = "dta/model_output/de_polling_error.csv")

ggplot(data = polling_error, aes(x = percent_election, y = `50%`)) +
  geom_point(aes(color = year)) +
  geom_smooth(aes(color = year), method = "lm", se = 0) +
  geom_smooth(method = "lm", se = 0) +
  labs(x = "CDU/CSU two-party vote share",
       y = "Polling error") +
  theme_light() +
  theme(legend.title = element_blank())






















