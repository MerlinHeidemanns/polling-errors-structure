library(boot)
library(cmdstanr)
library(tidyverse)
df <- read_csv("data/us_input/polling_error/polls_pres_dataset_00_20.csv")
results <- read_csv("data/us_background/potus_results_76_20.csv") %>%
  left_join(df %>% distinct(state), by = c("state_po" = "state"))
states_2020_ordered <- results %>%
  filter(year == 2020) %>%
  mutate(pos = dem/(dem + rep)) %>%
  arrange(pos) %>%
  pull(state_po)
results_2020 <- results %>%
  filter(year == 2020) %>%
  mutate(finalTwoPartyVSRepublican = rep/(dem + rep) * 100,
         finalTwoPartyVSDemocratic = dem/(dem + rep) * 100) %>%
  dplyr::select(-year, -State) %>%
  left_join(df %>% distinct(state, State), by = c("state_po" = "state"))
states_2020_ordered_lower <- results_2020 %>% filter(!is.na(State)) %>%
  arrange(finalTwoPartyVSDemocratic) %>% pull(State)
us_regions <- read_csv("data/us_input/polling_error/us census bureau regions and divisions.csv")
###############################################################################
## Model for scales
m <- file.path("code/stan/obs/input/polling_error/polling_error_diff_covariance_matrix", "polling_error_diff_covariance_matrix.stan")
mod <- cmdstan_model(m)
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
  left_join(data.frame(x = 1:300,
                       t = sort(rep(1:6, 50)),
                       s = rep(1:50, 6)))

indexes <- data.frame(x = 1:300,
                      t = sort(rep(1:6, 50)),
                      s = rep(1:50, 6)
)

data_list <- list(
  N = nrow(df),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
  corr_x = indexes %>% pull(x)
)
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)
##
xi_hat <- fit$draws("mu_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(draw = 1:n()) %>%
  pivot_longer(c(-draw),
               names_to = "par",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(par,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(par,"(\\d+).")[,2])]) %>%
  select(-par) %>%
  filter(!is.na(s)) %>%
  pivot_wider(id_cols = c(draw, t),
              names_from = s,
              values_from = val)
cor_df <- data.frame()
for (i in 1:1200){
  print(i)
  tmp <- xi_hat %>%
    filter(draw == i) %>%
    select(-draw, -t) %>%
    as.matrix() %>%
    cor() %>%
    as_tibble() %>%
    rownames_to_column(var = "s1") %>%
    mutate(s1 = state_abb[as.integer(s1)]) %>%
    pivot_longer(c(-s1),
                 names_to = "s2",
                 values_to = "corr")
  cor_df <- bind_rows(cor_df, tmp)
}
cor_df_sum <- cor_df %>%
  group_by(s1, s2) %>%
  summarize(
    q50 = quantile(corr, 0.5),
    q25 = quantile(corr, 0.25),
    q75 = quantile(corr, 0.75),
    q10 = quantile(corr, 0.10),
    q90 = quantile(corr, 0.90),
  )


## median
cor_df_wide_q50 <- cor_df_sum %>%
  select(s1, s2, q50) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q50) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q50[states_2020_ordered[-51],
                                       states_2020_ordered[-51]])
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_q50.jpeg",
       plt,
       width = 10, height = 6)
## 75%
cor_df_wide_q75 <- cor_df_sum %>%
  select(s1, s2, q75) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q75) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q75[states_2020_ordered[-51],
                                       states_2020_ordered[-51]])
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_q75.jpeg",
       plt,
       width = 10, height = 6)
## 75%
cor_df_wide_q25 <- cor_df_sum %>%
  select(s1, s2, q25) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q25) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q25[states_2020_ordered[-51],
                                       states_2020_ordered[-51]])
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_q25.jpeg",
       plt,
       width = 10, height = 6)

## Histograms
plt <- ggplot(cor_df %>%
         filter(s1 != s2, (s1 %in% states_2020_ordered[25:35]),
                (s2 %in% states_2020_ordered[25:35])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_25_35.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[1:10]),
                       (s2 %in% states_2020_ordered[41:50])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_1_10_41_50.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[1:10]),
                       (s2 %in% states_2020_ordered[1:10])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_1_10.jpeg",
       plt,
       width = 10, height = 6)


plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[11:20]),
                       (s2 %in% states_2020_ordered[31:40])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_11_20_31_40.jpeg",
       plt,
       width = 10, height = 6)


plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[41:50]),
                       (s2 %in% states_2020_ordered[41:50])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_41_50.jpeg",
       plt,
       width = 10, height = 6)


results %>% filter(year == 2020) %>%
  mutate(dem = dem/(dem + rep),
         q25 = quantile(dem, 0.25),
         q50 = quantile(dem, 0.5),
         q75 = quantile(dem, 0.75)
  ) %>%
  filter(round(dem, 2) == round(q75, 2) )

plt <- ggplot(cor_df %>%
                mutate(s1q = ifelse((s1 %in% states_2020_ordered[1:13]), "1st",
                             ifelse((s1 %in% states_2020_ordered[14:25]), "2nd",
                             ifelse((s1 %in% states_2020_ordered[26:38]), "3rd", "4th"))),
                       s2q = ifelse((s2 %in% states_2020_ordered[1:13]), "1st",
                             ifelse((s2 %in% states_2020_ordered[14:25]), "2nd",
                             ifelse((s2 %in% states_2020_ordered[26:38]), "3rd", "4th"))),
                       self = ifelse(s1q == s2q, "yes", "no")) %>%
                filter(s1 != s2), aes()) +
  geom_histogram(aes(x = corr, y = ..density.., fill = self), bins = 200, position = "identity", alpha = 0.3) +
  facet_grid(s1q~.) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/xi_diff_polling_error_cov_mat_hist_quantiles.jpeg",
       plt,
       width = 10, height = 6)




###############################################################################
## Difference
diff_hat <- fit$draws("diff_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(draw = 1:n()) %>%
  pivot_longer(c(-draw),
               names_to = "par",
               values_to = "val") %>%
  mutate(t = as.integer(str_match(par,",(\\d+)")[,2]),
         s = state_abb[as.integer(str_match(par,"(\\d+).")[,2])]) %>%
  select(-par) %>%
  filter(!is.na(s)) %>%
  pivot_wider(id_cols = c(draw, t),
              names_from = s,
              values_from = val)
cor_df <- data.frame()
for (i in 1:1200){
  print(i)
  tmp <- diff_hat %>%
    filter(draw == i) %>%
    select(-draw, -t) %>%
    as.matrix() %>%
    cor() %>%
    as_tibble() %>%
    rownames_to_column(var = "s1") %>%
    mutate(s1 = state_abb[as.integer(s1)]) %>%
    pivot_longer(c(-s1),
                 names_to = "s2",
                 values_to = "corr")
  cor_df <- bind_rows(cor_df, tmp)
}
cor_df_sum <- cor_df %>%
  group_by(s1, s2) %>%
  summarize(
    q50 = quantile(corr, 0.5),
    q25 = quantile(corr, 0.25),
    q75 = quantile(corr, 0.75),
    q10 = quantile(corr, 0.10),
    q90 = quantile(corr, 0.90),
  )


## median
cor_df_wide_q50 <- cor_df_sum %>%
  select(s1, s2, q50) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q50) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q50[states_2020_ordered[-51],
                                              states_2020_ordered[-51]],
                              title = "Difference")
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_q50.jpeg",
       plt,
       width = 10, height = 6)
## 75%
cor_df_wide_q75 <- cor_df_sum %>%
  select(s1, s2, q75) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q75) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q75[states_2020_ordered[-51],
                                              states_2020_ordered[-51]],
                              title = "Difference")
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_q75.jpeg",
       plt,
       width = 10, height = 6)
## 75%
cor_df_wide_q25 <- cor_df_sum %>%
  select(s1, s2, q25) %>%
  pivot_wider(id_cols = s1,
              names_from = s2,
              values_from = q25) %>%
  column_to_rownames(var = "s1")
plt <- ggcorrplot::ggcorrplot(cor_df_wide_q25[states_2020_ordered[-51],
                                              states_2020_ordered[-51]],
                              title = "Difference")
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_q25.jpeg",
       plt,
       width = 10, height = 6)

## Histograms
plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[25:35]),
                       (s2 %in% states_2020_ordered[25:35])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho",
       title = "Difference") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_25_35.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[1:10]),
                       (s2 %in% states_2020_ordered[41:50])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho",
       title = "Difference") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_1_10_41_50.jpeg",
       plt,
       width = 10, height = 6)

plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[1:10]),
                       (s2 %in% states_2020_ordered[1:10])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho",
       title = "Difference") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_1_10.jpeg",
       plt,
       width = 10, height = 6)


plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[11:20]),
                       (s2 %in% states_2020_ordered[31:40])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho",
       title = "Difference") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_11_20_31_40.jpeg",
       plt,
       width = 10, height = 6)


plt <- ggplot(cor_df %>%
                filter(s1 != s2, (s1 %in% states_2020_ordered[41:50]),
                       (s2 %in% states_2020_ordered[41:50])), aes(x = corr)) +
  geom_histogram(bins = 60) +
  facet_grid(s1~s2) +
  theme_light() +
  labs(x = "rho",
       title = "Difference") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_41_50.jpeg",
       plt,
       width = 10, height = 6)


results %>% filter(year == 2020) %>%
  mutate(dem = dem/(dem + rep),
         q25 = quantile(dem, 0.25),
         q50 = quantile(dem, 0.5),
         q75 = quantile(dem, 0.75)
  ) %>%
  filter(round(dem, 2) == round(q75, 2) )

plt <- ggplot(cor_df %>%
                mutate(s1q = ifelse((s1 %in% states_2020_ordered[1:13]), "1st",
                                    ifelse((s1 %in% states_2020_ordered[14:25]), "2nd",
                                           ifelse((s1 %in% states_2020_ordered[26:38]), "3rd", "4th"))),
                       s2q = ifelse((s2 %in% states_2020_ordered[1:13]), "1st",
                                    ifelse((s2 %in% states_2020_ordered[14:25]), "2nd",
                                           ifelse((s2 %in% states_2020_ordered[26:38]), "3rd", "4th"))),
                       self = ifelse(s1q == s2q, "yes", "no")) %>%
                filter(s1 != s2), aes()) +
  geom_histogram(aes(x = corr, y = ..density.., fill = self), bins = 200, position = "identity", alpha = 0.3) +
  facet_grid(s1q~.) +
  theme_light() +
  labs(x = "rho") +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/input/polling_error/polling_error_diff_cov_mat/diff_polling_error_cov_mat_hist_quantiles.jpeg",
       plt,
       width = 10, height = 6)




















