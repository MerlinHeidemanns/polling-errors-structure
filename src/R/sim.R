## Libraries
library(tidyverse)
##

mat <- boot::inv.logit(MASS::mvrnorm(1000, c(0, 0),
              matrix(c(1,0.8,0.8, 1),
                     byrow = TRUE, ncol = 2)))
df <- data.frame(mat) %>%
  rename(y_star = X1,
         response_rate = X2) %>%
  mutate(y = rbinom(n(), 1, y_star),
         response = rbinom(n(), 1, response_rate)) %>%
  group_by(response) %>%
  summarize(mu = mean(y))

simulate_survey_outcome <- function(N, rho, n){
  df <- lapply(1:N, function(x){
    mat <- boot::inv.logit(MASS::mvrnorm(n, c(0, 0),
                                         matrix(c(1,rho,rho, 1),
                                                byrow = TRUE, ncol = 2)))
    df <- data.frame(mat) %>%
      rename(y_star = X1,
             response_rate = X2) %>%
      mutate(y = rbinom(n(), 1, y_star),
             response = rbinom(n(), 1, response_rate))
    pop_mu <- df %>% pull(y) %>% mean()
    df <- df %>%
      group_by(response) %>%
      summarize(mu = mean(y)) %>%
      mutate(pop_mu = pop_mu,
             rho = rho)
    return(df)
  }) %>% do.call("bind_rows", .)
  return(df)
}
df <- lapply(seq(-1, 1, 0.1), function(rho){
  return(simulate_survey_outcome(100, rho, 10000))
}) %>% do.call("bind_rows",.)

df_sum <- df %>%
  group_by(rho, response) %>%
  mutate(diff = mu - pop_mu) %>%
  summarize(
    q50 = quantile(diff, 0.5),
    q25 = quantile(diff, 0.25),
    q75 = quantile(diff, 0.75),
    q10 = quantile(diff, 0.1),
    q90 = quantile(diff, 0.9)
  )
ggplot(data = df_sum, aes(x = rho, y = q50,
                          fill = as.factor(response),
                          color = as.factor(response))) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3) +
  theme_light()


sim_response_share <- function(mu){
  data.frame(y = rbinom(10000, 1, mu)) %>%
    mutate(response = rbinom(n(), 1, ifelse(y == 1, 1 - mean(y), mean(y)))) %>%
    filter(response == 1) %>%
    pull(y) %>%
    mean() %>%
    return()
}
df <- lapply(seq(0.05, 0.95, 0.05), function(x){
  data.frame(mu_hat = replicate(50, sim_response_share(x)),
             mu = x) %>%
    return()
}) %>%
  do.call("bind_rows", .)


df_sum <- df %>%
  group_by(mu) %>%
  mutate(diff = mu - mu_hat) %>%
  summarize(
    q50 = quantile(diff, 0.5),
    q25 = quantile(diff, 0.25),
    q75 = quantile(diff, 0.75),
    q10 = quantile(diff, 0.1),
    q90 = quantile(diff, 0.9)
  )
ggplot(data = df_sum, aes(x = mu, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3) +
  theme_light() +
  labs(x = "Share Republican",
       y = "Polling error (favoring Democrats")



























