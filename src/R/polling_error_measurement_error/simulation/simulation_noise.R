library(boot)
library(tidyverse)
library(cmdstanr)
## true data
simulate_true_data <- function(S, mu_b_sigma){
  mu_b <- rbeta(S, 10, 10)
  data.frame(
    s = 1:S,
    mu_b = mu_b
  ) %>%
    return()
}
## polls
simulate_polls <- function(N, respondents, true_data, alpha){
  S <- nrow(true_data)
  xi <- runif(S, min = -0.1, max = 0.1)
  s_poll <- sample(1:S, N, replace = TRUE)
  theta <- rep(NA, N)
  theta <- (1 - alpha) * inv.logit(logit(true_data$mu_b[s_poll]) + xi[s_poll]) + alpha * 0.5
  n <- rep(respondents, N)
  y <- rbinom(N, n, theta)
  df <- data.frame(
    s = s_poll,
    n = n,
    y = y
  )
  return(list(df = df,
              xi = xi,
              inv_logit_xi = inv.logit(logit(true_data$mu_b) + xi) - true_data$mu_b,
         inv_logit_xi_alpha = (1 - alpha) * inv.logit(logit(true_data$mu_b) + xi) -
           (1 - alpha) * true_data$mu_b))
}
## data list
prepare_data_list <- function(df, true_data){
  data_list <- list(
    N = nrow(df),
    S = nrow(true_data),
    s = df$s,
    y = df$y,
    n = df$n,
    outcome = true_data$mu_b
  )
  return(data_list)
}

## True data
S <- 50
true_data <- simulate_true_data(S, 0.10)
## polls
polls <- simulate_polls(1000, 1000, true_data, 0.2)
## datalist
data_list <- prepare_data_list(polls$df, true_data)
data_list[["prior_xi_sigma"]] <- 1
## model
mod <- cmdstan_model(
  "code/stan/models/models_polls/components/polling_error_measurement_error/simulation/base_model_w_xi_pp.stan")
## fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  iter_sampling = 2000,
  iter_warmup = 2000,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)
fit$summary("alpha")

xi <- fit$summary("polling_error",
                   q = ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(variable = as.integer(str_match(variable, "(\\d+)")[,2])) %>%
  pivot_longer(c(-variable),
               names_to = "q",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  pivot_wider(id_cols = variable,
              names_from = q,
              values_from = val,
              names_prefix = "q") %>%
  rename(s = variable) %>%
  mutate(kind = "estimated") %>%
  bind_rows(data.frame(
    q50 = polls$inv_logit_xi,
    s = 1:S,
    kind = "true"
  )) %>%
  left_join(true_data)

ggplot(xi, aes(x = mu_b, y = q50, color = kind)) +
  geom_point() +
  geom_smooth(method = "lm")


fit$draws(c("alpha", "xi")) %>%
  posterior::as_draws_df() %>%
  pivot_longer(c(-alpha),
               names_to = "s",
               values_to = "xi",
               names_pattern = "(\\d+)") %>%
  filter(!is.na(s)) %>%
  ggplot(aes(x = alpha, y = xi)) +
    geom_point() +
    facet_wrap(s ~ .)



xi <- fit$summary("xi",
                  q = ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(variable = as.integer(str_match(variable, "(\\d+)")[,2])) %>%
  pivot_longer(c(-variable),
               names_to = "q",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  pivot_wider(id_cols = variable,
              names_from = q,
              values_from = val,
              names_prefix = "q") %>%
  rename(s = variable) %>%
  mutate(kind = "estimated") %>%
  bind_rows(data.frame(
    q50 = polls$xi,
    s = 1:S,
    kind = "true"
  )) %>%
  left_join(true_data)

ggplot(xi, aes(x = mu_b, y = q50, color = kind)) +
  geom_point() +
  geom_smooth(method = "lm")


