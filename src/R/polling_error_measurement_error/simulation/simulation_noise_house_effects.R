library(boot)
library(tidyverse)
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
simulate_polls <- function(N, respondents, true_data, alpha, P){
  S <- nrow(true_data)
  xi <- runif(S, min = -0.1, max = 0.1)
  mu_c <- runif(P, min = -0.1, max = 0.1)
  s_poll <- sample(1:S, N, replace = TRUE)
  p_poll <- sample(1:P, N, replace = TRUE)
  theta <- rep(NA, N)
  theta <- inv.logit(logit(true_data$mu_b[s_poll]) + xi[s_poll] + mu_c[p_poll])
  n1 <- as.integer(rep(respondents, N) * (1 - alpha))
  n2 <- respondents - n1
  y1 <- rbinom(N, n1, theta); y2 <- rbinom(N, n2, 0.5)
  df <- data.frame(
    s = s_poll,
    p = p_poll,
    n = n1 + n2,
    y = y1 + y2
  )
  return(list(df = df,
              xi = xi,
              mu_c = mu_c,
              inv_logit_xi = inv.logit(logit(true_data$mu_b) + xi) - true_data$mu_b))
}
## data list
prepare_data_list <- function(df, true_data){
  data_list <- list(
    N = nrow(df),
    S = nrow(true_data),
    P = df$p %>% max(),
    p = df$p,
    s = df$s,
    y = df$y,
    n = df$n,
    outcome = true_data$mu_b
  )
  return(data_list)
}


## True data
S <- 200
true_data <- simulate_true_date(S, 0.10)
## polls
polls <- simulate_polls(1000, 500000, true_data, 0.2, 10)
## datalist
data_list <- prepare_data_list(polls$df, true_data)
## model
mod <- cmdstan_model("code/stan/models/models_polls/components/polling_error_measurement_error/simulation/base_model_mu_c.stan")
mod <- cmdstan_model("code/stan/models/models_polls/components/polling_error_measurement_error/simulation/base_model_adjustment_mu_c.stan")
## fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  iter_sampling = 1000,
  iter_warmup = 1000,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)

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






