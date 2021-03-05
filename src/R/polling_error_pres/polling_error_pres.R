## Libraries
library(cmdstanr)
library(tidyverse)
## Load data
df <- read_csv("dta/clean_data/polls_pres_prepared.csv")
indexes <- read_csv("dta/clean_data/index_pres_prepared.csv")
## Model
m <- file.path("src/stan/polling_error_additive", "polling_error_v5_additive.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df),
  R = df %>% pull(r) %>% max(),
  D = df %>% pull(d) %>% max(),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  s = df %>% pull(s),
  rt = df %>% pull(rt),
  dt = df %>% pull(dt),
  t = df %>% pull(t),
  x = df %>% pull(x),
  r = df %>% pull(r),
  d = df %>% pull(d),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome),
  corr_s = indexes %>% pull(s),
  corr_r = indexes %>% pull(r),
  corr_d = indexes %>% pull(d),
  corr_x = indexes %>% pull(x),
  corr_dt = indexes %>% pull(dt),
  corr_rt = indexes %>% pull(rt),
  corr_t = indexes %>% pull(t)
)
## Fit
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  init = 0.2
)
## Save
fit$save_object(file = "dta/model_output/polling_error_pres.RDS")


