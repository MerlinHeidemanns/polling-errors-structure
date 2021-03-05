## Libraries
library(cmdstanr)
library(tidyverse)
## Load data
df <- read_csv("dta/clean_data/polls_pres_prepared.csv")
indexes <- read_csv("dta/clean_data/index_pres_prepared.csv")
## Model
m <- file.path("src/stan/polling_error_pres", "polling_error_simple.stan")
mod <- cmdstan_model(m)
## Data list
data_list <- list(
  N = nrow(df),
  T = df %>% pull(t) %>% max(),
  S = df %>% pull(s) %>% max(),
  x = df %>% pull(x),
  y = df %>% pull(y),
  n = df %>% pull(n),
  outcome = df %>% pull(outcome)
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
fit$save_object(file = "dta/model_output/polling_error_pres_simple.RDS")


