################################################################################
## Implementing the model in Hausmann 2001
## q_i= a_o + (1 - a_0 - a_1)F(x_i * beta)) + e_i
################################################################################
## Load libraries
library(tidyverse)
library(boot)
################################################################################
## Simulate data

sim <- function(N = 1000, a_0 = 0.06, a_1 = 0.03, mu = 0.3){
  N <- 1000
  a_0 <- 0.06
  a_1 <- 0.03
  y_true <- rbinom(N, 1, mu)
  y_observed <- rep(NA, N)
  y_observed[y_true == 1] <- sample(c(0, 1), sum(y_true == 1), replace = TRUE, c(a_0, 1 - a_0))
  y_observed[y_true == 0] <- sample(c(0, 1), sum(y_true == 0), replace = TRUE, c(1 - a_1, a_1))
  return(mean(y_true) - mean(y_observed))
}