################################################################################
## Can misspecifying weights be the answer?
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Simulate data
N <- 10000
prob = c(0.3, 0.7)
z <- sample(c(1, 2), N, replace = TRUE, prob = prob)
pi <- c(0.25, 0.75)
selection_probability <- c(0.25, 0.5)
y <- rbinom(N, 1, pi[z])

true_mu <- mean(y)
true_mu <- sum(pi * prob)

replicate(100, mean(y[sample(z,1000, replace = FALSE, prob = selection_probability[z])]))


y_hat <- replicate(1000,
          mean(
            rbinom(1000, 1, pi[
              sample(c(1, 2),1000, replace = TRUE, prob = selection_probability * prob/sum(selection_probability))
              ]
              )
          )
)
hist(y_hat)
