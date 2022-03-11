################################################################################
## Simulate data according to the process plus alternatives
################################################################################
## Load libraries
library(tidyverse)
library(boot)
################################################################################
## Simulate
sim <- function(NSurveys, NLocations, NSample){
  id_sample <- sample(1:NLocations, NSurveys, replace = TRUE)
  true_prob <- inv.logit(rnorm(NLocations, 0, 0.6))
  sample_prob <- rbinom(NSurveys, NSample, true_prob[id_sample])/NSample
  df <- data.frame(
    sample_prob = sample_prob,
    true_prob   = true_prob[id_sample]
  )

  ggplot(data = df, aes(x = true_prob, y = true_prob - sample_prob)) +
    geom_point() +
    geom_smooth(method = "lm")
}
sim(200, 100, 10000)

## Simulate with non-sampling error
sim <- function(NSurveys, NLocations, NSample, sd_ns){
  id_sample <- sample(1:NLocations, NSurveys, replace = TRUE)
  true_prob <- inv.logit(rnorm(NLocations, 0, 0.6))
  sample_prob <- rbinom(NSurveys,
                        NSample,
                        inv.logit(logit(true_prob[id_sample]) +
                                          rnorm(NSurveys, 0, sd_ns)))/NSample
  df <- data.frame(
    sample_prob = sample_prob,
    true_prob   = true_prob[id_sample]
  )

  ggplot(data = df, aes(x = true_prob, y = true_prob - sample_prob)) +
    geom_point() +
    geom_smooth(method = "lm")
}
sim(200, 100, 10000, 0.2)

## Polling house error
sim <- function(NSurveys, NPollsters, NLocations, NSample, sd_error){
  id_sample <- sample(1:NLocations, NSurveys, replace = TRUE)
  true_prob <- inv.logit(rnorm(NLocations, 0, 0.6))
  sample_prob <- rbinom(NSurveys,
                        NSample,
                        inv.logit(logit(true_prob[id_sample]) +
                                    rnorm(NSurveys, 0, sd_ns)))/NSample
  df <- data.frame(
    sample_prob = sample_prob,
    true_prob   = true_prob[id_sample]
  )

  ggplot(data = df, aes(x = true_prob, y = true_prob - sample_prob)) +
    geom_point() +
    geom_smooth(method = "lm")
}
sim(200, 100, 10000, 0.2)




################################################################################
## Does it have to do with strenght of vote intentions?
#' Consider that the intention distribution is bimodal
#' Move mean around
#' Consider that there are cutoffs that determine whether voters indicate
#' a strong vote intention, lean or none
#' S shaped pattern
#' An increase in polarization (standard deviation decreases), increases the
#' pattern.
NSample <- 10000
df_out <- lapply(seq(0.2, 0.8, length.out = 100), function(j){
  mu <- logit(j)
  x <- ifelse(rbinom(NSample,1, 0.5), rnorm(NSample, -3, 1), rnorm(NSample, 3, 1))
  y_star <- mu + x
  y_star_cut <- as.integer(cut(y_star, breaks = c(-Inf,-2,-1, 1,2,Inf)))
  y <- rbinom(NSample, 1, inv.logit(y_star))
  lean <- case_when(
    y_star_cut %in% c(1,2) ~ 0,
    y_star_cut %in% c(4,5) ~ 1
  )
  convinced = case_when(
    y_star_cut == 1 ~ 0,
    y_star_cut == 5 ~ 1
  )
  tmp <- data.frame(
    mean_true = mean(y),
    mean_lean = mean(lean, na.rm = TRUE),
    mean_convinced = mean(convinced, na.rm = TRUE)
  )
  return(tmp)
}) %>%
  do.call("bind_rows", .)
df_out <- df_out %>%
  pivot_longer(c(mean_lean, mean_convinced),
               names_to = c("kind"),
               values_to = c("mean")) %>%
  mutate(polling_error = mean_true - mean)

ggplot(data = df_out, aes(x = mean_true, y = polling_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(kind ~.) +
  geom_hline(aes(yintercept = 0))

################################################################################
#' Variation of previous one but now vary not local mu but composition while
#' keeping bimodality
NSample <- 10000
df_out <- data.frame()
for (j in seq(0.1, 0.9, length.out = 100)){
  y_star <- ifelse(!rbinom(NSample,1, j), rnorm(NSample, -2, 3), rnorm(NSample, 2, 3))
  y_star_cut <- as.integer(cut(y_star, breaks = c(-Inf,-1.5,-0.5, 0.5,1.5,Inf)))
  y <- rbinom(NSample, 1, inv.logit(y_star))
  lean <- case_when(
    y_star_cut %in% c(1,2) ~ 0,
    y_star_cut %in% c(4,5) ~ 1
  )
  convinced = case_when(
    y_star_cut == 1 ~ 0,
    y_star_cut == 5 ~ 1
  )
  tmp <- data.frame(
    composition = j,
    mean_true = mean(y),
    mean_lean = mean(lean, na.rm = TRUE),
    mean_convinced = mean(convinced, na.rm = TRUE)
  )
  df_out <- bind_rows(df_out, tmp)
}
df_out <- df_out %>%
  pivot_longer(c(mean_lean, mean_convinced),
               names_to = c("kind"),
               values_to = c("mean")) %>%
  mutate(polling_error = mean_true - mean)

ggplot(data = df_out, aes(x = mean_true, y = polling_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(kind ~.) +
  geom_abline(aes(intercept = 0, slope = 1))


###############################################################################
N <- 1e5
G <- 100
g <- sample(1:G, N, replace = TRUE)
mu_g <- seq(0.1, 0.9, length.out = G)

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









