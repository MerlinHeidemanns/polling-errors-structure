## Libraries
library(tidyverse)
library(cmdstanr)
## Load data
df <- read_csv("dta/clean_data/cces_2008_2012_2016.csv")
## Fit model
df <- df %>%
  mutate(year = as.factor(year),
         error_dem = error_dem * 100,
         prev_rep_share = 1 - prev_dem_share) %>%
  filter(state_po != "DC")
fit <- stan_glm(error_dem ~ prev_rep_share * year, data = df)
new <- data.frame(
  year = as.factor(sort(rep(seq(2008, 2016, 4), 100))),
  prev_rep_share = rep(seq(0, 1, length.out = 100), 3),
  i = as.character(1:300)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(prev_rep_share, year) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = prev_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = prev_rep_share, y = error_dem), size = 0.5, data = df) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  facet_wrap(year~.) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)",
       title = "CCES") +
  xlim(c(min(df$prev_rep_share),max(df$prev_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_general_cces.png",
       plt,
       height = 6, width = 10)
###
fit <- stan_glm(error_dem ~ 1 + prev_rep_share, data = df)
new <- data.frame(
  prev_rep_share = seq(0, 1, length.out = 300),
  i = as.character(1:300)
)
pred <- posterior_epred(fit, new)
pred <- pred %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "i",
               values_to = "draw") %>%
  left_join(new) %>%
  group_by(prev_rep_share) %>%
  summarize(
    q50 = quantile(draw, 0.5),
    q25 = quantile(draw, 0.25),
    q75 = quantile(draw, 0.75),
    q10 = quantile(draw, 0.10),
    q90 = quantile(draw, 0.90)
  )
plt <- ggplot(data = pred, aes(x = prev_rep_share, y = q50)) +
  geom_line() +
  geom_point(aes(x = prev_rep_share, y = error_dem), size = 0.5, data = df) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.5) +
  theme_light() +
  labs(caption = "Positive polling errors underestimate Republican support
               Median, 50%/90% interval",
       x = "Republican vote share in previous election",
       y = "Polling error (%, favors Democrats)") +
  xlim(c(min(df$prev_rep_share),max(df$prev_rep_share)))
ggsave("plt/polling_error_voteshare/pres_polling_error_pres_voteshare_all_cces.png",
       plt,
       height = 6, width = 10)


