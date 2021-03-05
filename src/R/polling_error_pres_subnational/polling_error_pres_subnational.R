## Libraries
library(tidyverse)
## Load data
df <- read_csv("dta/clean_data/cces_2016_results_2012_urban2.csv")
## Plot
ggplot(data = df %>%
         filter(!is.na(rural2)), aes(x = 1 - dem_share,
                      y = 100 * (mean_dem_estimate - dem_share),
                      color = as.factor(rural2),
                      fill = as.factor(rural2))) +
  geom_text(aes(label = State)) +
  geom_smooth(method = "lm") +
  theme_light() +
  labs(x = "Republican voteshare in area in previous election",
       y = "Polling error (%, favors Democrats)",
       color = "Rural",
       fill = "Rural")