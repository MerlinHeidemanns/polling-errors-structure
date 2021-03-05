## Libraries
library(readr)
## Load data
df <- read_csv("dta/polls_pres_dataset_00_20.csv")
## Assess
table(df$pollName)
# Might involve some work to clean the pollster names.