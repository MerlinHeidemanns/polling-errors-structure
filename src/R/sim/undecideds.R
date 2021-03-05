


with_undecided <- function(y){
  # x = sample average
  # y = share undecided
  lapply(seq(0, 1, 0.5), function(alpha){
    lapply(seq(0, 1, 0.01), function(x){
      data.frame(
        sample_average = x,
        alpha = alpha,
        undecided = (alpha * x + (1 - alpha) * 0.5),
        true = (1 - y) * x + y * (alpha * x + (1 - alpha) * 0.5 )
      ) %>%
        return()
    }) %>%
      do.call("bind_rows", .) %>%
      return()
  }) %>%
    do.call("bind_rows", .) %>%
    return()
}
df <- with_undecided(0.1)

ggplot(df, aes(x = true, y = 100 * (sample_average - true), color = as.factor(alpha))) +
  geom_line() +
  labs(x = "'Republican vote share'",
       y = "Polling error (%, favoring 'Democrats'",
       color = "Undecided non-partisanship",
       caption = "1 = Undecideds are 50/50
                  0 = Undecideds are state average split") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.box = "horizontal")