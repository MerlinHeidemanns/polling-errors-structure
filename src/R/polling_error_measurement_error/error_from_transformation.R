


alpha <- 0.3
alpha * 0.8 + (1 - alpha) * 0.5


curve(inv.logit(x * logit(0.8) + (1 - x) * logit(0.5)))
abline(a = 0.5, b = .3)


diff <- (inv.logit(seq(0, 1, length.out = 100) * logit(0.3) + (1 - seq(0, 1, length.out = 100)) * logit(0.5))
) - (seq(0, 1, length.out = 100) * 0.3 + (1 - seq(0, 1, length.out = 100)) * 0.5)


diff <- (inv.logit(seq(0, 1, length.out = 100) * logit(0.4) + (1 - seq(0, 1, length.out = 100)) * logit(0.5))
) - (seq(0, 1, length.out = 100) * 0.4 + (1 - seq(0, 1, length.out = 100)) * 0.5)

out <- data.frame()
alpha_max <- 0.2
for (j in seq(0.5, 1, length.out = 100)){
  diff <- (inv.logit(seq(0, alpha_max, length.out = 100) * logit(j) + (1 - seq(0, alpha_max, length.out = 100)) * logit(0.5))
  ) - (seq(0, alpha_max, length.out = 100) * j + (1 - seq(0, alpha_max, length.out = 100)) * 0.5)
  out <- bind_rows(out,
                   data.frame(
                     pr = j,
                     max_error = max(abs(diff))
                   ))
}

plot(out$pr, out$max_error, type = "l")



