##
library(DirichletReg)
##

G <- 4
pop <- data.frame(
  r = sort(rep(seq(1, 2), 2)),
  d = rep(seq(1, 2), 2),
  shares = t(rdirichlet(1, rep(1, G) * 10)),
  supp = rbeta(G, 5, 5)
)
true <- sum(pop$shares * pop$supp)
N <- 1000
s <- sample(seq(1, 4), N, pop$shares, replace = TRUE)
y <- rbinom(N, 1, prob = pop$supp[s])

