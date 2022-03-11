data {
  int<lower=0> N;
  int<lower=0> E;
  int<lower=0> T;
  int<lower = 1, upper = E> idx_e[N];
  int<lower = 1, upper = T> idx_t[N];
  int y[N];
  int n[N];
  vector[E] mu;
}
parameters {
  vector[E] raw_epsilon;
  real<lower=0> epsilon_sigma;
  real mu_epsilon;
  vector[T] raw_gamma;
  real<lower = 0> gamma_sigma;
}
transformed parameters {
  vector[E] epsilon = mu_epsilon + raw_epsilon * epsilon_sigma;
  vector[T] gamma = raw_gamma * gamma_sigma;
}
model {
  mu_epsilon ~ normal(0, 1);
  raw_epsilon ~ std_normal();
  raw_gamma ~ std_normal();
  gamma_sigma ~ normal(0, 1);
  epsilon_sigma ~ normal(0, 0.1);
  y ~ binomial_logit(n, logit(mu[idx_e]) +
                          epsilon[idx_e] +
                          gamma[idx_t]);
}
generated quantities {
  vector[E] polling_error = inv_logit(logit(mu) + epsilon) - mu;
}
