data {
  int N;
  int E;
  int T;
  int<lower = 1, upper = E> idx_e[N];
  int<lower = 1, upper = T> idx_t[N];
  int<lower = 1, upper = T> idx_te[E];
  int y[N];
  int n[N];
  vector[E] outcome;
}
transformed data {
  vector[E] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  real<lower = 0> xi_sigma;
  vector[E] xi;
  real<lower = 0> gamma_sigma;
  vector[T] gamma;
}
model {
  xi_sigma ~ normal(0, 0.3);
  xi ~ normal(0, xi_sigma);
  gamma ~ normal(0, gamma_sigma);
  gamma_sigma ~ normal(0, 1);
  y ~ binomial_logit(n, logit_outcome[idx_e] + xi[idx_e] + gamma[idx_t]);
}
generated quantities {
  vector[E] epsilon;
  epsilon = inv_logit(logit_outcome) - inv_logit(logit_outcome + xi);
}
