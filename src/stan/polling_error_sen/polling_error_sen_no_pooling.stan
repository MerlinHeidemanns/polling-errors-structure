data {
  int N;
  int R;
  int r[N];
  int y[N];
  int n[N];
  vector[N] outcome;
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  real alpha;
  vector[R] beta;
}
model {
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  y ~ binomial_logit(n, logit_outcome + alpha + beta[r]);
}
generated quantities {
  vector[R] pred;
  pred = alpha + beta;
}
