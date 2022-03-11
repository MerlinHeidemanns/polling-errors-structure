data {
  int N;
  int S;
  int s[N];
  int y[N];
  int n[N];
  vector[S] outcome;
  real<lower = 0> xi_sigma;
}
transformed data{
  vector[S] outcome_logit = logit(outcome);
  //real alpha = 0.2;
}
parameters {
  real<lower = 0, upper = 1> alpha;
  vector[S] xi;
}
model{
  vector[N] mu;
  xi ~ normal(0, xi_sigma);
  mu = logit((1 - alpha) * inv_logit(outcome_logit[s] + xi[s]) + alpha * 0.5);
  //mu = logit((1 - alpha) * inv_logit(outcome_logit[s]) + alpha * 0.5) + xi[s];
  //mu = logit((1 - alpha) * inv_logit(outcome_logit[s] + xi[s]) + alpha * inv_logit(xi[s]));
  logit(alpha) ~ normal(0, 1);
  y ~ binomial_logit(n, mu);
}
generated quantities {
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
}

