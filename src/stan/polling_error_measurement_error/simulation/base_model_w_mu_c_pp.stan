data {
  int N;
  int S;
  int P;
  int s[N];
  int p[N];
  int y[N];
  int n[N];
  vector[S] outcome;
  real<lower = 0> prior_xi_sigma;
}
transformed data{
  vector[S] outcome_logit = logit(outcome);
  //real alpha = 0.2;
}
parameters {
  real<lower = 0, upper = 1> alpha;
  vector[S] xi;
  vector[P] mu_c;
  real<lower = 0.0001> mu_c_sigma;
  real<lower = 0.0001> xi_sigma;
}
model{
  vector[N] mu;
  xi_sigma ~ normal(0, prior_xi_sigma);
  mu_c_sigma ~ normal(0, mu_c_sigma);
  xi ~ normal(0, xi_sigma);
  mu_c ~ normal(0, mu_c_sigma);
  mu = logit((1 - alpha) * inv_logit(outcome_logit[s] + xi[s] + mu_c[p]) + alpha * 0.5);
  //mu = logit((1 - alpha) * inv_logit(outcome_logit[s]) + alpha * 0.5) + xi[s];
  //mu = logit((1 - alpha) * inv_logit(outcome_logit[s] + xi[s]) + alpha * inv_logit(xi[s]));
  logit(alpha) ~ normal(0, 1);
  y ~ binomial_logit(n, mu);
}
generated quantities {
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
}

