data {
  int N;
  int S;
  int P;
  int s[N];
  int p[N];
  int y[N];
  int n[N];
  vector[S] outcome;
}
transformed data{
  vector[S] outcome_logit = logit(outcome);
  real alpha = 0.2;
}
parameters {
  vector[S] xi;
  vector[P] mu_c;
  //real<lower = 0, upper = 1> alpha;
}
model{
  vector[N] mu;
  mu = inv_logit(outcome_logit[s] + xi[s] + mu_c[p]);
  xi ~ normal(0, 1);
  mu_c ~ normal(0, 1);
  //alpha ~ beta(2, 2);
  //logit(alpha) ~ normal(0, 1);
  y ~ binomial(n, (1 - alpha) * mu + alpha * 0.5);
}
generated quantities{
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
}
