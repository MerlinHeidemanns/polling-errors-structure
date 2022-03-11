data {
  int N;
  int S;
  int s[N];
  int y[N];
  int n[N];
  vector[S] outcome;
}
transformed data{
  vector[S] outcome_logit = logit(outcome);
}
parameters {
  vector[S] xi;
  real<lower = 0, upper = 1> alpha;
}
model{
  vector[N] mu;
  mu = inv_logit(outcome_logit[s] + xi[s]);
  xi ~ normal(0, 1);
  logit(alpha) ~ normal(0, 1);
  y ~ binomial(n, (1 - alpha) * mu + alpha * 0.5);
  //y ~ binomial_logit(n, logit((1 - alpha) * inv_logit(outcome_logit[s]) + alpha * 0.5) +  + xi[s]);

}
generated quantities{
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
}
