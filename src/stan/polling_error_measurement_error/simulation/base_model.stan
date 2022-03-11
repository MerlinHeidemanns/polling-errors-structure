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
}
model{
  vector[N] mu;
  mu = outcome_logit[s] + xi[s];
  xi ~ normal(0, 1);
  y ~ binomial_logit(n, mu);
}
generated quantities{
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
}
