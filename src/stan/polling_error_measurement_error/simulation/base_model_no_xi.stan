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
  //real alpha = 0.2;
}
parameters {
  real<lower = 0, upper = 1> alpha;
}
model{
  vector[N] mu;
  mu = logit((1 - alpha) * outcome[s] + alpha * 0.5);
  logit(alpha) ~ normal(0, 1);
  y ~ binomial_logit(n, mu);
}

