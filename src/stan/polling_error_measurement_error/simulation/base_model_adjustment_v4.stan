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
  vector[S] xi;
  real<lower = 0, upper = 1> alpha;
}
model{
  vector[N] mu;
  mu = logit((1 - alpha) * inv_logit(outcome_logit[s] + xi[s]) + alpha * 0.5);
  //mu = logit((1 - alpha) * inv_logit(outcome_logit[s]) + alpha * 0.5) + xi[s];
  xi ~ normal(0, 1);
  logit(alpha) ~ normal(0, 1);
  y ~ binomial_logit(n, mu);
  //y ~ binomial_logit(n, logit((1 - alpha) * inv_logit(outcome_logit[s]) + alpha * 0.5) +  + xi[s]);

}
generated quantities{
  vector[S] polling_error;
  polling_error = inv_logit(outcome_logit + xi) - outcome;
  //polling_error = inv_logit(logit((1 - alpha) * outcome + alpha * 0.5) + xi) -
  //(1 - alpha) * outcome + alpha * 0.5;

}
