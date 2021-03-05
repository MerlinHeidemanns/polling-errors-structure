data {
  int N; // N polls
  int S; // State
  int T; // Year
  int PT; // pollster x time index
  int p[N];
  int y[N];
  int n[N];
  vector[N] outcome;
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  vector[PT] beta;
}
model {
  beta ~ normal(0, 1);
  y ~ binomial_logit(n, logit_outcome + beta[p]);
}








