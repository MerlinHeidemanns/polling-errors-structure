data {
  int N;
  int S; // State
  int T; // Year
  int T_days;
  int t[N]; // pre election
  int x[N];
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
  vector<lower = 0>[S * T] xi_sigma;
  matrix[S * T, T_days] xi;
}
model {
  alpha ~ normal(0, 1);
  xi_sigma ~ normal(0, 0.1);
  xi[,1] ~ normal(0, xi_sigma);
  for (tt in 2:T_days) xi[,tt] ~ normal(xi[,tt - 1], xi_sigma);

  y ~ binomial_logit(n, logit_outcome +
    alpha +
    xi[x]);
}
generated quantities {
  vector[S * T] mu;
  matrix[S, T] mu_matrix;
  mu = alpha + xi;
  for (tt in 1:T){
    mu_matrix[:, tt] = mu[((tt - 1) * 50 + 1):(tt * 50)];
  }
}






