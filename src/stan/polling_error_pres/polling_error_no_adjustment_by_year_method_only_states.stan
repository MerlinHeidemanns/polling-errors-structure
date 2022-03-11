data {
  int N_state;
  int S; // State
  int T; // Year
  int M;
  int t_state[N_state];
  int s[N_state];
  int m[N_state];
  int x[N_state];
  int mx[N_state];
  int mt[N_state];
  int y_state[N_state];
  int n_state[N_state];
  vector[S * T] outcome_state;

}
transformed data {
  vector[N_state] logit_outcome_state;
  logit_outcome_state = logit(outcome_state[x]);
}
parameters {
  real<lower = 0.00001> xi_sigma;
  vector[S * T * M] raw_xi;
  real<lower = 0.00001> mu_sigma;
  vector[T * M]     raw_mu;
}
transformed parameters {
  vector[S * T * M] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[T * M] mu;
  xi = xi_sigma * raw_xi;
  mu = mu_sigma * raw_mu;
  for (tt in 1:T){
    for (mm in 1:M){
      xi[8 + (tt - 1) * 51 + (mm - 1) * S * T] = 0.0;
    }
  }
}
model {
  xi_sigma ~ normal(0, 0.2);
  to_vector(raw_xi) ~ std_normal();
  mu_sigma ~ normal(0, 1);
  to_vector(raw_mu) ~ std_normal();
  y_state ~ binomial_logit(n_state,
    logit_outcome_state + xi[mx] + mu[mt]);

}
generated quantities {
  matrix[S, T] epsilon[M];
  matrix[S, T] y_hat[M];
  for (mm in 1:M){
    for (tt in 1:T){
      y_hat[mm,:, tt] = inv_logit(logit(outcome_state[((tt - 1) * S + 1):(tt * S)]) +
        to_vector(xi[((tt - 1) * S + 1) + (mm - 1) * S * T:(tt * S) + (mm - 1) * S * T]));
      epsilon[mm,:, tt] = 100 * (outcome_state[((tt - 1) * S + 1):(tt * S)] - y_hat[mm,:, tt]);
    }
  }
}
