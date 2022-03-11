data {
  int N_state;
  int N_national;
  int S; // State
  int T; // Year
  int t_state[N_state];
  int t_national[N_national];
  int s[N_state];
  int x[N_state];
  int y_state[N_state];
  int n_state[N_state];
  int n_national[N_national];
  int y_national[N_national];
  vector[S * T] outcome_state;
  vector[T] outcome_national;
  matrix[T, S] turnout_weights;

}
transformed data {
  vector[N_national] logit_outcome_national;
  vector[N_state] logit_outcome_state;
  logit_outcome_national = logit(outcome_national[t_national]);
  logit_outcome_state = logit(outcome_state[x]);
}
parameters {
  real<lower = 0.00001> xi_sigma;
  vector[S * T] raw_xi;
  real<lower = 0.00001> mu_sigma;
  vector[T]     raw_mu;
}
transformed parameters {
  vector[T] xi_national;
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[T] mu;
  xi = xi_sigma * raw_xi;
  mu = mu_sigma * raw_mu;
  for (tt in 1:T){
    xi[8 + (tt - 1) * 51] = 0.0;
    xi_national[tt] = logit(turnout_weights[tt] *
      inv_logit(logit(outcome_state[(1 + (tt - 1) * S):(tt * S)]) +
        xi[(1 + (tt - 1) * S):(tt * S)] +
        mu[tt]));
  }
}
model {
  xi_sigma ~ normal(0, 0.2);
  to_vector(raw_xi) ~ std_normal();
  mu_sigma ~ normal(0, 1);
  to_vector(raw_mu) ~ std_normal();

  y_state ~ binomial_logit(n_state, logit_outcome_state + xi[x] + mu[t_state]);
  y_national ~ binomial_logit(n_national, xi_national[t_national]);
}
generated quantities {
  matrix[S, T] zeta_matrix;
  matrix[S, T] epsilon;
  matrix[S, T] y_hat;
  for (tt in 1:T){
    zeta_matrix[:, tt] = xi[((tt - 1) * S + 1):(tt * S)];
    y_hat[:, tt] = inv_logit(logit(outcome_state[((tt - 1) * S + 1):(tt * S)]) +
      xi[((tt - 1) * S + 1):(tt * S)]);
    epsilon[:, tt] = 100 * (outcome_state[((tt - 1) * S + 1):(tt * S)] - y_hat[:, tt]);
  }
}
