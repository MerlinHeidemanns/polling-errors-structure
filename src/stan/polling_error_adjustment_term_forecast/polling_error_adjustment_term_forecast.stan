data {
  int N_state;
  int S; // State
  int T; // Year
  int t_state[N_state];
  int s[N_state];
  int x[N_state];
  int y_state[N_state];
  int n_state[N_state];
  vector[S * T] outcome_state;
}
parameters {
  vector[S] beta;
  real<lower = 0.00001> xi_sigma;
  vector[S * T] raw_xi;
  vector<lower = 0, upper = 1>[T] lambda;
  real<lower = 0> sigma_lambda;
  real mu_lambda;
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[S * T] outcome_state_hat;
  vector[N_state] logit_outcome_state;

  outcome_state_hat[1:S * (T - 1)] = logit(outcome_state[1:S * (T - 1)]);
  outcome_state_hat[1 + S * (T - 1):S * T] = beta;
  logit_outcome_state = outcome_state_hat[x];

  // xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi[1 + S * (tt - 1):S * tt] = lambda[tt] * inv_logit(outcome_state_hat[1 + S * (tt - 1):S * tt]) +
      raw_xi[1 + S * (tt - 1):S * tt] * xi_sigma;
    xi[8 + (tt - 1) * S] = 0.0;
  }
}
model {
  beta ~ normal(0, 1);

  sigma_lambda ~ normal(0, 0.1);
  mu_lambda ~ normal(-1.3, 0.1);
  lambda ~ normal(0, 0.1);
  xi_sigma ~ normal(0, 0.1);
  to_vector(raw_xi) ~ std_normal();
  // y_state ~ binomial(n_state, (1 - lambda[t_state]) .* inv_logit(logit_outcome_state + xi[x]) +
  //   lambda[t_state] * 0.5);
  // for (tt in 1:T){
  //   xi[1 + S * (tt - 1):S * tt] ~ normal(lambda[tt] * inv_logit(outcome_state_hat[1 + S * (tt - 1):S * tt]), xi_sigma);
  // }
  // y_state ~ binomial_logit(n_state, logit((1 - lambda[t_state]) .* inv_logit(logit_outcome_state) +
  //   lambda[t_state] * 0.5) + xi[x]);
  y_state ~ binomial_logit(n_state, logit_outcome_state + xi[x]);
}
generated quantities {
  matrix[S, T] zeta_matrix;
  matrix[S, T] epsilon;
  matrix[S, T] y_hat;
  for (tt in 1:T - 1){
    zeta_matrix[:, tt] = xi[((tt - 1) * S + 1):(tt * S)];
    y_hat[:, tt] = inv_logit(logit(outcome_state[((tt - 1) * S + 1):(tt * S)]) +
      xi[((tt - 1) * S + 1):(tt * S)]);
    epsilon[:, tt] = 100 * (outcome_state[((tt - 1) * S + 1):(tt * S)] - y_hat[:, tt]);
  }
  zeta_matrix[:, 6] = xi[((6 - 1) * S + 1):(6 * S)];
  y_hat[:, 6] = inv_logit(beta + xi[((6 - 1) * S + 1):(6 * S)]);
  epsilon[:, 6] = 100 * (outcome_state[((6 - 1) * S + 1):(6 * S)] - y_hat[:, 6]);

}
