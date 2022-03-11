data {
  int N_state_past;
  int N_state_current;
  int S; // State
  int T; // Year
  int t_state_past[N_state_past];
  int s_past[N_state_past];
  int s_current[N_state_current];
  int x_past[N_state_past];
  int x_current[N_state_current];
  int y_state_past[N_state_past];
  int n_state_past[N_state_past];
  int y_state_current[N_state_current];
  int n_state_current[N_state_current];
  vector[S * T] outcome_state;
}
transformed data {
  vector[N_state_past + N_state_current] logit_outcome_state;
  logit_outcome_state = logit(append_row(
    outcome_state[x_past],
    outcome_state[x_current]));
}
parameters {
  real<lower = 0.00001> xi_sigma;
  vector[S * T] raw_xi;
  vector<lower = 0, upper = 1>[T - 1] lambda;
  vector[T] raw_xi_national;
  real<lower = 0> sigma_xi_national;
  vector[S] beta;
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[T] xi_national;
  xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi[8 + (tt - 1) * S] = 0.0;
  }
  xi_national = raw_xi_national * sigma_xi_national;
}
model {
  beta ~ normal(0, 0.2);
  raw_xi_national ~ std_normal();
  sigma_xi_national ~ normal(0, 0.01);
  lambda ~ beta(2, 2);
  xi_sigma ~ normal(0, 0.01);
  to_vector(raw_xi) ~ std_normal();
  y_state_past ~ binomial(n_state_past,
    (1 - lambda[t_state_past]) .* inv_logit(logit(outcome_state[x_past]) +
     + xi[x_past] + xi_national[t_state_past]) +
    lambda[t_state_past] * 0.5);
  y_state_current ~ binomial(n_state_current, inv_logit(beta[s_current] +
                                                          xi[x_current] +
                                                          xi_national[T]));
}
generated quantities {
  matrix[S, T] epsilon;
  matrix[S, T] y_hat;
  real lambda_new;
  vector[S] beta_new;
  vector[S] pred_new;
  vector[S] pred_base;
  for (tt in 1:(T - 1)){
    y_hat[:, tt] = inv_logit(logit(outcome_state[((tt - 1) * S + 1):(tt * S)]) +
      xi[((tt - 1) * S + 1):(tt * S)]);
    epsilon[:, tt] = 100 * (outcome_state[((tt - 1) * S + 1):(tt * S)] - y_hat[:, tt]);
  }
  lambda_new = normal_rng(mean(lambda), sd(lambda));
  beta_new   = logit((inv_logit(beta +
                                xi[((T - 1) * S + 1):(T * S)] +
                                xi_national[T]) - lambda_new * 0.5) / (1 - lambda_new));

  y_hat[:, T] = inv_logit(beta_new);
  pred_new = inv_logit(beta_new);
  pred_base = inv_logit(beta + xi[((T - 1) * S + 1):(T * S)] + xi_national[T]);
  epsilon[:, T] = 100 * (outcome_state[((T - 1) * S + 1):(T * S)] - y_hat[:, T]);
}
