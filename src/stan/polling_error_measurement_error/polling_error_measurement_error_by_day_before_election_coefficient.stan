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
  vector[21] seq;
  int t_state_before_election[N_state];
  int t_national_before_election[N_national];
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
  vector<lower = 0, upper = 1>[T] lambda;
  vector[T] beta;
  vector[T] mu;
  real<lower = 0.00001> beta_sigma;
  real<lower = 0.00001> mu_sigma;
}
transformed parameters {
  vector[T] xi_national;
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2, .... (300)
  matrix[21, T] inv_logit_lambda;
  xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi[8 + (tt - 1) * S] = 0.0; // DC
    xi_national[tt] = turnout_weights[tt] * inv_logit(logit(outcome_state[(1 + (tt - 1) * S):(tt * S)]) + xi[(1 + (tt - 1) * S):(tt * S)]);
  }
  for (tt in 1:T){
    inv_logit_lambda[,tt] = inv_logit(mu[tt] + beta[tt] * seq);
  }
}

model {
  mu_sigma ~ normal(0, 10);
  beta_sigma ~ normal(0, 10);
  mu ~ normal(0, mu_sigma);
  beta ~ normal(0, beta_sigma);
  xi_sigma ~ normal(0, 1);
  to_vector(raw_xi) ~ std_normal();
  for (nn in 1:N_state){
    y_state[nn] ~ binomial(n_state[nn], (1 - inv_logit_lambda[t_state_before_election[nn], t_state[nn]]) .* inv_logit(logit_outcome_state[nn] + xi[x[nn]]) +
      inv_logit_lambda[t_state_before_election[nn], t_state[nn]] * 0.5);
  }
  for (nn in 1:N_national){
    y_national[nn] ~ binomial(n_national[nn], (1 - inv_logit_lambda[t_national_before_election[nn], t_national[nn]]) .* xi_national[t_national[nn]] + inv_logit_lambda[t_national_before_election[nn], t_national[nn]] * 0.5);
  }
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
