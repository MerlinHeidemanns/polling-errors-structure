data {
  int N_state;
  int N_national;
  int S; // State
  int T; // Year
  int t_state_before_election[N_state];
  int t_national_before_election[N_national];
  int t_state[N_state];
  int t_national[N_national];
  int s[N_state];
  int x[N_state];
  int y_state[N_state];
  int n_state[N_state];
  int n_national[N_national];
  int y_national[N_national];
  vector[N_state] outcome_state;
  vector[N_national] outcome_national;
  matrix[T, S] turnout_weights;
}
transformed data {
  vector[N_national] logit_outcome_national;
  vector[N_state] logit_outcome_state;
  vector[N_state] ones_state = rep_vector(1, N_state);
  vector[N_national] ones_national = rep_vector(1, N_national);
  logit_outcome_national = logit(outcome_national);
  logit_outcome_state = logit(outcome_state);
}
parameters {
  real<lower = 0.00001> xi_sigma;
  vector[S * T] raw_xi;
  vector[21] raw_alpha;
  real<lower = 0.00001> alpha_sigma;
  vector[T] mu_alpha;
}
transformed parameters {
  vector[T] xi_national;
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[21] alpha;
  matrix[21, T] inv_logit_alpha;
  xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi_national[tt] = logit(turnout_weights[tt] * inv_logit(xi[(1 + (tt - 1) * S):(tt * S)]));
  }
  alpha[21] = raw_alpha[21] * alpha_sigma;
  for (tt in 1:20){
    alpha[21 - tt] = alpha[22 - tt] + raw_alpha[21 - tt] * alpha_sigma;
  }
  for (tt in 1:T){
    inv_logit_alpha[, tt] = inv_logit(alpha + mu_alpha[tt]);
  }
}
model {
  mu_alpha ~ normal(0, 1);
  raw_alpha ~ std_normal();
  alpha_sigma ~ normal(0, 1);
  xi_sigma ~ normal(0, 1);
  to_vector(raw_xi) ~ std_normal();
  for (nn in 1:N_state){
    y_state[nn] ~ binomial(n_state[nn], inv_logit_alpha[t_state_before_election[nn], t_state[nn]] .* inv_logit(logit_outcome_state[nn] + xi[x[nn]]) +
      (1 - inv_logit_alpha[t_state_before_election[nn], t_state[nn]]) * 0.5);
  }
  for (nn in 1:N_national){
    y_national[nn] ~ binomial(n_national[nn], inv_logit_alpha[t_national_before_election[nn], t_national[nn]] .* inv_logit(logit_outcome_national[nn] +
      xi_national[t_national[nn]]) + (1 - inv_logit_alpha[t_national_before_election[nn], t_national[nn]]) * 0.5);
  }
}
generated quantities {
  matrix[S, T] mu_matrix;
  for (tt in 1:T){
    mu_matrix[:, tt] = xi[((tt - 1) * S + 1):(tt * S)];
  }
}
