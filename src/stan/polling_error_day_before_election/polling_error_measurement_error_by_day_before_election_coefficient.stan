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
  vector[21] seq;
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
  vector[T] beta;
  vector[T] mu;
  real<lower = 0.00001> beta_sigma;
  real<lower = 0.00001> mu_sigma;
}
transformed parameters {
  vector[T] xi_national;
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  matrix[21, T] inv_logit_alpha;
  matrix[21, T] m1_inv_logit_alpha;
  xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi_national[tt] = logit(turnout_weights[tt] * inv_logit(xi[(1 + (tt - 1) * S):(tt * S)]));
  }
  for (tt in 1:T){
    inv_logit_alpha[,tt] = inv_logit(mu[tt] + beta[tt] * seq);
  }
  m1_inv_logit_alpha = 1 - inv_logit_alpha;
}
model {
  mu_sigma ~ normal(0, 1);
  beta_sigma ~ normal(0, 1);
  mu ~ normal(0, mu_sigma);
  beta ~ normal(0, beta_sigma);
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
