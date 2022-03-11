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
  vector<lower = 0, upper = 1>[T] alpha;
}
transformed parameters {
  vector[T] xi_national;
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  xi = xi_sigma * raw_xi;
  for (tt in 1:T){
    xi_national[tt] = logit(turnout_weights[tt] * inv_logit(xi[(1 + (tt - 1) * S):(tt * S)]));
  }
}
model {
  alpha ~ beta(2, 2);
  xi_sigma ~ normal(0, 1);
  to_vector(raw_xi) ~ std_normal();
  y_state ~ binomial_logit(n_state, logit(alpha[t_state] .* outcome_state + (1 - alpha[t_state]) * 0.5) + xi[x]);
  //y_national ~ binomial(n_national, alpha[t_national] .* inv_logit(logit_outcome_national +
  //  xi_national[t_national]) + (1 - alpha[t_national]) * 0.5);
}
generated quantities {
  matrix[S, T] mu_matrix;
  for (tt in 1:T){
    mu_matrix[:, tt] = xi[((tt - 1) * S + 1):(tt * S)];
  }
}
