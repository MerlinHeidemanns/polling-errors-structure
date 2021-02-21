data {
  int N;
  int S; // State
  int T; // Year
  int x[N];
  int y[N];
  int n[N];
  vector[N] outcome;

  int corr_x[S * T];
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  vector<lower = 0>[S] xi_sigma;
  vector[S] raw_xi;
  matrix[S, (T - 1)] raw_diff;
  cholesky_factor_corr[S] L_Omega;
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  vector[S * (T - 1)] diff;
  diff = to_vector(diag_pre_multiply(xi_sigma, L_Omega) * raw_diff);
  xi[1:S] = raw_xi;
  for (j in 2:T){
    xi[(1 + (j - 1) * S):(S * j)] = xi[(1 + (j - 2) * S):(S * (j - 1))] + diff[(1 + (j - 2) * S):(S * (j - 1))];
  }
}
model {
  L_Omega ~ lkj_corr_cholesky(1);
  to_vector(raw_diff) ~ std_normal();
  raw_xi ~ normal(0, 0.1);
  xi_sigma ~ normal(0, 0.1);
  y ~ binomial_logit(n, logit_outcome +
    xi[x]);
}
generated quantities {
  corr_matrix[S] Omega;
  vector[S * T] mu;
  matrix[S, T] mu_matrix;
  matrix[S, T - 1] diff_matrix;
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  for (tt in 1:T){
    mu_matrix[:, tt] = xi[((tt - 1) * 50 + 1):(tt * 50)];
  }
  for (tt in 1:(T - 1)){
    diff_matrix[:, tt] = diff[((tt - 1) * 50 + 1):(tt * 50)];
  }
}






