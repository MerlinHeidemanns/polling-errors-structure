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
  matrix[S, T] raw_xi;
  cholesky_factor_corr[S] L_Omega;
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  xi = to_vector(diag_pre_multiply(xi_sigma, L_Omega) * raw_xi);
}
model {
  L_Omega ~ lkj_corr_cholesky(0.5);
  to_vector(raw_xi) ~ std_normal();
  xi_sigma ~ normal(0, 0.1);
  y ~ binomial_logit(n, logit_outcome +
    xi[x]);
}
generated quantities {
  corr_matrix[S] Omega;
  vector[S * T] mu;
  matrix[S, T] mu_matrix;
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  mu = xi[corr_x];
  for (tt in 1:T){
    mu_matrix[:, tt] = mu[((tt - 1) * 50 + 1):(tt * 50)];
  }
}






