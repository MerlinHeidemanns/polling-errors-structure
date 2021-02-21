data {
  int N;
  int S; // State
  int R; // Region
  int D; // Division
  int T; // Year
  int rt[N];
  int dt[N];
  int t[N];
  int s[N];
  int r[N];
  int d[N];
  int x[N];
  int y[N];
  int n[N];
  vector[N] outcome;

  int corr_rt[S * T];
  int corr_dt[S * T];
  int corr_t[S * T];
  int corr_s[S * T];
  int corr_r[S * T];
  int corr_d[S * T];
  int corr_x[S * T];

  vector[T * S] Z;
  vector[T * S] outcome_ts;
}
transformed data {
  vector[N] logit_outcome = logit(outcome);
  vector[T * S] logit_outcome_ts = logit(outcome_ts);
}
parameters {
  real alpha;
  vector[S] beta;
  vector[R] gamma;
  vector[D] delta;
  vector[T] eta;
  vector[R * T] gamma_eta;
  vector[D * T] delta_eta;
  vector<lower = 0>[6] sigma;
  vector<lower = 0>[S] xi_sigma;
  matrix[S, T] raw_xi;
  cholesky_factor_corr[S] L_Omega;
  real<lower = 0> sigma_error;
  real<lower = 0> beta_sigma_error;
  vector[T] beta_error;
  real alpha_error;
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  xi = to_vector(diag_pre_multiply(xi_sigma, L_Omega) * raw_xi);
}
model {
  vector[T * S] mu = inv_logit(alpha +
    beta[corr_s] +
    gamma[corr_r] +
    delta[corr_d] +
    eta[corr_t] +
    gamma_eta[corr_rt] +
    delta_eta[corr_dt] +
    xi[corr_x] + logit_outcome_ts) -
    outcome_ts;
  alpha ~ normal(0, 0.1);
  sigma ~ normal(0, 0.1);
  beta ~ normal(0, sigma[1]);
  gamma ~ normal(0, sigma[2]);
  delta ~ normal(0, sigma[3]);
  eta ~ normal(0, sigma[4]);
  gamma_eta ~ normal(0, sigma[5]);
  delta_eta ~ normal(0, sigma[6]);
  L_Omega ~ lkj_corr_cholesky(0.5);
  to_vector(raw_xi) ~ std_normal();
  xi_sigma ~ normal(0, 0.1);
  // poll model
  y ~ binomial_logit(n, logit_outcome +
    alpha +
    beta[s] +
    gamma[r] +
    delta[d] +
    eta[t] +
    gamma_eta[rt] +
    delta_eta[dt] +
    xi[x]);
  sigma_error ~ normal(0, 1);
  beta_sigma_error ~ normal(0, 0.1);
  beta_error ~ normal(0, beta_sigma_error);
  alpha_error ~ normal(0, 1);
  // predicting polling error size
  mu ~ normal(alpha_error + Z ./ beta_error[corr_t], sigma_error);
}
generated quantities {
  corr_matrix[S] Omega;
  vector[S * T] mu;
  matrix[S, T] mu_matrix;
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  mu = alpha +
    beta[corr_s] +
    gamma[corr_r] +
    delta[corr_d] +
    eta[corr_t] +
    gamma_eta[corr_rt] +
    delta_eta[corr_dt] +
    xi[corr_x];
  for (tt in 1:T){
    mu_matrix[:, tt] = mu[((tt - 1) * 50 + 1):(tt * 50)];
  }
}






