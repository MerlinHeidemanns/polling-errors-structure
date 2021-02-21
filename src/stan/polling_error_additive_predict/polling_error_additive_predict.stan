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

  int pred_r[S];
  int pred_d[S];
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
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
}
transformed parameters {
  vector[S * T] xi; // S1 1, S2 1, S3 1, ..., S1 2 (300)
  xi = to_vector(diag_pre_multiply(xi_sigma, L_Omega) * raw_xi);
}
model {
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
  y ~ binomial_logit(n, logit_outcome +
    alpha +
    beta[s] +
    gamma[r] +
    delta[d] +
    eta[t] +
    gamma_eta[rt] +
    delta_eta[dt] +
    xi[x]);
}
generated quantities {
  vector[S] pred;
  real eta_new;
  vector[R] gamma_eta_new;
  vector[D] delta_eta_new;
  vector[S] xi_new;
  eta_new = normal_rng(0, sigma[4]);
  gamma_eta_new = to_vector(normal_rng(rep_vector(0.0, R), sigma[5]));
  delta_eta_new = to_vector(normal_rng(rep_vector(0.0, D), sigma[6]));
  xi_new = to_vector(diag_pre_multiply(xi_sigma, L_Omega) * to_vector(normal_rng(rep_vector(0.0, S), 1)));
  pred = alpha +
    beta +
    gamma[pred_r] +
    delta[pred_d] +
    eta_new +
    gamma_eta_new[pred_r] +
    delta_eta_new[pred_d];
}






