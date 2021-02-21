data {
  int N;
  int S; // State
  int R; // Region
  int D; // Division
  int T; // Year
  int rt[N];
  int dt[N];
  int t[N];
  int x[N];
  int y[N];
  int n[N];
  vector[N] outcome;

  int pred_r[S];
  int pred_d[S];
  int pred_x[S];
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
  vector[T - 1] raw_eta;
  vector[R * (T - 1)] raw_gamma_eta;
  vector[D * (T - 1)] raw_delta_eta;
  vector<lower = 0>[4] sigma;
  vector[S *  (T - 1)] raw_xi;
  real<lower = 0> rho_sigma[3];
  real rho_eta;
  vector[R] rho_gamma_eta;
  vector[D] rho_delta_eta;
  vector[S] rho_xi;
}
transformed parameters {
  vector[T] eta;
  vector[R * T] gamma_eta;
  vector[D * T] delta_eta;
  vector[S * T] xi;
  eta[1] = alpha;
  gamma_eta[1:R] = gamma;
  delta_eta[1:D] = delta;
  xi[1:S] = beta;
  for (tt in 2:T){
    eta[tt] = rho_eta * eta[tt - 1] + raw_eta[tt - 1] * sigma[4];
    gamma_eta[1 + R * (tt - 1):(R * tt)] =
      rho_gamma_eta .* gamma_eta[1 + R * (tt - 2):(R * (tt - 1))] +
      raw_gamma_eta[1 + R * (tt - 2):(R * (tt - 1))] * sigma[3];
    delta_eta[1 + D * (tt - 1):(D * tt)] =
      rho_delta_eta .* delta_eta[1 + D * (tt - 2):(D * (tt - 1))] +
      raw_delta_eta[1 + D * (tt - 2):(D * (tt - 1))] * sigma[2];
    xi[1 + S * (tt - 1):(S * tt)] =
      rho_xi .* xi[1 + S * (tt - 2):(S * (tt - 1))] + raw_xi[1 + S * (tt - 2):(S * (tt - 1))] * sigma[1];
  }
}

model {
  rho_sigma ~ normal(0, 0.25);
  rho_xi ~ normal(1, rho_sigma[1]);
  rho_delta_eta ~ normal(1, rho_sigma[2]);
  rho_gamma_eta ~ normal(1, rho_sigma[3]);
  rho_eta ~ normal(1, 0.25);
  sigma ~ normal(0, 0.1);
  alpha ~ normal(0, sigma[4]);
  beta ~ normal(0, sigma[1]);
  delta ~ normal(0, sigma[2]);
  gamma ~ normal(0, sigma[3]);
  raw_eta ~ std_normal();
  raw_gamma_eta ~ std_normal();
  raw_delta_eta ~ std_normal();
  raw_xi ~ std_normal();
  y ~ binomial_logit(n, logit_outcome +
    eta[t] +
    gamma_eta[rt] +
    delta_eta[dt] +
    xi[x]);
}
generated quantities {
  //vector[S * T] mu;
  //matrix[S, T] mu_matrix;
  vector[S] pred;
  real eta_new;
  vector[R] gamma_eta_new;
  vector[D] delta_eta_new;
  vector[S] xi_new;
  eta_new = normal_rng(rho_eta * eta[T], sigma[4]);
  gamma_eta_new = to_vector(normal_rng(rho_gamma_eta .* gamma_eta[1 + (R * (T - 1)):T * R], sigma[3]));
  delta_eta_new = to_vector(normal_rng(rho_delta_eta .* delta_eta[1 + (D * (T - 1)):T * D], sigma[2]));
  xi_new = to_vector(normal_rng(rho_xi .* xi[1 + (S * (T - 1)):T * S], sigma[1]));
  pred = eta_new +
      gamma_eta_new[pred_r[1:S]] +
      delta_eta_new[pred_d[1:S]] +
      xi_new[pred_x[1:S]];
  //mu = eta[corr_t] +
  //    gamma_eta[corr_rt] +
  //    delta_eta[corr_dt] +
  //    xi[corr_x];
  //for (tt in 1:T){
  //  mu_matrix[:, tt] = mu[((tt - 1) * 50 + 1):(tt * 50)];
  //}
}







