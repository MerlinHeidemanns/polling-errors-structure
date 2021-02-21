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
  real<lower = 0> sigma_error;
  real<lower = 0> beta_sigma_error;
  vector[T] beta_error;
}
model {
  vector[T * S] mu;
  alpha ~ normal(0, 0.1);
  sigma ~ normal(0, 0.1);
  beta ~ normal(0, sigma[1]);
  gamma ~ normal(0, sigma[2]);
  delta ~ normal(0, sigma[3]);
  eta ~ normal(0, sigma[4]);
  gamma_eta ~ normal(0, sigma[5]);
  delta_eta ~ normal(0, sigma[6]);
  sigma_error ~ normal(0, 1);
  beta_sigma_error ~ normal(0, 0.1);
  beta_error ~ normal(0, beta_sigma_error);

  mu = Z ./ beta_error[corr_t];
  // poll model
  y ~ binomial_logit(n, logit_outcome +
    alpha +
    gamma[r] +
    delta[d] +
    eta[t] +
    gamma_eta[rt] +
    delta_eta[dt] +
    mu[x]);

  // predicting polling error size
}
generated quantities {
  vector[S * T] pred;
  matrix[S, T] mu_matrix;
  pred = alpha +
    beta[corr_s] +
    gamma[corr_r] +
    delta[corr_d] +
    eta[corr_t] +
    gamma_eta[corr_rt] +
    delta_eta[corr_dt];
  for (tt in 1:T){
    mu_matrix[:, tt] = pred[((tt - 1) * 50 + 1):(tt * 50)];
  }
}






