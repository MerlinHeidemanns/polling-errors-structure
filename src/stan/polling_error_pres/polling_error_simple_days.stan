data {
  int N;
  int S; // State
  int T; // Year
  int T_days;
  int t[N]; // pre election
  int x[N];
  int y[N];
  int n[N];
  vector[N] outcome;
  int xi_index[S * T];
}
transformed data {
  vector[N] logit_outcome;
  logit_outcome = logit(outcome);
}
parameters {
  real alpha;
  //vector<lower = 0>[T] xi_sigma;
  //matrix[S * T, T_days] raw_xi;
  matrix[S * T, T_days] xi;
}
//transformed parameters {
  //matrix[S * T, T_days] xi;
  //xi[,1] = raw_xi[,1] .* xi_sigma[xi_index];
  //for (tt in 2:T_days) xi[,tt] = xi[,tt - 1] + raw_xi[,tt] .* xi_sigma[xi_index];
//}
model {
  //xi_sigma ~ normal(0, 4);
  //to_vector(raw_xi) ~ std_normal();
  to_vector(xi) ~ normal(0, 2);
  alpha ~ normal(0, 2);
  for (nn in 1:N){
    y[nn] ~ binomial_logit(n[nn], logit_outcome[nn] + alpha +
      xi[x[nn], t[nn]]);
  }
}
generated quantities {
  matrix[S, T] mu_matrix[T_days];
  for (jj in 1:T_days){
    for (tt in 1:T){
      mu_matrix[jj, :, tt] = alpha + xi[((tt - 1) * 50 + 1):(tt * 50), jj];
    }
  }
}






