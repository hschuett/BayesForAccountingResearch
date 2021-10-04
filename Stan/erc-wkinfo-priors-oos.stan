data{
  int<lower=1> N;                   // num obs
  int<lower=1> J;                   // num groups
  int<lower=1> K;                   // num coefficients
  int<lower=1, upper=J> GroupID[N]; // GroupID for obs, e.g. FirmID or Industry-YearID
  vector[N] y;                      // Response
  matrix[N, K] x;                   // Predictors (incl. Intercept)

  // data for the oos test
  int<lower=1> N_test;              // num obs in test sample
  matrix[N_test, K] x_test;              // Predictors (incl. Intercept)
  int<lower=1, upper=J> GroupID_test[N_test]; // GroupID for  for test sample obs
}
parameters{
  matrix[K, J] z;                  // standard normal sampler
  cholesky_factor_corr[K] L_Omega; // hypprior coefficient correlation
  vector<lower=0>[K] tau;          // hypprior coefficient scales
  vector[K] mu_b;                  // hypprior mean coefficients
  real<lower=0> sigma;             // error-term scale
}
transformed parameters{
  matrix[J, K] b;                  // coefficient vector
  // The multivariate non-centered version:
  b = (rep_matrix(mu_b, J) + diag_pre_multiply(tau,L_Omega) * z)';
}
model{
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  mu_b[1]  ~ normal(0, 0.1);
  mu_b[2]  ~ normal(0, 40);
  sigma ~ exponential(1.0 / 0.08);   // exp: 0.08 (std (abnormal returns))
  tau[1] ~ exponential(1.0 / 0.1);   // exp: 0.1
  tau[2] ~ exponential(1.0 / 40);    // exp: 40
  y ~ normal(rows_dot_product(b[GroupID] , x), sigma);
}
generated quantities {
  vector[N_test] y_pred = rows_dot_product(b[GroupID_test] , x_test);
}
