data{
  int<lower=1> N;                   // num obs
  int<lower=1> J;                   // num groups
  int<lower=1> K;                   // num coefficients
  int<lower=1, upper=J> GroupID[N]; // GroupID for obs, e.g. FirmID or Industry-YearID
  // vector[N] y;                      // Response
  matrix[N, K] x;                   // Predictors (incl. Intercept)
}
generated quantities {
  matrix[K, J] z;                  // standard normal sampler
  cholesky_factor_corr[K] L_Omega; // hypprior coefficient correlation
  vector<lower=0>[K] tau;          // hypprior coefficient scales
  vector[K] mu_b;                  // hypprior mean coefficients
  real<lower=0> sigma;             // error-term scale
  matrix[J, K] b;                  // coefficient vector
  array[N] real y_pred;

  for (k in 1:K) {
    for (j in 1:J){
      z[k, j] = normal_rng(0, 1);
    }
  }
  L_Omega = lkj_corr_cholesky_rng(K, 2);
  mu_b[1] = normal_rng(0, 0.1);
  mu_b[2] = normal_rng(0, 40);
  sigma = exponential_rng(1.0 / 0.08);   // exp: 0.08 (std (abnormal returns))
  tau[1] = exponential_rng(1.0 / 0.1);   // exp: 0.1
  tau[2] = exponential_rng(1.0 / 40);    // exp: 40
  b = (rep_matrix(mu_b, J) + diag_pre_multiply(tau,L_Omega) * z)';
  y_pred = normal_rng(rows_dot_product(b[GroupID] , x), sigma);
}
