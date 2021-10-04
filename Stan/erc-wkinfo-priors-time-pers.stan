data{
  int<lower=1> N;                   // num obs
  int<lower=1> J;                   // num groups
  int<lower=1> K;                   // num coefficients
  int<lower=1> M;                   // num periods
  int<lower=1, upper=J> GroupID[N]; // GroupID for obs, e.g. FirmID or Industry-YearID
  int<lower=1, upper=M> TimeID[N];  // GroupID for obs, e.g. FirmID or Industry-YearID
  vector[N] y;                      // Response
  vector[N] x;                      // Predictor (without Intercept)
}
parameters{
  matrix[K, J] z;                  // standard normal sampler
  cholesky_factor_corr[K] L_Omega; // hypprior coefficient correlation
  vector<lower=0>[K] tau;          // hypprior coefficient scales
  real<lower=0> sigma;             // error-term scale
  real mu_0;

  real<lower=0,upper=1> rho_raw;   // used to construct rho, the AR(1) coefficient
  real<lower=0> sig_t;             // error-term scale
  vector[M] z_t;
}
transformed parameters{
  matrix[J, K] b_i;                // firm-level components
  vector[K] mu_b;                  // hypprior mean firm-level coefficients
  // The multivariate non-centered version:
  mu_b[1] = mu_0;
  mu_b[2] = 0;
  b_i = (rep_matrix(mu_b, J) + diag_pre_multiply(tau,L_Omega) * z)';

  // non-centered parameterization of AR(1) process priors
  real rho = 2 * rho_raw - 1;      // ensures that rho is between -1 and 1
  vector[M] b_t = sig_t * z_t;     // all of them share this term
  b_t[1] /= sqrt(1 - rho^2);       // mo[1] = mo[1] / sqrt(1 - rho^2)
  for (m in 2:M) {
    b_t[m] += rho * b_t[m-1];      // mo[m] = mo[m] + rho * mo[m-1];
  }

}
model{
  to_vector(z) ~ normal(0, 1);
  z_t ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  mu_0  ~ normal(0, 0.1);
  rho_raw  ~ beta(10, 5);

  sigma ~ exponential(1.0 / 0.08);   // exp: 0.08 (std (abnormal returns))
  tau[1] ~ exponential(1.0 / 0.1);   // exp: 0.1
  tau[2] ~ exponential(1.0 / 40);    // exp: 40
  sig_t ~ exponential(1.0 / 40);     // exp: 40

  vector[N] n_loc = b_i[GroupID, 1] + (b_t[TimeID] + b_i[GroupID, 2]) .* x;

  y ~ normal(n_loc, sigma);
}
