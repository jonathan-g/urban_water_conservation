data {
  int<lower=0> N; // # MSA's
  int<lower=1> K; // # MSA-level predictors
  int<lower=1> J; // # groups
  int<lower=1> L; // # group-level predictors
  matrix[N,K] x;      // MSA-levelpredictors (real)
  matrix[J,L] u; // group-level predictors
  int<lower=1,upper=J> group[N];
  int y[N];      // vwci
  int<lower=1> K_ACTIONS; // Number of possible actions
}

parameters {
  matrix[K,J] z;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] tau;
  matrix[L,K] gamma;      // group coeffs
}

transformed parameters {
  matrix[J,K] beta;
  // vector<lower=0>[K] tau; // prior scale
  // for (k in 1:K) {
  //  tau[k] <- 2.5 * tan(tau_unif[k]);
  //}
  beta <- u * gamma + (diag_pre_multiply(tau, L_Omega) * z)';
}

model {
  vector[N] theta;

  to_vector(z) ~ normal(0,1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(gamma) ~ normal(0,3);
  tau ~ normal(0,2);
  // pdf for tau_unif is uniform on [0,pi/2]

  for (n in 1:N) {
    real phi;

    phi <- x[n] * beta[group[n]]';
    theta[n] <- inv_logit(phi);
  }

  y ~ binomial(K_ACTIONS, theta);
}
