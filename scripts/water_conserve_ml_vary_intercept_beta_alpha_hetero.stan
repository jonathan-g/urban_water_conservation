data {
  int<lower=1> M; // # groups
  int<lower=0> N; // # observations
  int<lower=1> J; // # group-level predictors
  int<lower=0> K; // # predictors
  matrix[M,J] w;  // # group-level predictors
  matrix[N,K] xx;  // predictors (real)
  int<lower=1,upper=M> group[N];
  int y[N];      // vwci
  int<lower=1> K_ACTIONS; // number of possible actions
  real mu_phi;
  real<lower=0> sigma_phi;
  real<lower=0> sig_sig_delta;
}

parameters {
  real<lower = 0> phi;
  real log_scale_phi;
  real alpha_0;
  real<lower = 0> sigma_delta;
  vector[M - 1] delta_raw;
  vector[J] gamma; // slopes of group-level predictors
  vector[K] beta;
}

transformed parameters {
  vector[M] alpha;
  vector[M] delta;
  real scale_phi;

  delta[1] = - sum(delta_raw);
  delta[2:M] = delta_raw;

  alpha = alpha_0 + delta + w * gamma;
  scale_phi = exp(log_scale_phi);
}

model {
  vector[N] theta;
  vector[N] a;
  vector[N] b;
  vector[N] phi_prime;

  alpha_0 ~ cauchy(0,2.5);
  sigma_delta ~  cauchy(0,sig_sig_delta);

  delta_raw ~ normal(0,sigma_delta);
  log_scale_phi ~ cauchy(0,0.5);

  gamma ~ cauchy(0,2.5); // slopes of group-level predictors
  beta ~ cauchy(0,2.5);
// phi ~ gamma(1,1);
  phi ~ cauchy(mu_phi, sigma_phi);

  for(i in 1:N) {
    int j;
    real mu;

    j = group[i];
    mu = alpha[j] + xx[i,:] * beta;
    theta[i] = inv_logit(mu);
    phi_prime[i] = phi * exp(-mu * scale_phi);
  }
  a = theta .* phi_prime;
  b = (1 - theta) .* phi_prime;

  y ~ beta_binomial(K_ACTIONS, a, b);
}

generated quantities {
  vector[N] log_lik;
  int y_pred[N];

  for(i in 1:N) {
    int j;
    real mu;
    real theta;
    real a;
    real b;
    real phi_prime;

    j = group[i];
    mu = alpha[j] + xx[i,:] * beta;
    theta = inv_logit(mu);
    phi_prime = phi * exp(-mu * scale_phi);

    a = theta * phi_prime;
    b = (1 - theta) * phi_prime;

    log_lik[i] = beta_binomial_lpmf(y[i] | K_ACTIONS, a, b);
    y_pred[i] = beta_binomial_rng(K_ACTIONS, a, b);
  }
}
