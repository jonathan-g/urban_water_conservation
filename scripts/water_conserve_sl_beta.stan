data {
  int<lower=0> N; // # observations
  int<lower=0> K; // # predictors
  matrix[N,K] xx;  // predictors (real)
  int y[N];      // vwci
  int<lower=1> K_ACTIONS; // number of possible actions
  real mu_phi;
  real<lower=0> sigma_phi;
}

parameters {
  real<lower = 0> phi;
  real alpha;
  vector[K] beta;
}

model {
  vector[N] theta;
  vector[N] a;
  vector[N] b;

  alpha ~ cauchy(0,2.5);
  beta ~ cauchy(0,2.5);
  phi ~ cauchy(mu_phi, sigma_phi);

  for(i in 1:N) {
    real mu;

    mu = alpha + xx[i,:] * beta;
    theta[i] = inv_logit(mu);
  }

  a = theta * phi;
  b = (1 - theta) * phi;

  y ~ beta_binomial(K_ACTIONS, a, b);
}

generated quantities {
  vector[N] log_lik;
  int y_pred[N];

   for(i in 1:N) {
    real mu;
    real theta;

    mu = alpha + xx[i,:] * beta;
    theta = inv_logit(mu);

    log_lik[i] = binomial_lpmf(y[i] | K_ACTIONS, theta);
    y_pred[i] = binomial_rng(K_ACTIONS, theta);
  }
}
