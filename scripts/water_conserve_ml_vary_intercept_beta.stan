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
}

parameters {
  real<lower = 0> phi;
  real alpha;
  vector[J] gamma; // slopes of group-level predictors
  vector[K] beta;
}

model {
  vector[N] theta;
  vector[N] a;
  vector[N] b;

  alpha ~ cauchy(0,10);

  gamma ~ cauchy(0,2.5); // slopes of group-level predictors
  beta ~ cauchy(0,2.5);
#  phi ~ gamma(1,1);
  phi ~ cauchy(mu_phi, sigma_phi);

  for(i in 1:N) {
    int j;
    real mu;

    j = group[i];
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
    int j;
    real mu;
    real theta;
    real a;
    real b;

    j = group[i];
    mu = alpha + xx[i,:] * beta;
    theta = inv_logit(mu);

    a = theta * phi;
    b = (1 - theta) * phi;

    log_lik[i] = beta_binomial_lpmf(y[i] | K_ACTIONS, a, b);
    y_pred[i] = beta_binomial_rng(K_ACTIONS, a, b);
  }
}
