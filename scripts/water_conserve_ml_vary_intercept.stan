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
}

parameters {
  real alpha_0;
  vector[J] gamma; // slopes of group-level predictors
  vector[K] beta;
}

transformed parameters {
  vector[M] alpha;

  alpha = alpha_0 + w * gamma;
}

model {
  vector[N] theta;

  alpha_0 ~ cauchy(0,10);

  gamma ~ cauchy(0,2.5); // slopes of group-level predictors
  beta ~ cauchy(0,2.5);

  for(i in 1:N) {
    int j;
    real mu;

    j = group[i];
    mu = alpha[j] + xx[i,:] * beta;
    theta[i] = inv_logit(mu);
  }

  y ~ binomial(K_ACTIONS, theta);
}

generated quantities {
  vector[N] log_lik;
  int y_pred[N];

  for(i in 1:N) {
    int j;
    real mu;
    real theta;

    j = group[i];
    mu = alpha[j] + xx[i,:] * beta;
    theta = inv_logit(mu);

    log_lik[i] = binomial_lpmf(y[i] | K_ACTIONS, theta);
    y_pred[i] = binomial_rng(K_ACTIONS, theta);
  }
}
