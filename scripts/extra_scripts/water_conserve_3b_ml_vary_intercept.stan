data {
  int<lower=1> M; // # groups
  int<lower=0> N; // # observations
  int<lower=1> J; // # group-level predictors
  int<lower=0> K; // # predictors
  matrix[M,J] w;  // # group-level predictors
  matrix[N,K] x;  // predictors (real)
  int<lower=1,upper=M> group[N];
  int y[N];      // vwci
  int<lower=1> K_ACTIONS; // number of possible actions
}

parameters {
  real<lower=0> sigma;
  vector[M] eps;
  vector[J] gamma; // slopes of group-level predictors
  vector[K] beta;
}

transformed parameters {
  vector[M] alpha;
  
  alpha = w * gamma + eps;
}

model {
  real theta[N];
  
  gamma[1] ~ cauchy(0,2.5); // intercept
  gamma[2:J] ~ cauchy(0,2.5); // slopes of group-level predictors
  beta ~ cauchy(0,2.5);
  sigma ~ cauchy(0,1);
  eps ~ normal(0, sigma);

  for(i in 1:N) {
    int j;
    real mu;

    j = group[i];
    mu = alpha[j] + x[i,:] * beta;
    theta[i] = inv_logit(mu);
  }

  y ~ binomial(K_ACTIONS, theta);
}
