data {
  int<lower=1> J; // # predictors
  int<lower=1> M; // # groups
  int<lower=0> N; // # observations
  matrix[N,J] x;  // predictors (real)
  int<lower=1,upper=M> group[N];
  int y[N];      // vwci
  int<lower=1> K_ACTIONS; // number of possible actions
}

parameters {
  real mu_alpha; // intercept
  # real<lower = 0> sigma_alpha;
  vector[J] beta;
  # vector<lower=0>[J] sigma_beta;

  vector[M-1] alpha_raw;
}

transformed parameters {
  vector[M] alpha;

  alpha[1:(M-1)] <- alpha_raw;
  alpha[M] <- - sum(alpha_raw);
#  alpha <- alpha * sigma_alpha + mu_alpha;
  alpha <- alpha + mu_alpha;
}

model {
  real theta[N];

  mu_alpha ~ normal(0,4);
#  sigma_alpha ~ normal(0,4);
  beta ~ normal(0,4);
#  sigma_beta ~ normal(0,4);

  alpha_raw ~ normal(0,4);

  for(i in 1:N) {
    int j;
    real mu;

    j <- group[i];
    mu <- alpha[j] + x[i,:] * beta;
    theta[i] <- inv_logit(mu);
  }

  y ~ binomial(K_ACTIONS, theta);
}
