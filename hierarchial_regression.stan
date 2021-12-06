data {
  int<lower=0> N; // number of obervations per series
  int<lower=0> M; // number of series
  matrix[N, M] x;
  matrix[N, M] y;
  matrix[N, M+1] x_pred;
}

parameters {
  
  // hyperparameters
  real mu_alpha;
  real mu_beta;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  
  // regression coefficients and noise variance
  vector[M] alpha;
  vector[M] beta;
  real<lower=0> sigma;
}

model {
  
  // hyperpriors
  mu_alpha ~ normal(0, 1);
  mu_beta ~ normal(0, 1);
  sigma_alpha ~ inv_chi_square(0.1);
  sigma_beta ~ inv_chi_square(1);

  // prior
  sigma ~ inv_chi_square(1);
  
  for (j in 1:M){

    // priors
    alpha[j] ~ normal(mu_alpha, sigma_alpha);
    beta[j] ~ normal(mu_beta, sigma_beta);

    // likelihood
    y[,j] ~ normal(alpha[j] + beta[j] * x[,j], sigma);
  }
}

generated quantities {

  matrix[N, M] log_lik; // log-likelihood matrix
  matrix[N, M+1] y_pred; // posterior predictions
  real alpha_pred; // intercept for new station
  real beta_pred; // slope for new station

  alpha_pred = normal_rng(mu_alpha, sigma_alpha);
  beta_pred = normal_rng(mu_beta, sigma_beta);

  for (i in 1:N) {
    for (j in 1:M) {
      log_lik[i,j] = normal_lpdf(y[i,j ] | alpha[j] + beta[j] * x[i,j], sigma);
      y_pred[i,j] = normal_rng(alpha[j] + beta[j] * x_pred[i,j], sigma);
    }
    y_pred[i,M+1] = normal_rng(alpha_pred + beta_pred * x_pred[i,M+1], sigma);
  }

}
