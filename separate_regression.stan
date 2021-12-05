data {
  int<lower=0> N; // number of obervations per series
  int<lower=0> M; // number of series
  matrix[N, M] x;
  matrix[N, M] y;
  matrix[N, M] x_pred;
}

parameters {

  // regression coefficients and noise variance
  vector[M] alpha;
  vector[M] beta;
  vector<lower=0>[M] sigma;
  
}

model {
  for (j in 1:M){

    // priors
    alpha[j] ~ normal(0, 1);
    beta[j] ~ normal(0, 1);
    sigma[j] ~ inv_chi_square(1);

    // likelihood
    y[,j] ~ normal(alpha[j] + beta[j] * x[,j], sigma[j]);
  }
}

generated quantities {

  matrix[N, M] log_lik; // log-likelihood matrix
  matrix[N, M] y_pred; // posterior predictions

  for (i in 1:N) {
    for (j in 1:M) {
      log_lik[i,j] = normal_lpdf(y[i,j ] | alpha[j] + beta[j] * x[i,j], sigma[j]);
      y_pred[i,j] = normal_rng(alpha[j] + beta[j] * x_pred[i,j], sigma[j]);
    }
  }

}