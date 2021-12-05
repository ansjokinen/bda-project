
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> M; //nimber of series
  matrix[N, M] x;
  matrix[N, M] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[M] alpha;
  vector[M] beta;
  vector<lower=0>[M] sigma;
  real mu; 
  real<lower=0> tau; 
  vector[M] muvec; 
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:M){
    mu ~ normal(0, 1);
    tau ~ normal(0, 1);
    sigma ~ cauchy(0, 1);
    
    muvec[i] ~ normal(mu, tau);
    alpha[i] ~ normal(muvec[i], sigma);
    beta[i] ~ normal(muvec[i], sigma);
    y[,i] ~ normal(alpha[i] + beta[i] * x[,i], sigma);
  }
}
