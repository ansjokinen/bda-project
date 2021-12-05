
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
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:M){
    alpha[i] ~ normal(0, 1);
    beta[i] ~ normal(0,1);
    y[,i] ~ normal(alpha[i] + beta[i] * x[,i], sigma[i]);
  }
}

