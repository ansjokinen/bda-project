data {
  int<lower=1> Tm;            // num observations
  int<lower=0> N; //number of steps to predict
  real y[Tm];                 // observed outputs
}
parameters {
  real mu;                   // mean coeff
  real phi;                  // autoregression coeff
  real theta;                // moving avg coeff
  //real sphi;
  real stheta;
  real<lower=0> sigma;       // noise scale
}
model {
  vector[Tm] nu;              // prediction for time t
  vector[Tm] err;             // error for time t
  nu[1] = mu + phi * mu;    // assume err[0] == 0
  
  err[1] = 0;
  
  //for (t in 2:52) {
    //nu[t] = mu + phi * mu;
    //err[t] = nu[t]-y[t];
  //  nu[t] = 0;
  //  err[t] = 0;
  //}
  

  
  //err[1] = y[1] - nu[1];
  
  for (t in 53:Tm) {
    nu[t] = mu + phi * y[t-1] + theta * err[t-1]; //+ stheta*err[t-52]; //sphi * y[t-52];
    err[t] = y[t] - nu[t];
  }
  mu ~ normal(0, 10);         // priors
  phi ~ normal(0, 1);
  //sphi ~ normal(0,1);
  theta ~ normal(0, 2);
  sigma ~ normal(0, 1);
  stheta ~ normal(0, 1);
  err ~ normal(0, sigma);    // likelihood
}








