data {
  int<lower=1> N; // number of observations
  int<lower=1> L; // number of groups
  int<lower=1> K; // number of regressors (1 constant + 1 predictor)
  
  int<lower=1, upper=L> id[N]; // vector describing which group an observation is in
  
  matrix[N,K] X; // model matrix
  int Y[N]; // dependent variable
  vector[L] terror_return; // out-of-sample return observed on day of terror attack
}
parameters {
  vector[K] gamma; // population-level regression coefs
  vector<lower=0>[K] tau; // standard deviation of regression coefs
  
  vector[K] beta[L]; // matrix of group-level regression coefs
  real<lower=0> sigma; // st dev of individual observations
}
model {
  vector[N] mu; // linear predictor
  // priors
  gamma ~ normal(0,5); // weakly informative regression coefficient priors
  tau ~ cauchy(0, 2.5); // more weakly informative priors
  sigma ~ gamma(2, 0.1); // as above
  
  for(l in 1:L){
    beta[l] ~ normal(gamma, tau); // filling group level coefs
  }
  
  for(n in 1:N){
    mu[n] = X[n] * beta[id[n]];
  }
  
  // likelihood
  for(n in 1:N){
  Y[n] ~  bernoulli_logit(mu[n]);
  }
}
generated quantities {
  vector[L] y_hat;
  vector[L] mu_hat;
  
  for(l in 1:L){
    mu_hat[l] = X[l] * beta[l];
    y_hat = inv_logit(mu_hat);
    
  }
  
  
}
