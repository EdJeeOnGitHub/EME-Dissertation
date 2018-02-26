data {
  int<lower=1> N; // number of observations
  int<lower=1> L; // number of groups
  int<lower=1> K; // number of regressors (1 constant + 1 predictor)
  
  int<lower=1, upper=L> id[N]; // vector describing which group an observation is in
  
  matrix[N,K] X; // model matrix
  vector[N] Y; // dependent variable
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
  Y ~ bernoulli_logit(mu);
}
generated quantities {
  vector[L] y_hat;
  vector[L] x_beta_ll_terror;
  for(l in 1:L){
    x_beta_ll_terror[l] = terror_return[l]*beta[l];
    y_hat = inv_logit(x_beta_ll_terror);
  }
}
