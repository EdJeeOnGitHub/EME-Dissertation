data {
  int<lower=1> N; // Number of observations
  int<lower=1> E; // Number of events
  int<lower=1> K; // Number of predictors
  
  
  int id[N]; // vector of group indices
  vector[N] x; // predictors
  int Y[N]; // Response variable
}
parameters{
  real a[E];
  vector[K] beta;
  real<lower=0, upper=10> sigma_a;
  real mu_a;
  
  real mu_b;
  real<lower=0, upper=10> sigma_b;
}
model {
  // priors
  a ~ normal(mu_a, sigma_a);
  beta ~ normal(mu_b, sigma_b);

  for (n in 1:N){
    Y[n] ~ bernoulli_logit(a[id[n]] + x[n]*beta); 
  }
  
}

