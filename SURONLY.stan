data {
  int<lower=1> N; // Number of observations per event
  int<lower=1> J; // Number of covariates (1 for now)
  int<lower=1> K // Number of Events
  vector[J] x[N];
  vector[K] y[N];
}
parameters {
   matrix[K, J] beta;
   cov_matrix[K] Sigma;
}
model {
  vector[K] mu[N];
  
  for (n in 1:N){
    mu[n] = beta * x[n];
  }
  y ~ multi_normal(mu, Sigma);
}
