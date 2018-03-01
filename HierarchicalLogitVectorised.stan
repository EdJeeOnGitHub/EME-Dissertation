data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of groups
  int<lower=1> K; // number of regressors (1 constant + 1 predictor)

  
  int<lower=1, upper = L> id[N];
  matrix[N, K] X; // model matrix
  int Y[N]; // dependent variable
  // vector[L] terror_return;
}
parameters {
  vector[K] gamma; // population-level regression coefs
  vector<lower=0>[K] tau; // standard deviation of regression coefs
  
  vector[K] beta[L]; // matrix of group-level regression coefs


}
model {
  vector[N] mu; // linear predictor
  
  gamma ~ normal(0,5);
  tau ~ normal(0,5);
  
  for(l in 1:L){
    beta[l] ~ student_t(3, gamma, tau);
  }
  
  for(n in 1:N){
    mu[n] = X[n]*beta[id[n]];
  }
  
  // likelihood
  Y ~ bernoulli_logit(mu);

}
generated quantities {
  vector[L] y_hat;
  vector[L] mu_hat;

  for(l in 1:L){
    mu_hat[l] = X[l] * beta[l];
    y_hat = inv_logit(mu_hat);

  }


}
