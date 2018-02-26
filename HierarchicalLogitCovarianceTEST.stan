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
  
  cholesky_factor_corr[L] L_Omega;
  vector<lower=0>[L] L_sigma;
   
}
model {
  vector[N] mu; // linear predictor
  matrix[L, L] L_Sigma;
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);
  
  
  // priors
  gamma ~ normal(0,5); // weakly informative regression coefficient priors
  tau ~ normal(0, 5); // more weakly informative priors
  
  

  
  for(l in 1:L){
    beta[l] ~ student_t(3, gamma, tau); // filling group level coefs
  }
  
  for(n in 1:N){
    mu[n] = X[n] * beta[id[n]];
  }
  
  // likelihood
  
  Y ~  multi_normal_cholesky(mu, L_Sigma);
  
}

// generated quantities {
//   vector[L] y_hat;
//   vector[L] mu_hat;
//   
//   for(l in 1:L){
//     mu_hat[l] = X[l] * beta[l];
//     y_hat = inv_logit(mu_hat);
//     
//   }
  
  
// }
