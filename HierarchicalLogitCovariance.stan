data {
  int<lower=0> N; // number of observations
  int<lower=1> L; // number of groups
  int<lower=1> K; // number of regressors (1 constant + 1 predictor)
  

  vector[K] X[N]; // model matrix
  vector[L] Y[N]; // dependent variable
}
parameters {
  vector[K] gamma; // population-level regression coefs
  vector<lower=0>[K] tau; // standard deviation of regression coefs
  
  matrix[L, K] beta; // matrix of group-level regression coefs
  
  cholesky_factor_corr[L] L_Omega;
  vector<lower=0>[L] L_sigma;
}
model {
  vector[L] mu[N]; // linear predictor
  matrix[L, L] L_Sigma;
  
    for(n in 1:N){
    mu[n] = beta * X[N];
  }


  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  
  to_vector(beta) ~ student_t(3, gamma, tau);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);
    

  // likelihood
  Y ~ multi_normal_cholesky(mu, L_Sigma);
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
