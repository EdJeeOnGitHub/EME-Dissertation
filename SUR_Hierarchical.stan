data {
  int<lower=1> N; // Number of observations 
  int<lower=0> L; // Number of events and also number of SUR equations
  vector[L] Y[N]; // Y variable, replicated L = 20 times
  real X[N]; // predictor variable
  int<lower=1, upper=L> id[N]; // id describing which event X and Y correspond to
 
}
parameters{
  vector[L] a[L]; // Intercept term
  vector[L] b[L]; // coefficient on X
  
  vector[L] mu_a; // hierarchical priors for the intercept
  vector<lower=0>[L] sigma_a;
  
  vector[L] mu_b; // hierarchical priors for the predictor
  vector<lower=0>[L] sigma_b;
  
  cholesky_factor_corr[L] L_Omega; // Covariance matrix for the events
  vector<lower=0>[L] L_sigma;
  
}
model {
  vector[L] y_hat[N];
  matrix[L, L] L_Sigma;
  
  mu_a ~ normal(0,5);
  mu_b ~ normal(0,5);
  
  sigma_a ~ normal(0,5);
  sigma_b ~ normal(0,5);
  
  for (l in 1:L){
  a[l] ~ normal(mu_a, sigma_a);
  b[l] ~ normal(mu_b, sigma_b);
  }
   
  
  for (i in 1:N){
    y_hat[i] = a[id[i]] + b[id[i]] * X[i];
  }
  
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);
  
  Y ~ multi_normal_cholesky(y_hat, L_Sigma);
}


