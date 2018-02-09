data {
  int<lower=1> N; // Number of observations
  int L; // Number of events
  
  int<lower=1, upper=L> ll[N]; // id for each?
  vector[N] returns; // predictors
  int Y[N]; // Response variable
  vector[N] terror_return;
}
parameters{
  real mu_b;
  real<lower=0> sigma_b;
  real beta[L];
  
  real mu_a;
  real sigma_a;
  real a[L];
  
}
model {

  // priors
  
  mu_b ~ normal(0,1);
  mu_a ~ normal(0.5, 0.5);
  for (l in 1:L){
    beta[l] ~ student_t(3, mu_b, sigma_b);
    a[l] ~ normal(mu_a, sigma_a);
  }
  {
    vector[N] x_beta_ll;
    for (n in 1:N)
      x_beta_ll[n] = a[ll[n]] + returns[n] * beta[ll[n]];
      Y ~ bernoulli_logit(x_beta_ll);
}
  
}
generated quantities{
    vector[N] y_hat;
    
    vector[N] x_beta_ll_terror;
    for (n in 1:N)
      x_beta_ll_terror[n] = a[ll[n]] + terror_return[n] * beta[ll[n]];
      y_hat = inv_logit(x_beta_ll_terror);
}

