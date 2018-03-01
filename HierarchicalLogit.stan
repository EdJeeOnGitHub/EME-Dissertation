data {
  int<lower=1> N; // Number of observations
  int L; // Number of events
  
  int<lower=1, upper=L> id[N]; // id for each?
  vector[N] returns; // predictors
  int Y[N]; // Response variable
  // vector[L] terror_return;
}
parameters{
  real mu_b;
  real<lower=0> sigma_b;
  real beta[L];
  
  real mu_a;
  real<lower=0> sigma_a;
  real a[L];
  
}
model {

  // priors
  
  mu_b ~ normal(0,1);
  sigma_b ~ normal(2.5, 5);
  
  mu_a ~ normal(0.5, 0.5);
  sigma_a ~ normal(1, 5);
  for (l in 1:L){
    beta[l] ~ student_t(3, mu_b, sigma_b);
    a[l] ~ normal(mu_a, sigma_a);
  }
  {
    vector[N] x_beta_ll;
    for (n in 1:N)
      x_beta_ll[n] = a[id[n]] + returns[n] * beta[id[n]];
      Y ~ bernoulli_logit(x_beta_ll);
}
  
}
generated quantities{
    vector[L] y_hat;

    vector[L] x_beta_ll_terror;
    for (l in 1:L)
      x_beta_ll_terror[l] = a[l] + terror_return[l] * beta[l];
      y_hat = inv_logit(x_beta_ll_terror);
}

