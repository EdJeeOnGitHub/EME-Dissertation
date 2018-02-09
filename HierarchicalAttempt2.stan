data {
  int<lower=1> N; // Number of observations
  int Event[N]; // Number of events
  
  vector[N] returns; // predictors
  int Y[N]; // Response variable
  vector[N] terror_return;
}
parameters{
  vector[5] a;
  vector[5] b;
  real beta;
  real mu_a;
  real mu_b;

  
  real<lower=0, upper=10> sigma_a;
  real<lower=0, upper=10> sigma_b;

  
}
model {
  vector[N] y;
  // priors
  
  mu_a ~ normal(0,1);
  a ~ normal(mu_a, sigma_a);
  
  mu_b ~ normal(0,1);
  b ~ student_t(3, mu_b, sigma_b);

  
  for (i in 1:N){
    y[i] = a[Event[i]] + returns[i]*b[Event[i]];
  }
  Y ~ bernoulli_logit(y);
  
}
generated quantities{

  real y_hat;

  
  y_hat = inv_logit(a[Event[1]] + returns[1]*b[Event[1]]);
}

