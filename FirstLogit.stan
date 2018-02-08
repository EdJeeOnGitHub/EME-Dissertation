data {
  int<lower = 0> E; // Event E
  int<lower = 0, upper = 1> Y[E]; // Binary dependent variable Y in event E
  vector[N] returns; // Independent variable, returns
}
parameters {
  real alpha; // intercept
  real b_returns; // beta for returns
}
model {
  alpha ~ normal(0, 100); // Intercept with weak prior
  b_returns ~ normal(0, 100); // Another weak prior
  Y ~ bernoulli_logit(alpha + b_returns * returns); // The model
}
generated quantities {
  real y_hat;
  y_hat = inv_logit(alpha + b_returns);
}
