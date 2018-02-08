data {
  int<lower = 0> E; // Event E
  int<lower = 0, upper = 1> Y[E]; // Binary dependent variable Y in event E
  vector[N] returns; // Independent variable, returns
  real terror_return;
}
parameters {
  real alpha; // intercept
  real b_returns; // beta for returns
}
model {
  alpha ~ normal(0.5, 0.5); // Intercept with weak prior
  b_returns ~ student_t(1, 0, 2.5); // Another weak prior
  Y ~ bernoulli_logit(alpha + b_returns * returns); // The model
}
generated quantities {
  real y_hat;
  y_hat = inv_logit(alpha + b_returns*terror_return);
}
