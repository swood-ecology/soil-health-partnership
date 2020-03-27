data {
  // Dimensions
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> J;
  
  // Variables
  vector[N] trt;
  vector[N] yrs;
  vector[N] yrsInv;
  vector[N] trtYrs;
  vector[N] soy;
  vector[N] clay;
  vector[N] silt;
  vector[N] y;
  
  // Random Effect
  int farm[N];
}
parameters  {
  // Define parameters
  vector[J] alpha;
  real betaTrt;
  real betaYrs;
  real betaYrsInv;
  real betaTrtYrs;
  real betaSoy;
  real betaClay;
  real betaSilt;
  real mu_a;
  real<lower=0> sigma;
  real<lower=0> sigma_a;
}
model {
  // Uninformative priors
  betaTrt ~ normal(0,10);
  betaYrs ~ normal(0,10);
  betaYrsInv ~ normal(0,10);
  betaTrtYrs ~ normal(0,10);
  betaSoy ~ normal(0,10);
  betaClay ~ normal(0.00133,0.0005);
  betaSilt ~ normal(0,10);
  
  mu_a ~ normal(0,10);
  
  sigma ~ cauchy(0,5);
  sigma_a ~ cauchy(0,5);

  alpha ~ normal(mu_a, sigma_a);

  // Run model
  for(n in 1:N)
    y[n] ~ lognormal(trt[n]*betaTrt + yrs[n]*betaYrs + yrsInv[n]*betaYrsInv + trtYrs[n]*betaTrtYrs + soy[n]*betaSoy + clay[n]*betaClay + silt[n]*betaSilt + alpha[farm[n]], sigma);
}
generated quantities {
  vector[N] y_tilde; // Vector for predicted data

  // Predict data from model parameters
  for(n in 1:N)
    y_tilde[n] = lognormal_rng(trt[n]*betaTrt + yrs[n]*betaYrs + yrsInv[n]*betaYrsInv + trtYrs[n]*betaTrtYrs + soy[n]*betaSoy + clay[n]*betaClay + silt[n]*betaSilt + alpha[farm[n]], sigma);
}
