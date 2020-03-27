data {
  // Dimensions
  int<lower=0> N; // number of rows
  int<lower=0> K; // number of predictors
  int<lower=0> J; // number of levels in random effect
  
  // Variables
  matrix[N,K] x;  // matrix of predictors
  vector[N] y;    // vector of response variable
  int farm[N];    // random Effect
}
transformed data{
  matrix[N, K] Q_ast;
  matrix[K, K] R_ast;
  matrix[K, K] R_ast_inverse;
  // thin and scale the QR decomposition
  Q_ast = qr_thin_Q(x) * sqrt(N - 1);
  R_ast = qr_thin_R(x) / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
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
      y[n] ~ lognormal(Q_ast[n,1]*betaTrt + Q_ast[n,2]*betaYrs + Q_ast[n,3]*betaYrsInv + Q_ast[n,4]*betaTrtYrs + Q_ast[n,5]*betaSoy + Q_ast[n,6]*betaClay + Q_ast[n,7]*betaSilt + alpha[farm[n]], sigma);
}
generated quantities {
  vector[N] y_tilde; // Vector for predicted data

  // Predict data from model parameters
  for(n in 1:N)
    y_tilde[n] = lognormal_rng(Q_ast[n,1]*betaTrt + Q_ast[n,2]*betaYrs + Q_ast[n,3]*betaYrsInv + Q_ast[n,4]*betaTrtYrs + Q_ast[n,5]*betaSoy + Q_ast[n,6]*betaClay + Q_ast[n,7]*betaSilt + alpha[farm[n]], sigma);
}
