
data {
  int<lower=0> n;       // Number of data points (input from R)
  int<lower=0> n_seeds; // Number of seeds
  int seedindex[n];  // Which seed does each observation belong to? 
  vector[n] age;        // Vector of tree ages (input from R)
  vector[n] height;     // Vector of tree heights (input from R) 
}

transformed data {
  vector[n] age2; 
  age2 = age .* age;
}

parameters {

  vector[n_seeds] eta0;
  vector[n_seeds] eta1;
  vector[n_seeds] eta2;

  real b0_overall;
  real b1_overall;
  real b2_overall;

  real<lower=0> sigma0_overall;
  real<lower=0> sigma1_overall;
  real<lower=0> sigma2_overall;

  real<lower=0> sigma;   // Tells Stan we want to estimate sigma (error)
}

transformed parameters {

  vector[n_seeds] b0;    // Tells Stan we want to estimate b0 (intercept)
  vector[n_seeds] b1;    // Tells Stan we want to estimate b1 (slope)
  vector[n_seeds] b2;    // Tells Stan we want to estimate b2 (quadratic coef)

  b0 = b0_overall + sigma0_overall*eta0;
  b1 = b1_overall + sigma1_overall*eta1;
  b2 = b2_overall + sigma2_overall*eta2;

}

model {

  eta0 ~ normal(0,1);
  eta1 ~ normal(0,1);
  eta2 ~ normal(0,1);

  for (i in 1:n) {
    height[i] ~ normal(b0[seedindex[i]] + age[i]*b1[seedindex[i]] + age2[i]*b2[seedindex[i]], sigma);   
  }
  
}
