
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
  vector[n_seeds] b0;    // Tells Stan we want to estimate b0 (intercept)
  vector[n_seeds] b1;    // Tells Stan we want to estimate b1 (slope)
  vector[n_seeds] b2;    // Tells Stan we want to estimate b2 (quadratic coef)
  real<lower=0> sigma;   // Tells Stan we want to estimate sigma (error)
}

model {

  for (i in 1:n) {
    height[i] ~ normal(b0[seedindex[i]] + age[i]*b1[seedindex[i]] + age2[i]*b2[seedindex[i]], sigma);   
  }
  
}
