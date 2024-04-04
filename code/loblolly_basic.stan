data {
  int<lower=0> n;     // Number of data points (input from R)
  vector[n] age;      // Vector of tree ages (input from R)
  vector[n] height;   // Vector of tree heights (input from R) 
}

parameters {
  real b0;             // Tells Stan we want to estimate b0 (intercept)
  real b1;             // Tells Stan we want to estimate b1 (slope)
  real<lower=0> sigma; // Tells Stan we want to estimate sigma (error)
}

model {
  height ~ normal(b0 + age*b1, sigma); // The statistical model 
}
