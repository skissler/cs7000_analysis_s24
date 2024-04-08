
data {
  int<lower=0> n;     // Number of data points (input from R)
  vector[n] age;      // Vector of tree ages (input from R)
  vector[n] height;   // Vector of tree heights (input from R) 
}

transformed data {
  vector[n] age2; 
  age2 = age .* age;
}

parameters {
  real b0;             // Tells Stan we want to estimate b0 (intercept)
  real b1;             // Tells Stan we want to estimate b1 (slope)
  real b2;             // Tells Stan we want to estimate b2 (quadratic coef)
  real<lower=0> sigma; // Tells Stan we want to estimate sigma (error)
}

model {
  height ~ normal(b0 + age*b1 + age2*b2, sigma); // The statistical model 
}





// model {

//   for (i in 1:n){
//     height[i] ~ normal(b0 + age[i]*b1 + age2[i]*b2, sigma);
//   }

// }
