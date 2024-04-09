// Following advice from: https://mc-stan.org/docs/stan-users-guide/regression.html#seemingly-unrelated-regressions

data {
  int<lower=0> n;           // Number of data points
  int<lower=0> n_species;   // Number of species
  int<lower=0> species[n];  // Species assignment of each data point
  vector[2] measurement[n]; // width/length vector for each flower
}

parameters {

  vector[2] mu[n_species];
  cov_matrix[2] sigma[n_species];
  
}

model {

  for(i in 1:n){
    measurement[i] ~ multi_normal(____, ____);
  }

}
