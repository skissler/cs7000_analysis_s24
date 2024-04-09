data {
  ____ n;          // Number of data points
  ____ n_species;  // Number of species
  ____ species[____]; // Species assignment of each data point
  ____ petalwidth;   // Petal length for each data point 
}

parameters {

  ____ width_mean;
  ____ width_sd;

}

model {

  for(i in 1:n){
    petalwidth[i] ~ normal(____, ____);
  }

}
