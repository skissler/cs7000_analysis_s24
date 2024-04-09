data {
  int<lower=0> n;          // Number of data points
  int<lower=0> n_species;  // Number of species
  int<lower=0> species[n]; // Species assignment of each data point
  vector[n] petalwidth;   // Petal length for each data point 
}

parameters {

  vector[n_species] width_mean;
  vector<lower=0>[n_species] width_sd;

}

model {

  for(i in 1:n){
    petalwidth[i] ~ normal(width_mean[species[i]], width_sd[species[i]]);
  }

}
