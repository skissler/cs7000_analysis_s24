data {
  int<lower=0> n;          // Number of data points
  int<lower=0> n_species;  // Number of species
  int<lower=0> species[n]; // Species assignment of each data point
  vector[n] petallength;   // Petal length for each data point 
}

parameters {

  vector[n_species] length_mean;
  vector<lower=0>[n_species] length_sd;

}

model {

  for(i in 1:n){
    petallength[i] ~ normal(length_mean[species[i]], length_sd[species[i]]);
  }

}