data {

  int<lower=0> n;       // Number of data points
  int<lower=0> inf[n];  // Infection assignment for each data point 
  int<lower=0> n_inf;   // Number of infections in the dataset
  real t[n];            // Time value for each data point (with 0 corresponding to the time of the max measured viral load for a given infection)
  real ydiff[n];        // The data: Change in Ct relative to limit of detection

}

parameters {

  real<lower=0> peak_mu; 
  real<lower=0> proliferation_mu; 
  real<lower=0> clearance_mu;

  real<lower=0> peak_sigma;
  real<lower=0> proliferation_sigma;
  real<lower=0> clearance_sigma;

  real<lower=0> peak[n_inf];
  real<lower=0> proliferation[n_inf];
  real<lower=0> clearance[n_inf];

  real<lower=0> tmax_sd;
  real tmax[n_inf];

  real<lower=0> measurement_sigma; 

}

model {

  // Sample the peak time for each infection 
  tmax ~ normal(0, tmax_sd);  

  // Sample peak Ct, proliferation time, and clearance time for each person
  peak ~ lognormal(peak_mu, peak_sigma);
  proliferation ~ lognormal(proliferation_mu, proliferation_sigma);
  clearance ~ lognormal(clearance_mu, clearance_sigma);

  // Calculate the likelihood
  for(i in 1:n){

    if(t[i] < (-proliferation[inf[i]] + tmax[inf[i]])){

      ydiff[i] ~ normal(0, measurement_sigma);

    } else if(t[i] < (tmax[inf[i]])) { 

      ydiff[i] ~ normal(
        (peak[inf[i]] / proliferation[inf[i]])*(t[i] - tmax[inf[i]]) + peak[inf[i]], 
        measurement_sigma);

    } else if(t[i] < (clearance[inf[i]]+tmax[inf[i]])){

      ydiff[i] ~ normal(
        -(peak[inf[i]] / clearance[inf[i]])*(t[i] - tmax[inf[i]]) + peak[inf[i]]
        , measurement_sigma);

    } else {

      ydiff[i] ~ normal(0, measurement_sigma);

    }    
  }
}
