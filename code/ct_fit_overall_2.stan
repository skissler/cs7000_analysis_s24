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

  real<lower=0> tmax_sd;
  real tmax[n_inf];

  real<lower=0> peak_jitter[n_inf];
  real<lower=0> proliferation_jitter[n_inf];
  real<lower=0> clearance_jitter[n_inf];

  real<lower=0> measurement_sigma; 

}

transformed parameters {

  real<lower=0> peak[n_inf];
  real<lower=0> proliferation[n_inf];
  real<lower=0> clearance[n_inf];

  for(k in 1:n_inf){
    peak[k] = exp(peak_mu + peak_sigma*peak_jitter[k]);
    proliferation[k] = exp(proliferation_mu + proliferation_sigma*proliferation_jitter[k]);
    clearance[k] = exp(clearance_mu + clearance_sigma*clearance_jitter[k]);  
  }
  
}


model {

  // Sample the peak time for each infection 
  tmax ~ normal(0, tmax_sd);  

  // Sample peak Ct, proliferation time, and clearance time for each person
  peak_jitter ~ normal(0, 1);
  proliferation_jitter ~ normal(0, 1);
  clearance_jitter ~ normal(0, 1);

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
