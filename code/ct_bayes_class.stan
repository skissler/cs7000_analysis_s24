
data {

  int<lower=0> n; // Number of data points
  int<lower=0> n_inf; // Number of infections 
  real ydrop[n]; // the data itself 

  real t[n]; // time points at which each data point was collected
  real inf[n]; // The infection to which each data point belongs 

}


parameters {

  real peak_mu; 
  real peak_sigma; 

  real proliferation_mu;
  real proliferation_sigma; 

  real clearance_mu;
  real clearance_sigma; 

  real<lower=0> peak[n_inf];
  real<lower=0> proliferation[n_inf];
  real<lower=0> clearance[n_inf];

  real tmax[n_inf];
  real<lower=0> tmax_sigma;

  real sigma_measurement;

}


model {

  // Draw the temporal offset of each infection: 
  tmax ~ normal(0, tmax_sigma);

  // Draw individual-level parameters from population-level distributions: 
  peak ~ lognormal(peak_mu, peak_sigma);
  proliferation ~ lognormal(proliferation_mu, proliferation_sigma);
  clearance ~ lognormal(clearance_mu, clearance_sigma);

  // Loop over all the data: 
  for(i in 1:n){

    // STAGE 1: 
    if(t[i] < tmax[inf[i]] - proliferation[inf[i]]){

      ydrop[i] ~ normal(0, sigma_measurement);

      // STAGE 2: 
    } else if(t[i] < tmax[inf[i]]){

      ydrop[i] ~ normal(peak[inf[i]]/proliferation[inf[i]]*(t[i]-tmax[inf[i]]) + peak[inf[i]], sigma_measurement); 

      // STAGE 3:
    } else if(t[i] < tmax[inf[i]] + clearance[inf[i]]){

      ydrop[i] ~ normal(-peak[inf[i]]/clearance[inf[i]]*(t[i]-tmax[inf[i]]) + peak[inf[i]], sigma_measurement); 

      // STAGE 4: 
    } else {  

      ydrop[i] ~ normal(0, sigma_measurement);

    }




  }




}














