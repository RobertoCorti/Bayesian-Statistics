functions {

  int neg_binomial_2_log_safe_rng(real eta, real phi) {
    real gamma_rate = gamma_rng(phi, phi / exp(eta));
    if (gamma_rate >= exp(20.79))
      return -9;
      
    return poisson_rng(gamma_rate);
  }
}
data {
  int<lower=1> N;                     
  int<lower=0> complaints[N];              
  vector<lower=0>[N] traps;                
  
  // 'exposure'
  vector[N] log_sq_foot;  
  
  // building-level data
  int<lower=1> K;
  int<lower=1> J;
  int<lower=1, upper=J> building_idx[N];
  matrix[J,K] building_data;
}
parameters {
  real<lower=0> inv_phi;   
  real beta;               
  real alpha;              
  vector[K] zeta;         
  vector[K] gamma;
  vector[J] mu_raw;        
  real<lower=0> sigma_mu;  
  vector[J] kappa_raw;        
  real<lower=0> sigma_kappa;  
}
transformed parameters {
  real phi = inv(inv_phi);
  
  // non-centered parameterization
  vector[J] mu = alpha + building_data * zeta + sigma_mu * mu_raw;
  vector[J] kappa = beta + building_data * gamma + sigma_kappa * kappa_raw;
}
model {
  mu_raw ~ normal(0, 1);   
  sigma_mu ~ normal(0, 1);
  kappa_raw ~ normal(0, 1);   
  sigma_kappa ~ normal(0, 1);
  
  alpha ~ normal(log(4), 1);
  zeta ~ normal(0, 1);
  beta ~ normal(-0.5, 1);
  inv_phi ~ normal(0, 1);
  
 complaints ~ neg_binomial_2_log(mu[building_idx] + kappa[building_idx] .* traps
                                  + log_sq_foot, phi);
} 
generated quantities {
  int y_rep[N];
  for (n in 1:N) {
    real eta_n = mu[building_idx[n]] + kappa[building_idx[n]] * traps[n] + log_sq_foot[n];
    y_rep[n] = neg_binomial_2_log_safe_rng(eta_n, phi);
  }
}
