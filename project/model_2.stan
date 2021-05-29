data {
  int<lower=0> N; //number of games
  int<lower=0> T; //number of teams
  
  int<lower=0> y_home[N];
  int<lower=0> y_away[N];
  
  int<lower=1, upper=T> home_team_index[N];
  int<lower=1, upper=T> away_team_index[N];
   
}

parameters {
  real home;
  
  simplex[3] pi_att[T];
  simplex[3] pi_def[T];
  
  real att[3];
  real def[3];
  
  real<lower=0> tau_att[3];
  real<lower=0> tau_def[3];
  
  vector[T] att_team_raw; //https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
  vector[T] def_team_raw; // we need that sum(att_team) = sum(def_team) = 0
}

transformed parameters{
  
  real sigma_att[3];
  real sigma_def[3];
  
  vector[T] att_team;
  vector[T] def_team;
  
  for(t in 1:T){
    att_team[t] = att_team_raw[t]- mean(att_team_raw[]);
    def_team[t] = def_team_raw[t]- mean(def_team_raw[]);
  }
  
  for(k in 1:3){
    sigma_att[k] = 1/(tau_att[k]);
    sigma_def[k] = 1/(tau_def[k]);
  }
  
}


model {
  
  real l_theta_temp[2];
	real theta[2];
	vector[3] contributions_att;
	vector[3] contributions_def;
  
  //Priors on the random effects
  // group 1: bottom-table team
  tau_att[1] ~ gamma(0.01, 0.01);
  tau_def[1] ~ gamma(0.01, 0.01);
  att[1] ~ normal(-0.5, 0.25);
  def[1] ~ normal(0.5, 0.25);
  
  // group 2: mid-table teams
  tau_att[2] ~ gamma(0.01, 0.01);
  tau_def[2] ~ gamma(0.01, 0.01);
  att[2] ~ normal(0, 0.25);
  def[2] ~ normal(0, 0.25);
  
  // group 3: top-table teams
  tau_att[3] ~ gamma(0.01, 0.01);
  tau_def[3] ~ gamma(0.01, 0.01);
  att[3] ~ normal(0.5, 0.25);
  def[3] ~ normal(-0.5, 0.25);
  
  home ~ normal(0, 1000);
  
   for(t in 1:T){
    pi_att[t,] ~ dirichlet(rep_vector(1, 3));
    pi_def[t,] ~ dirichlet(rep_vector(1, 3));
    
    
    for(k in 1:3){
      contributions_att[k] = log(pi_att[t,k]) + student_t_lpdf(att_team_raw[t] | 4, att[k], sigma_att[k]);
      contributions_def[k] = log(pi_def[t,k]) + student_t_lpdf(def_team_raw[t] | 4, def[k], sigma_def[k]);
    }
    
    target += log_sum_exp(contributions_att);
    target += log_sum_exp(contributions_def);
    
  }
  
  for (n in 1:N){
    l_theta_temp[1] = home + att_team[home_team_index[n]] + def_team[away_team_index[n]];
    l_theta_temp[2] = att_team[away_team_index[n]] + def_team[home_team_index[n]];
    
		y_home[n] ~ poisson_log(l_theta_temp[1]);
		y_away[n] ~ poisson_log(l_theta_temp[2]);
    
  }
  //y_home ~ poisson_log(home + att_team[home_team_index] + def_team[away_team_index]);
  //y_away ~ poisson_log(att_team[away_team_index] + def_team[home_team_index]);
  
}
