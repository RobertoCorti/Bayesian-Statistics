data {
  int<lower=0> N; //number of games
  int<lower=0> T; //number of teams
  
  int<lower=0> y_home[N];
  int<lower=0> y_away[N];
  
  int<lower=1, upper=T> home_team_index[N];
  int<lower=1, upper=T> away_team_index[N];
 
  vector[3] alpha[T]; 
}

parameters {
  real home;
  
  simplex[3] pi_att[T];
  simplex[3] pi_def[T];
  
  real<lower=-3, upper=0> att_bottom;
  real att_medium;
  real<lower=0, upper=3> att_top;
  
  real<lower=0, upper=3> def_bottom;
  real def_medium;
  real<lower=-3, upper=0> def_top;
  
  
  real<lower=0> sigma_att[3];
  real<lower=0> sigma_def[3];
  
  vector[T] att_team_raw; 
  vector[T] def_team_raw; 
}

transformed parameters{
  
  vector[T] att_team;
  vector[T] def_team;
  
  for(t in 1:T){
    att_team[t] = att_team_raw[t]- mean(att_team_raw[]);
    def_team[t] = def_team_raw[t]- mean(def_team_raw[]);
  }
  
}


model {
  
  real l_theta_temp[2];
	real theta[2];
	vector[3] contributions_att;
	vector[3] contributions_def;
  
  //Priors on the random effects
  // group 1: bottom-table team
  sigma_att[1] ~ inv_gamma(1, 1);
  sigma_def[1] ~ inv_gamma(1, 1);
  att_bottom ~ normal(0, 0.5);
  def_bottom ~ normal(0, 0.5);
  
  
  // group 2: mid-table teams
  sigma_att[2] ~ inv_gamma(1, 1);
  sigma_def[2] ~ inv_gamma(1, 1);
  att_medium ~ normal(0, 0.25);
  def_medium ~ normal(0, 0.25);
  
  // group 3: top-table teams
  sigma_att[3] ~ inv_gamma(1, 1);
  sigma_def[3] ~ inv_gamma(1, 1);
  att_top ~ normal(0, 0.5);
  def_top ~ normal(0, 0.5);
  
  home ~ normal(0, 1000);
  
   for(t in 1:T){
    pi_att[t,] ~ dirichlet(alpha[t]);
    pi_def[t,] ~ dirichlet(alpha[t]);
    
    contributions_att[1] = log(pi_att[t,1]) + student_t_lpdf(att_team_raw[t] | 4, att_bottom, sigma_att[1]);
    contributions_def[1] = log(pi_def[t,1]) + student_t_lpdf(def_team_raw[t] | 4, def_bottom, sigma_def[1]);

    contributions_att[2] = log(pi_att[t,2]) + student_t_lpdf(att_team_raw[t] | 4, att_medium, sigma_att[2]);
    contributions_def[2] = log(pi_def[t,2]) + student_t_lpdf(def_team_raw[t] | 4, def_medium, sigma_def[2]);
    
    contributions_att[3] = log(pi_att[t,3]) + student_t_lpdf(att_team_raw[t] | 4, att_top, sigma_att[3]);
    contributions_def[3] = log(pi_def[t,3]) + student_t_lpdf(def_team_raw[t] | 4, def_top, sigma_def[3]);
    
    target += log_sum_exp(contributions_att);
    target += log_sum_exp(contributions_def);
    
  }
  
  for (n in 1:N){
    l_theta_temp[1] = home + att_team[home_team_index[n]] + def_team[away_team_index[n]];
    l_theta_temp[2] = att_team[away_team_index[n]] + def_team[home_team_index[n]];
    
		y_home[n] ~ poisson_log(l_theta_temp[1]);
		y_away[n] ~ poisson_log(l_theta_temp[2]);
    
  }
  
}

generated quantities {
  int<lower=0> y_home_rep[N];
  int<lower=0> y_away_rep[N];
  vector[N] log_lik_home;
  vector[N] log_lik_away;
  
  
  for (n in 1:N) {
    log_lik_home[n] = poisson_log_lpmf(y_home[n] | home + att_team[home_team_index[n]] + def_team[away_team_index[n]]);
    y_home_rep[n] = poisson_log_rng(home + att_team[home_team_index[n]] + def_team[away_team_index[n]]);
    
    log_lik_away[n] = poisson_log_lpmf(y_away[n] | att_team[away_team_index[n]] + def_team[home_team_index[n]]);
    y_away_rep[n] = poisson_log_rng(att_team[away_team_index[n]] + def_team[home_team_index[n]]);
  }
}
