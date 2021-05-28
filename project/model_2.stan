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
  
  vector[3] theta_att[T];
  vector[3] theta_def[T];
  
}

transformed parameters{
  real sigma_att[3] = inv(tau_att);
  real sigma_def[3] = inv(tau_def);
  
  vector[T] att_team_star;
  vector[T] def_team_star;
  vector[T] att_team;
  vector[T] def_team;
  
  for(t in 1:T){
    att_team_star[t] = pi_att[t,1]*(att[1]+sigma_att[1]*theta_att[t,1]) + pi_att[t,2]*(att[2]+sigma_att[2]*theta_att[t,2]) + pi_att[t,3]*(att[3]+ sigma_att[3]*theta_att[t,3]); 
    def_team_star[t] = pi_def[t,1]*(def[1]+sigma_def[1]*theta_def[t,1]) + pi_def[t,2]*(def[2]+sigma_def[2]*theta_def[t,2]) + pi_def[t,3]*(def[3]+ sigma_def[3]*theta_def[t,3]); 
  }
  
  for(t in 1:T){
    att_team[t] = att_team_star[t] - mean(att_team_star);
    def_team[t] = def_team_star[t] - mean(def_team_star);
  }
}


model {
  
  // Prior on home effect
  home ~ normal(0, 10000);
  
  //Priors on the random effects
  // group 1: bottom-table team
  tau_att[1] ~ gamma(0.01, 0.01);
  tau_def[1] ~ gamma(0.01, 0.01);
  att[1] ~ normal(0, 1000) T[-3, 0];
  def[1] ~ normal(0, 1000) T[0, 3];
  
  // group 2: mid-table teams
  tau_att[2] ~ gamma(0.01, 0.01);
  tau_def[2] ~ gamma(0.01, 0.01);
  att[2] ~ normal(0, sigma_att[2]);
  def[2] ~ normal(0, sigma_def[2]);
  
  // group 3: top-table teams
  tau_att[3] ~ gamma(0.01, 0.01);
  tau_def[3] ~ gamma(0.01, 0.01);
  att[3] ~ normal(0, 1000) T[0,3];
  def[3] ~ normal(0, 1000) T[-3,0];
  
  // Grouping effect
  // Prior on team cateogry probability
  for(t in 1:T){
    pi_att[t] ~ dirichlet(rep_vector(1, 3));
    pi_def[t] ~ dirichlet(rep_vector(1, 3));
  }
  
  for(t in 1:T){
    for(k in 1:3){
      theta_att[t,k] ~ normal(0, 1);
      theta_def[t,k] ~ normal(0, 1);  
    }
  }
  
  
  /*
  // Model probability for team category + model for team attack/defense effect
  for(t in 1:T){
  
    lp[1]= log(pi_att[t,1]) + normal_lpdf( att_team_star[t] | att[1], sigma_att[1]);
    lp[2]= log(pi_att[t,2]) + normal_lpdf( att_team_star[t] | att[2], sigma_att[2]);
    lp[3]= log(pi_att[t,3]) + normal_lpdf( att_team_star[t] | att[3], sigma_att[3]);
    target += log_sum_exp(lp);
    
    lp[1]= log(pi_def[t,1]) + normal_lpdf( def_team_star[t] | def[1], sigma_def[1]);
    lp[2]= log(pi_def[t,2]) + normal_lpdf( def_team_star[t] | def[2], sigma_def[2]);
    lp[3]= log(pi_def[t,3]) + normal_lpdf( def_team_star[t] | def[3], sigma_def[3]);
    target += log_sum_exp(lp);
  }
    */


  // Model probability for home and away goals 
  y_home ~ poisson_log(home + att_team[home_team_index] + def_team[away_team_index]);
  y_away ~ poisson_log(att_team[away_team_index] + def_team[home_team_index]);
  
}
/*
generated quantities {
  int<lower=0> y_home_rep[N];
  int<lower=0> y_away_rep[N];
  
  for (n in 1:N) {
    y_home_rep[n] = poisson_log_rng(home + att_team[home_team_index[n]] + def_team[away_team_index[n]]);
    y_away_rep[n] = poisson_log_rng(att_team[away_team_index[n]] + def_team[home_team_index[n]]);
  }
}
*/
