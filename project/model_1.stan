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
  real att;
  real def;
  
  vector[T-1] att_team_raw; //https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
  vector[T-1] def_team_raw; // we need that sum(att_team) = sum(def_team) = 0
  
  real<lower=0> tau_att;
  real<lower=0> tau_def;
}

transformed parameters{
  real sigma_att = inv(tau_att);
  real sigma_def = inv(tau_def);
  
  vector[T] att_team = append_row(att_team_raw, -sum(att_team_raw));
  vector[T] def_team = append_row(def_team_raw, -sum(att_team_raw));
}


model {
  tau_att ~ gamma(0.1, 0.1);
  tau_def ~ gamma(0.1, 0.1);
  
  att ~ normal(0, 10000);
  def ~ normal(0, 10000);
  
  home ~ normal(0, 10000);
  
  for (n in 1:T-1){
    att_team_raw[n] ~ normal(att, sigma_att);
    def_team_raw[n] ~ normal(def, sigma_def);
  }

  y_home ~ poisson_log(home + att_team[home_team_index] + def_team[away_team_index]);
  y_away ~ poisson_log(att_team[away_team_index] + def_team[home_team_index]);
  
}

generated quantities {
  int<lower=0> y_home_rep[N];
  int<lower=0> y_away_rep[N];
  
  for (n in 1:N) {
    //real theta_home_n = home + att_team[home_team_index[n]] + def_team[away_team_index[n]];
    y_home_rep[n] = poisson_log_rng(home + att_team[home_team_index[n]] + def_team[away_team_index[n]]);
    
    //real theta_away_n = att_team[away_team_index[n]] + def_team[home_team_index[n]];
    y_away_rep[n] = poisson_log_rng(att_team[away_team_index[n]] + def_team[home_team_index[n]]);
  }
}

