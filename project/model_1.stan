//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //number of games
  int<lower=0> T; //number of teams
  
  int<lower=0> y_home[N];
  int<lower=0> y_away[N];
  
  int<lower=1, upper=T> home_team_index[N];
  int<lower=1, upper=T> away_team_index[N];
   
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[1] home;
  vector[T] att_team;
  vector[T] def_team;
  real att;
  real def;
  real<lower=0> tau_att;
  real<lower=0> tau_def;
}

transformed parameters{
  real sigma_att = inv(tau_att);
  real sigma_def = inv(tau_def);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  tau_att ~ gamma(0.1, 0.1);
  tau_def ~ gamma(0.1, 0.1);
  
  att ~ normal(0, 10000);
  def ~ normal(0, 10000);
  
  home ~ normal(0, 10000);
  
  for (n in 1:T){
    att_team[n] ~ normal(att, sigma_att);
    def_team[n] ~ normal(def, sigma_def);
  }
  /*
  for (n in 1:N){
    y_home[n] ~ poisson_log(home + att_team[home_team_index[n]] + def_team[away_team_index[n]]);
    y_away[n] ~ poisson_log(att_team[away_team_index[n]] + def_team[home_team_index[n]]);
  }*/
  
  y_home ~ poisson_log(home + att_team[home_team_index] + def_team[away_team_index]);
  y_away ~ poisson_log(att_team[away_team_index] + def_team[home_team_index]);
  
}
 
