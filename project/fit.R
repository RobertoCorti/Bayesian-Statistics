library(rstan)
library(bayesplot)
options(mc.cores=4)
setwd("../Desktop/Università/DSSC/Secondo_Anno/Bayesian_Statistics/Bayesian-Statistics/project/")

## fit 1991/92

# prepare data
df <- readRDS("./data/season1991.rds")

stan_dat_model_1 <- list(
                        N = nrow(df), 
                        T = length(unique(df$home)),
                        y_home = df$hgoal,
                        y_away = df$vgoal,
                        home_team_index = df$home_id,
                        away_team_index = df$visitor_id
                      )
str(stan_dat_model_1)

# compile
comp_model_1 <- stan_model('model_1.stan')

# fit
fit_model_1 <- sampling(comp_model_1, data = stan_dat_model_1)

saveRDS(fit_model_1, "models/model_1_fit_1991.rds")


y_home_rep <- as.matrix(fit_model_1, pars = "y_home_rep")
saveRDS(y_home_rep, "simulations/y_home_rep_model_1_1991.rds")

y_away_rep <- as.matrix(fit_model_1, pars= "y_away_rep")
saveRDS(y_away_rep, "simulations/y_away_rep_model_1_1991.rds")

remove(fit_model_1)
remove(y_home_rep)
remove(y_away_rep)

alpha <- readRDS("functions/dirichlet_hyper1991.rds")

stan_dat_model_2 <- list(
  N = nrow(df), 
  T = length(unique(df$home)),
  y_home = df$hgoal,
  y_away = df$vgoal,
  home_team_index = df$home_id,
  away_team_index = df$visitor_id,
  alpha = alpha
)
str(stan_dat_model_2)

# compile
comp_model_2 <- stan_model('model_2.stan')
# fit
fit_model_2 <- sampling(comp_model_2, data = stan_dat_model_2, iter=5000)
saveRDS(fit_model_2, "models/model_2_fit_1991.rds")

y_home_rep <- as.matrix(fit_model_2, pars = "y_home_rep")
saveRDS(y_home_rep, "simulations/y_home_rep_model_2_1991.rds")

y_away_rep <- as.matrix(fit_model_2, pars= "y_away_rep")
saveRDS(y_away_rep, "simulations/y_away_rep_model_2_1991.rds")


## fit 2007/08

# prepare data
df <- readRDS("./data/season2007.rds")

stan_dat_model_1 <- list(
  N = nrow(df), 
  T = length(unique(df$home)),
  y_home = df$hgoal,
  y_away = df$vgoal,
  home_team_index = df$home_id,
  away_team_index = df$visitor_id
)
str(stan_dat_model_1)

# compile
comp_model_1 <- stan_model('model_1.stan')

# fit
fit_model_1 <- sampling(comp_model_1, data = stan_dat_model_1, iter=7000)
saveRDS(fit_model_1, "models/model_1_fit_2007.rds")

y_home_rep <- as.matrix(fit_model_1, pars = "y_home_rep")
saveRDS(y_home_rep, "simulations/y_home_rep_model_1_2007.rds")

y_away_rep <- as.matrix(fit_model_1, pars= "y_away_rep")
saveRDS(y_away_rep, "simulations/y_away_rep_model_1_2007.rds")

remove(fit_model_1)
remove(y_home_rep)
remove(y_away_rep)

alpha <- readRDS("functions/dirichlet_hyper2007.rds")

stan_dat_model_2 <- list(
  N = nrow(df), 
  T = length(unique(df$home)),
  y_home = df$hgoal,
  y_away = df$vgoal,
  home_team_index = df$home_id,
  away_team_index = df$visitor_id,
  alpha = alpha
)
str(stan_dat_model_2)

# compile
comp_model_2 <- stan_model('model_2.stan')
# fit
fit_model_2 <- sampling(comp_model_2, data = stan_dat_model_2, chain=4, iter=8000)
saveRDS(fit_model_2, "models/model_2_fit_2007.rds")

y_home_rep <- as.matrix(fit_model_2, pars = "y_home_rep")
saveRDS(y_home_rep, "simulations/y_home_rep_model_2_2007.rds")

y_away_rep <- as.matrix(fit_model_2, pars= "y_away_rep")
saveRDS(y_away_rep, "simulations/y_away_rep_model_2_2007.rds")


