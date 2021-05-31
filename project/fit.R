library(rstan)
library(bayesplot)

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

saveRDS(fit_model_1, "models/model_1_fit.rds")


y_home_rep <- as.matrix(fit_model_1, pars = "y_home_rep")
saveRDS(y_home_rep, "y_home_rep.rds")

y_away_rep <- as.matrix(fit_model_1, pars= "y_away_rep")
saveRDS(y_away_rep, "y_away_rep.rds")


# compile
comp_model_2 <- stan_model('model_2.stan')
# fit
fit_model_2 <- sampling(comp_model_2, data = stan_dat_model_1, chain=4, iter=5000, warmup=2500)
saveRDS(fit_model_2, "models/model_2_fit.rds")

y_home_rep <- as.matrix(fit_model_2, pars = "y_home_rep")
saveRDS(y_home_rep, "y_home_rep_2.rds")

y_away_rep <- as.matrix(fit_model_2, pars= "y_away_rep")
saveRDS(y_away_rep, "y_away_rep_2.rds")


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
fit_model_1 <- sampling(comp_model_1, data = stan_dat_model_1)

saveRDS(fit_model_1, "models/model_1_fit_2007.rds")

y_home_rep <- as.matrix(fit_model_1, pars = "y_home_rep")
saveRDS(y_home_rep, "y_home_rep_2007.rds")

y_away_rep <- as.matrix(fit_model_1, pars= "y_away_rep")
saveRDS(y_away_rep, "y_away_rep_2007.rds")


# compile
comp_model_2 <- stan_model('model_2.stan')
# fit
fit_model_2 <- sampling(comp_model_2, data = stan_dat_model_1, chain=4, iter=5000, warmup=2500)
saveRDS(fit_model_2, "models/model_2_fit.rds")

y_home_rep <- as.matrix(fit_model_2, pars = "y_home_rep")
saveRDS(y_home_rep, "y_home_rep_2_2007.rds")

y_away_rep <- as.matrix(fit_model_2, pars= "y_away_rep")
saveRDS(y_away_rep, "y_away_rep_2.rds")


