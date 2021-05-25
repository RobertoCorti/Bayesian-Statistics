library(rstan)
library(bayesplot)

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

y_away_rep <- as.matrix(fit_model_1, pars = "y_away_rep")
saveRDS(y_away_rep, "y_away_rep.rds")



