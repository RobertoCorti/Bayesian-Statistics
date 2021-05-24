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

saveRDS(fit_model_1, "model_1_fit.rds")

fit_model_1 <- readRDS("model_1_fit.rds")

mcmc_hist(as.matrix(fit_model_1, pars = c('att_team')))
mcmc_hist(as.matrix(fit_model_1, pars = c('def_team')))
mcmc_scatter(as.matrix(fit_P_real_data, pars = c('alpha','beta')), alpha = 0.2)
