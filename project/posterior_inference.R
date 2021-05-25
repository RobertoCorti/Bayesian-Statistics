fit_model_1 <- readRDS("models/model_1_fit.rds")

print(fit_model_1, pars = c('att_team','def_team','home'))


y_home_rep <- readRDS("y_home_rep.rds")
y_away_rep <- readRDS("y_away_rep.rds")