library(dplyr)
library(ggplot2)
library(bayesplot)
setwd("../Desktop/Università/DSSC/Secondo_Anno/Bayesian_Statistics/Bayesian-Statistics/project/")

## functions
simulate_data <- function(df, y_home_rep, y_away_rep){
  df_simulate <-  data.frame(Date = df$Date,
                             Season = df$Season,
                             home = df$home,
                             visitor = df$visitor,
                             hgoal= apply(as.data.frame(y_home_rep), 2, median),
                             vgoal= apply(as.data.frame(y_away_rep), 2, median), 
                             home_id = df$home_id,
                             visitor_id = df$visitor_id)
  return(df_simulate)
}


ranking <- readRDS("functions/ranking.rds")

cum_points <- readRDS("functions/cum_points.rds")


#### SEASON 1991-92

df <- readRDS("./data/season1991.rds")
df_ranking <- readRDS("./data/season1991_ranking.rds")

### Model 1
fit_model_1 <- readRDS("models/model_1_fit_1991.rds")
y_home_rep <- readRDS("simulations/y_home_rep_model_1_1991.rds")
y_away_rep <- readRDS("simulations/y_away_rep_model_1_1991.rds")
loglik1_home <- as.matrix(fit_model_1, pars="log_lik_home")
loglik1_away <- as.matrix(fit_model_1, pars="log_lik_away")

df_sim1 <- simulate_data(df, y_home_rep = y_home_rep, y_away_rep = y_away_rep)
df_ranking_sim1 <- ranking(df_sim1, point_per_win = 2)
cum_points_list_sim1 <- cum_points(df_sim1)
cum_points_list <- readRDS("data/season1991_cum_points.rds")





bayesplot::ppc_bars(df$hgoal, y_home_rep[2000:4000,]) + ggtitle("Model 1 posterior predictive histogram of home goals", subtitle = "Serie A 1991-92")
bayesplot::ppc_bars(df$vgoal, y_away_rep[2000:4000,]) + ggtitle("Model 1 posterior predictive histogram of away goals", subtitle = "Serie A 1991-92")

bayesplot::ppc_bars_grouped(df$hgoal, y_home_rep[2000:4000,], group = df$home)  + ggtitle("Model 1 Posterior predictive histogram of home goals per team", subtitle = "Serie A 1991-92")
bayesplot::ppc_bars_grouped(df$vgoal, y_away_rep[2000:4000,], group = df$visitor)  + ggtitle("Model 1 Posterior predictive histogram of away goals per team", subtitle = "Serie A 1991-92")

bayesplot::mcmc_intervals(as.matrix(fit_model_1),
                          regex_pars = c("att_team\\["), pars = c("att", "sigma_att")) +
                          scale_y_discrete(label=c("att_team[1]" = "AC Milan", "att_team[2]" = "ACF Fiorentina",
                                                   "att_team[3]" = "AS Bari",  "att_team[4]" = "AS Roma",
                                                   "att_team[5]" = "Ascoli Calcio", "att_team[6]" ="Atalanta",
                                                   "att_team[7]" = "Cagliari Calcio", "att_team[8]" = "Genoa CFC",
                                                   "att_team[9]" = "Hellas Verona", "att_team[10]" = "Inter",
                                                   "att_team[11]" = "Juventus", "att_team[12]" = "Lazio Roma",
                                                   "att_team[13]" = "Parma AC", "att_team[14]" = "Sampdoria",
                                                   "att_team[15]" = "SSC Napoli", "att_team[16]" = "Torino FC",
                                                   "att_team[17]" = "US Cremonese", "att_team[18]" = "US Foggia"
                                                   ))+
                          ggtitle("Attack parameters credible intervals", subtitle = "Serie A 1991-92")

bayesplot::mcmc_intervals(as.matrix(fit_model_1),
                          regex_pars = c("def_team\\["), pars = c("def", "sigma_def")) +
  scale_y_discrete(label=c("def_team[1]" = "AC Milan", "def_team[2]" = "ACF Fiorentina",
                           "def_team[3]" = "AS Bari",  "def_team[4]" = "AS Roma",
                           "def_team[5]" = "Ascoli Calcio", "def_team[6]" ="Atalanta",
                           "def_team[7]" = "Cagliari Calcio", "def_team[8]" = "Genoa CFC",
                           "def_team[9]" = "Hellas Verona", "def_team[10]" = "Inter",
                           "def_team[11]" = "Juventus", "def_team[12]" = "Lazio Roma",
                           "def_team[13]" = "Parma AC", "def_team[14]" = "Sampdoria",
                           "def_team[15]" = "SSC Napoli", "def_team[16]" = "Torino FC",
                           "def_team[17]" = "US Cremonese", "def_team[18]" = "US Foggia"
  ))+
  ggtitle("Defense parameters-credible intervals", subtitle = "Serie A 1991-92")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep,
  group = df$home,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Median home goals vs posterior predictive sample medians", subtitle = "Serie A 1991-92")

bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep,
  group = df$home,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Std. dev. home goals vs posterior predictive sample std. dev.", subtitle = "Serie A 1991-92")

bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep,
  group = df$visitor,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Median away goals vs posterior predictive sample medians", subtitle = "Serie A 1991-92")

bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep,
  group = df$visitor,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Std. dev. away goals vs posterior predictive sample std. dev.", subtitle = "Serie A 1991-92")

mean_y_rep <- colMeans(y_home_rep)
std_resid <- (df$hgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + ggtitle("Home goals std res - Model 1", subtitle = "Serie A 1991-92") + hline_at(2) + hline_at(-2)

mean_y_rep <- colMeans(y_away_rep)
std_resid <- (df$vgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + ggtitle("Away goals std res - Model 1", subtitle = "Serie A 1991-92") + hline_at(2) + hline_at(-2)



### Model 2

fit_model_2 <- readRDS("models/model_2_fit_1991.rds")
y_home_rep_2 <- readRDS("simulations/y_home_rep_model_2_1991.rds")
y_away_rep_2 <- readRDS("simulations/y_away_rep_model_2_1991.rds")
loglik2_home <- as.matrix(fit_model_2, pars="log_lik_home")
loglik2_away <- as.matrix(fit_model_2, pars="log_lik_away")


df_sim2 <- simulate_data(df, y_home_rep = y_home_rep_2, y_away_rep = y_away_rep_2)
df_ranking_sim2 <- ranking(df_sim2, point_per_win = 2)
cum_points_list_sim2 <- cum_points(df_sim2)

bayesplot::ppc_bars(df$hgoal, y_home_rep_2[2000:4000,]) + ggtitle("Model 2 posterior predictive histogram of home goals", subtitle = "Serie A 1991-92")
bayesplot::ppc_bars(df$vgoal, y_away_rep_2[2000:4000,]) + ggtitle("Model 2 posterior predictive histogram of away goals", subtitle = "Serie A 1991-92")

bayesplot::ppc_bars_grouped(df$hgoal, y_home_rep_2[2000:4000,], group = df$home)  + ggtitle("Model 2 Posterior predictive histogram of home goals per team", subtitle = "Serie A 1991-92")
bayesplot::ppc_bars_grouped(df$hgoal, y_away_rep_2[2000:4000,], group = df$visitor)  + ggtitle("Model 2 Posterior predictive histogram of away goals per team", subtitle = "Serie A 1991-92")

bayesplot::mcmc_intervals(as.matrix(fit_model_2),
                          regex_pars = c("att_team\\[")) +
  scale_y_discrete(label=c("att_team[1]" = "AC Milan", "att_team[2]" = "ACF Fiorentina",
                           "att_team[3]" = "AS Bari",  "att_team[4]" = "AS Roma",
                           "att_team[5]" = "Ascoli Calcio", "att_team[6]" ="Atalanta",
                           "att_team[7]" = "Cagliari Calcio", "att_team[8]" = "Genoa CFC",
                           "att_team[9]" = "Hellas Verona", "att_team[10]" = "Inter",
                           "att_team[11]" = "Juventus", "att_team[12]" = "Lazio Roma",
                           "att_team[13]" = "Parma AC", "att_team[14]" = "Sampdoria",
                           "att_team[15]" = "SSC Napoli", "att_team[16]" = "Torino FC",
                           "att_team[17]" = "US Cremonese", "att_team[18]" = "US Foggia"
  ))+
  ggtitle("Model 2 Attack parameters-credible intervals", subtitle = "Serie A 1991-92")

bayesplot::mcmc_intervals(as.matrix(fit_model_2),
                          regex_pars = c("def_team\\[")) +
  scale_y_discrete(label=c("def_team[1]" = "AC Milan", "def_team[2]" = "ACF Fiorentina",
                           "def_team[3]" = "AS Bari",  "def_team[4]" = "AS Roma",
                           "def_team[5]" = "Ascoli Calcio", "def_team[6]" ="Atalanta",
                           "def_team[7]" = "Cagliari Calcio", "def_team[8]" = "Genoa CFC",
                           "def_team[9]" = "Hellas Verona", "def_team[10]" = "Inter",
                           "def_team[11]" = "Juventus", "def_team[12]" = "Lazio Roma",
                           "def_team[13]" = "Parma AC", "def_team[14]" = "Sampdoria",
                           "def_team[15]" = "SSC Napoli", "def_team[16]" = "Torino FC",
                           "def_team[17]" = "US Cremonese", "def_team[18]" = "US Foggia"
  ))+
  ggtitle("Model 2 Defense parameters-credible intervals", subtitle = "Serie A 1991-92")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep_2,
  group = df$home,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 2 Median home goals vs posterior predictive sample medians", subtitle = "Serie A 1991-92")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep_2,
  group = df$home,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 2 Std. dev. home goals vs posterior predictive sample std. dev.", subtitle = "Serie A 1991-92")


bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep_2,
  group = df$visitor,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 2 Median away goals vs posterior predictive sample medians", subtitle = "Serie A 1991-92")

bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep_2,
  group = df$visitor,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 2 Std. dev. away goals vs posterior predictive sample std. dev.", subtitle = "Serie A 1991-92")


mean_y_rep <- colMeans(y_home_rep_2)
std_resid <- (df$hgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid, main="Home goals std res - model 2") + ggtitle("Home goals std res - Model 2", subtitle = "Serie A 1991-92") + hline_at(2) + hline_at(-2)
 + hline_at(2) + hline_at(-2)

mean_y_rep <- colMeans(y_away_rep_2)
std_resid <- (df$vgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid, main="Away goals std res - model 2") + ggtitle("Away goals std res - Model 2", subtitle = "Serie A 1991-92") + hline_at(2) + hline_at(-2)


# Set plot layout
layout(mat = matrix(c(1:9), 
                    nrow = 3, 
                    ncol = 3),
             heights = c(1, 1),    # Heights of the two rows
             widths = c(1, 1))     # Widths of the two columns

### Model 1 and 2 on 1991 data

x<-1:34;
for (team in sort(unique(df$home))) {
  
  y1=cum_points_list[[team]]
  y2=cum_points_list_sim1[[team]]
  y3=cum_points_list_sim2[[team]]
  plot(x,y1,xlim = c(0,34),ylim = c(0,60),type = "l", col="black", main=team, xlab="Games", ylab="Points")
  lines(x, y2, pch=18, col="red", type="l", lwd=2)
  lines(x, y3, pch=18, col="blue", type="l", lwd=2)
  legend(1, 50, legend=c("Real values", "Model 1", "Model 2"),
         col=c("black", "red", "blue"), lty=1:1, lwd=2:2)
}

library(loo)




################################


#### SEASON 2007-08

df <- readRDS("./data/season2007.rds")
df_ranking <- readRDS("./data/season2007_ranking.rds")

### Model 1
fit_model_1 <- readRDS("models/model_1_fit_2007.rds")
y_home_rep <- readRDS("simulations/y_home_rep_model_1_2007.rds")
y_away_rep <- readRDS("simulations/y_away_rep_model_1_2007.rds")
df_sim1 <- simulate_data(df, y_home_rep = y_home_rep, y_away_rep = y_away_rep)
df_ranking_sim1 <- ranking(df_sim1, point_per_win = 3)
loglik1_home <- as.matrix(fit_model_1, pars="log_lik_home")
loglik1_away <- as.matrix(fit_model_1, pars="log_lik_away")
cum_points_list_sim1 <- cum_points(df_sim1)
cum_points_list <- readRDS("data/season2007_cum_points.rds")


bayesplot::ppc_bars(df$hgoal, y_home_rep[2000:4000,]) + ggtitle("Model 1 Posterior predictive histogram of home goals", subtitle = "Serie A 2007-08")
bayesplot::ppc_bars(df$vgoal, y_away_rep[2000:4000,]) + ggtitle("Model 1 Posterior predictive histogram of away goals", subtitle = "Serie A 2007-08")

bayesplot::ppc_bars_grouped(df$hgoal, y_home_rep[2000:4000,], group = df$home)  + ggtitle("Model 1 Posterior predictive histogram of home goals per team", subtitle = "Serie A 2007-08")
bayesplot::ppc_bars_grouped(df$vgoal, y_away_rep[2000:4000,], group = df$visitor)  + ggtitle("Model 1 Posterior predictive histogram of away goals per team", subtitle = "Serie A 2007-08")

bayesplot::mcmc_intervals(as.matrix(fit_model_1),
                          regex_pars = c("att_team\\["), pars = c("att", "sigma_att")) +
  scale_y_discrete(label=c("att_team[1]" = "AC Milan", "att_team[2]" = "AC Siena",
                           "att_team[3]" = "ACF Fiorentina",  "att_team[4]" = "AS Livorno",
                           "att_team[5]" = "AS Roma", "att_team[6]" ="Atalanta",
                           "att_team[7]" = "Cagliari Calcio", "att_team[8]" = "Calcio Catania",
                           "att_team[9]" = "Empoli FC", "att_team[10]" = "Genoa CFC",
                           "att_team[11]" = "Inter", "att_team[12]" = "Juventus",
                           "att_team[13]" = "Lazio Roma", "att_team[14]" = "Parma FC",
                           "att_team[15]" = "Reggina Calcio", "att_team[16]" = "Sampdoria",
                           "att_team[17]" = "SSC Napoli", "att_team[18]" = "Torino FC",
                           "att_team[19]" = "Udinese Calcio", "att_team[20]" = "US Palermo"))+
  ggtitle("Model 1 Attack parameters-credible intervals", subtitle = "Serie A 2007-08")

bayesplot::mcmc_intervals(as.matrix(fit_model_1),
                          regex_pars = c("def_team\\["), pars = c("def", "sigma_def")) +
  scale_y_discrete(label=c("def_team[1]" = "AC Milan", "def_team[2]" = "AC Siena",
                           "def_team[3]" = "ACF Fiorentina",  "def_team[4]" = "AS Livorno",
                           "def_team[5]" = "AS Roma", "def_team[6]" ="Atalanta",
                           "def_team[7]" = "Cagliari Calcio", "def_team[8]" = "Calcio Catania",
                           "def_team[9]" = "Empoli FC", "def_team[10]" = "Genoa CFC",
                           "def_team[11]" = "Inter", "def_team[12]" = "Juventus",
                           "def_team[13]" = "Lazio Roma", "def_team[14]" = "Parma FC",
                           "def_team[15]" = "Reggina Calcio", "def_team[16]" = "Sampdoria",
                           "def_team[17]" = "SSC Napoli", "def_team[18]" = "Torino FC",
                           "def_team[19]" = "Udinese Calcio", "def_team[20]" = "US Palermo"))+
  ggtitle("Model 1 Defense parameters-credible intervals", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep,
  group = df$home,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 1 Median home goals vs posterior predictive sample medians", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep,
  group = df$home,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 1 Std. dev. home goals vs posterior predictive sample std. dev.", subtitle = "Serie A 2007-08")

bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep,
  group = df$visitor,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 1 Median away goals vs posterior predictive sample medians", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep,
  group = df$visitor,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 1 Std. dev. away goals vs posterior predictive sample std. dev.", subtitle = "Serie A 2007-08")


mean_y_rep <- colMeans(y_home_rep)
std_resid <- (df$hgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2) + ggtitle("Home goals std res - Model 1", subtitle = "Serie A 2007-08") + hline_at(2) + hline_at(-2)


mean_y_rep <- colMeans(y_away_rep)
std_resid <- (df$vgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) +  ggtitle("Away goals std res - Model 1", subtitle = "Serie A 2007-08") + hline_at(2) + hline_at(-2)



### Model 2

fit_model_2 <- readRDS("models/model_2_fit_2007.rds")
y_home_rep_2 <- readRDS("simulations/y_home_rep_model_2_2007.rds")
y_away_rep_2 <- readRDS("simulations/y_away_rep_model_2_2007.rds")
df_sim2 <- simulate_data(df, y_home_rep = y_home_rep_2, y_away_rep = y_away_rep_2)
df_ranking_sim2 <- ranking(df_sim2, point_per_win = 3)
cum_points_list_sim2 <- cum_points(df_sim2)

bayesplot::ppc_bars(df$hgoal, y_home_rep_2[1:200,]) + ggtitle("Model 2 Posterior predictive histogram of home goals", subtitle = "Serie A 2007-08")
bayesplot::ppc_bars(df$vgoal, y_away_rep_2[1:200,]) + ggtitle("Model 2 Posterior predictive histogram of away goals", subtitle = "Serie A 2007-08")

bayesplot::ppc_bars_grouped(df$hgoal, y_home_rep_2[1:200,], group = df$home)  + ggtitle("Model 2 Posterior predictive histogram of home goals per team", subtitle = "Serie A 2007-08")
bayesplot::ppc_bars_grouped(df$hgoal, y_away_rep_2[1:200,], group = df$visitor)  + ggtitle("Model 2 Posterior predictive histogram of away goals per team", subtitle = "Serie A 2007-08")

bayesplot::mcmc_intervals(as.matrix(fit_model_2),
                          regex_pars = c("att_team\\[")) +
  scale_y_discrete(label=c("att_team[1]" = "AC Milan", "att_team[2]" = "AC Siena",
                           "att_team[3]" = "ACF Fiorentina",  "att_team[4]" = "AS Livorno",
                           "att_team[5]" = "AS Roma", "att_team[6]" ="Atalanta",
                           "att_team[7]" = "Cagliari Calcio", "att_team[8]" = "Calcio Catania",
                           "att_team[9]" = "Empoli FC", "att_team[10]" = "Genoa CFC",
                           "att_team[11]" = "Inter", "att_team[12]" = "Juventus",
                           "att_team[13]" = "Lazio Roma", "att_team[14]" = "Parma FC",
                           "att_team[15]" = "Reggina Calcio", "att_team[16]" = "Sampdoria",
                           "att_team[17]" = "SSC Napoli", "att_team[18]" = "Torino FC",
                           "att_team[19]" = "Udinese Calcio", "att_team[20]" = "US Palermo"))+
  ggtitle("Model 2 Attack parameters-credible intervals", subtitle = "Serie A 2007-08")

bayesplot::mcmc_intervals(as.matrix(fit_model_2),
                          regex_pars = c("def_team\\[")) +
  scale_y_discrete(label=c("def_team[1]" = "AC Milan", "def_team[2]" = "AC Siena",
                           "def_team[3]" = "ACF Fiorentina",  "def_team[4]" = "AS Livorno",
                           "def_team[5]" = "AS Roma", "def_team[6]" ="Atalanta",
                           "def_team[7]" = "Cagliari Calcio", "def_team[8]" = "Calcio Catania",
                           "def_team[9]" = "Empoli FC", "def_team[10]" = "Genoa CFC",
                           "def_team[11]" = "Inter", "def_team[12]" = "Juventus",
                           "def_team[13]" = "Lazio Roma", "def_team[14]" = "Parma FC",
                           "def_team[15]" = "Reggina Calcio", "def_team[16]" = "Sampdoria",
                           "def_team[17]" = "SSC Napoli", "def_team[18]" = "Torino FC",
                           "def_team[19]" = "Udinese Calcio", "def_team[20]" = "US Palermo"))+
  ggtitle("Model 2 Defense parameters-credible intervals", subtitle = "Serie A 2007-08")



bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep_2,
  group = df$home,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 2 Median home goals vs posterior predictive sample medians", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$hgoal,
  yrep = y_home_rep_2,
  group = df$home,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 1 Std. dev. home goals vs posterior predictive sample std. dev.", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep_2,
  group = df$visitor,
  stat = 'median',
  binwidth = 0.5
) + ggtitle("Model 2 Median away goals vs posterior predictive sample medians", subtitle = "Serie A 2007-08")


bayesplot::ppc_stat_grouped(
  y = df$vgoal,
  yrep = y_away_rep_2,
  group = df$visitor,
  stat = 'sd',
  binwidth = 0.1
) + ggtitle("Model 1 Std. dev. away goals vs posterior predictive sample std. dev.", subtitle = "Serie A 2007-08")


mean_y_rep <- colMeans(y_home_rep_2)
std_resid <- (df$hgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + hline_at(-2) + ggtitle("Home goals std res - Model 2", subtitle = "Serie A 2007-08") + hline_at(2) + hline_at(-2)


mean_y_rep <- colMeans(y_away_rep_2)
std_resid <- (df$vgoal - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + hline_at(-2) + ggtitle("Away goals std res - Model 2", subtitle = "Serie A 2007-08") + hline_at(2) + hline_at(-2)




# Set plot layout
layout(mat = matrix(c(1:9), 
                    nrow = 3, 
                    ncol = 3),
       heights = c(1, 1),    # Heights of the two rows
       widths = c(1, 1))     # Widths of the two columns

### Model 1 and 2 on 1991 data

x<-1:38;
for (team in sort(unique(df$home))) {
  
  y1=cum_points_list[[team]]
  y2=cum_points_list_sim1[[team]]
  y3=cum_points_list_sim2[[team]]
  plot(x,y1,xlim = c(0,34),ylim = c(0,60),type = "l", col="black", main=team, xlab="Games", ylab="Points")
  lines(x, y2, pch=18, col="red", type="l", lwd=2)
  lines(x, y3, pch=18, col="blue", type="l", lwd=2)
  legend(1, 50, legend=c("Real values", "Model 1", "Model 2"),
         col=c("black", "red", "blue"), lty=1:1, lwd=2:2)
}


#################################################

library(loo)

loo1_home <- loo(loglik1_home)
loo2_home <- loo(loglik2_home)

elpdi1 <- loo1_home$pointwise[, "elpd_loo"]
elpdi2 <- loo2_home$pointwise[, "elpd_loo"]

elpd_diffs <-
  data.frame(
    home = df$home,
    visitor=df$visitor,
    FT = df$FT,
    diff12 = elpdi2 - elpdi1
  )

assign_rank_1991 <- readRDS("functions/assign_rank_1991.rds")

assign_position_1991 <- function(team){
  df_ranking_1991 <- readRDS("data/season1991_ranking.rds")
  return(df_ranking_1991[df_ranking_1991$team == team, ]$position)
}

elpd_diffs <- elpd_diffs %>% 
  mutate(team_rank = sapply(home, assign_rank_1991), team_pos=sapply(home, assign_position_1991))

elpd_diffs <- elpd_diffs %>% 
  arrange(team_rank, team_pos) %>%
  mutate(
    big_diff12 = abs(diff12) > 6,
    Index = 1:n()
  )

theme_set(bayesplot::theme_default(base_size = 14))
theme_update(axis.text = element_text(size = 16))

elpd_diff_plot <- ggplot(elpd_diffs, aes(x = Index,y = diff12)) + 
  geom_point(
    aes(colour = factor(team_rank)), 
    size = 3, 
    alpha = 0.8
  ) + 
  #scale_color_manual(
  #  values = 
  #    c("top" = "#00C094",
  #      "medium" = "#FB61D7",
  #      "bottom" = "#00B6EB")
  #) +
  hline_0(size = 0.25) +
  ylab(expression(ELPD[i][2] - ELPD[i][1])) +
  labs(color='Team category') +
  #legend_none() +
  ggtitle("Goals home - Difference in pointwise ELPD Model 1 and Model 2", subtitle = "Serie A 1991-92")

plot(elpd_diff_plot)
ggsave(filename = "plots/loo_elpd_diff_12_home.png", width = 7.5, height = 5.75)

loo1_away <- loo(loglik1_away)
loo2_away <- loo(loglik2_away)

elpdi1 <- loo1_away$pointwise[, "elpd_loo"]
elpdi2 <- loo2_away$pointwise[, "elpd_loo"]

elpd_diffs <-
  data.frame(
    home = df$home,
    visitor=df$visitor,
    FT = df$FT,
    diff12 = elpdi2 - elpdi1
  )

elpd_diffs <- elpd_diffs %>% 
  mutate(team_rank = sapply(visitor, assign_rank_1991), team_pos=sapply(visitor, assign_position_1991))

elpd_diffs <- elpd_diffs %>% 
  arrange(team_rank, visitor) %>%
  mutate(
    big_diff12 = abs(diff12) > 6,
    Index = 1:n()
  )

theme_set(bayesplot::theme_default(base_size = 14))
theme_update(axis.text = element_text(size = 16))

elpd_diff_plot <- ggplot(elpd_diffs, aes(x = Index,y = diff12)) + 
  geom_point(
    aes(colour = factor(team_rank)), 
    size = 3, 
    alpha = 0.8
  ) + 
  #scale_color_manual(
  #  values = 
  #    c("top" = "#00C094",
  #      "medium" = "#FB61D7",
  #      "bottom" = "#00B6EB")
  #) +
  hline_0(size = 0.25) +
  ylab(expression(ELPD[i][2] - ELPD[i][1])) +
  labs(color='Team category') +
  #legend_none() +
  ggtitle("Goals away - Difference in pointwise ELPD Model 1 and Model 2", subtitle = "Serie A 1991-92")

plot(elpd_diff_plot)
ggsave(filename = "plots/loo_elpd_diff_12_away.png", width = 7.5, height = 5.75)
