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
                             hgoal= as.integer(round(colMeans(y_home_rep))),
                             vgoal= as.integer(round(colMeans(y_away_rep))), 
                             home_id = df$home_id,
                             visitor_id = df$visitor_id)
  return(df_simulate)
}


ranking <- function(df_simulate){
  
  temp <-
    rbind(
      df_simulate %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
      df_simulate %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
    ) #rbind two copies of the orignal df, simply reversing home/away team for each match
  
  df_ranking_sim<-
    temp %>%
    mutate(GD = GF-GA) %>%
    group_by(team) %>%
    summarize(games_played = n(),
              goals_made = sum(GF),
              goals_received = sum(GA),
              goals_difference = sum(GD),
              wins = sum(GD>0),
              draws = sum(GD==0),
              losses = sum(GD<0)
    ) %>%
    mutate(Pts = (wins*2) + draws) %>%
    arrange(desc(Pts))
  
  df_ranking_sim <- df_ranking_sim %>% mutate(position = as.integer(rank(desc(Pts))))
  return(df_ranking_sim)
}


cum_points <- function(df){
  
  team_names <- sort(unique(df$home))
  cum_points_list <- vector("list", length = length(team_names))
  names(cum_points_list) <- team_names
  
  for (name in team_names) {
    df_team <- df %>% 
      filter(home==name | visitor==name) %>% 
      select(Date=Date, home=home, visitor=visitor, hgoal=hgoal, vgoal=vgoal) %>% 
      mutate(loc=ifelse(home==name, 1, 0)) %>%
      mutate(DG =ifelse(loc==1, hgoal-vgoal, vgoal-hgoal)) %>% 
      mutate(point=ifelse(DG>0, 2, ifelse(DG==0, 1, 0)))
    
    cum_points_list[[name]] <- cumsum(df_team$point)
  }
  
  return(cum_points_list)
}

df <- readRDS("./data/season1991.rds")
df_ranking <- readRDS("./data/season1991_ranking.rds")

# read data
fit_model_1 <- readRDS("models/model_1_fit.rds")
fit_model_2 <- readRDS("models/model_2_fit.rds")
y_home_rep <- readRDS("y_home_rep.rds")
y_away_rep <- readRDS("y_away_rep.rds")
y_home_rep_2 <- readRDS("y_home_rep_2.rds")
y_away_rep_2 <- readRDS("y_away_rep_2.rds")

df_sim1 <- simulate_data(df, y_home_rep = y_home_rep, y_away_rep = y_away_rep)
df_sim2 <- simulate_data(df, y_home_rep = y_home_rep_2, y_away_rep = y_away_rep_2)

df_ranking_sim1 <- ranking(df_sim1)
df_ranking_sim2 <- ranking(df_sim2)


cum_points_list_sim1 <- cum_points(df_sim1)
cum_points_list_sim2 <- cum_points(df_sim2)
cum_points_list <- cum_points(df)

# Set plot layout
layout(mat = matrix(c(1:9), 
                    nrow = 3, 
                    ncol = 3),
             heights = c(1, 1),    # Heights of the two rows
             widths = c(1, 1))     # Widths of the two columns


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

bayesplot::ppc_dens_overlay(df$hgoal, y_home_rep[1:200,])
bayesplot::ppc_dens_overlay(df$hgoal, y_home_rep_2[1:200,])

bayesplot::ppc_dens_overlay(df$vgoal, y_away_rep[1:200,])
bayesplot::ppc_dens_overlay(df$vgoal, y_away_rep_2[1:200,])

mean_y_rep_1 <- colMeans(y_home_rep)
std_resid <- (df$hgoal - mean_y_rep_1) / sqrt(mean_y_rep_1)
qplot(mean_y_rep_1, std_resid) + hline_at(2) + hline_at(-2)

mean_y_rep_2 <- colMeans(y_home_rep_2)
std_resid <- (df$hgoal - mean_y_rep_2) / sqrt(mean_y_rep_2)
qplot(mean_y_rep_2, std_resid) + hline_at(2) + hline_at(-2)


