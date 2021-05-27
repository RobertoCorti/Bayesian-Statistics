library(dplyr)
library(bayesplot)


# read data
fit_model_1 <- readRDS("models/model_1_fit.rds")
y_home_rep <- readRDS("y_home_rep.rds")
y_away_rep <- readRDS("y_away_rep.rds")
df <- readRDS("./data/season1991.rds")
df_ranking <- readRDS("./data/season1991_ranking.rds")

print(fit_model_1, pars = c('att_team','def_team','home'))

# create ranking with simulations
df_simulate <-  data.frame(Date = df$Date,
                           Season = df$Season,
                           home = df$home,
                           visitor = df$visitor,
                           hgoal= as.integer(round(colMeans(y_home_rep))),
                           vgoal= as.integer(round(colMeans(y_away_rep))), 
                           home_id = df$home_id,
                           visitor_id = df$visitor_id)


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

remove(temp)
df_ranking_sim <- df_ranking_sim %>% mutate(position = as.integer(rank(desc(Pts))))

# create cum points lines

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


cum_points_list_sim <- cum_points(df_simulate)
cum_points_list <- cum_points(df)

# Set plot layout
layout(mat = matrix(c(1:9), 
                    nrow = 3, 
                    ncol = 3),
             heights = c(1, 1),    # Heights of the two rows
             widths = c(1, 1))     # Widths of the two columns


x<-1:34;
for (team in sort(unique(df$home))[1:9]) {
  
  y1=cum_points_list[[team]]; y2=cum_points_list_sim[[team]]
  plot(x,y1,xlim = c(0,34),ylim = c(0,60),type = "l", col="black", main=team, xlab="Games", ylab="Points")
  lines(x, y2, pch=18, col="red", type="l", lwd=2)
  legend(1, 50, legend=c("Real values", "Fitted values"),
         col=c("black", "red"), lty=1:1, lwd=2:2)
}

# Set plot layout
layout(mat = matrix(c(1:9), 
                    nrow = 3, 
                    ncol = 3),
       heights = c(1, 1),    # Heights of the two rows
       widths = c(1, 1))     # Widths of the two columns


x<-1:34;
for (team in sort(unique(df$home))[9:18]) {
  print(team)
  y1=cum_points_list[[team]]; y2=cum_points_list_sim[[team]]
  plot(x,y1,xlim = c(0,34),ylim = c(0,60),type = "l", col="black", main=team, xlab="Games", ylab="Points")
  lines(x, y2, pch=18, col="red", type="l", lwd=2)
  legend(1, 50, legend=c("Real values", "Fitted values"),
         col=c("black", "red"), lty=1:1, lwd=2:2)
}





