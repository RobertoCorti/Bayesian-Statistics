### libraries
library(engsoccerdata)
library(dplyr)
library(tidyr)

### functions
ranking <- function(df, point_per_win){
  
  temp <-
    rbind(
      df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
      df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
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
    mutate(Pts = (wins*point_per_win) + draws) %>%
    arrange(desc(Pts))
  
  df_ranking_sim <- df_ranking_sim %>% mutate(position = as.integer(rank(desc(Pts))))
  return(df_ranking_sim)
}


assign_dir_hyp <- function(year){
  
  df <- engsoccerdata::italy %>% 
    filter(Season == year)
  
  df_prev <- engsoccerdata::italy %>% 
    filter(Season == year-1)
  
  df_ranking_prev <- ranking(df_prev, point_per_win = 2)
  
  team_names <- sort(unique(df$home))
  
  hyps <- matrix(0, nrow = length(team_names), ncol = 3)
  
  for (i in 1:length(team_names)) {
    if (team_names[i] %in% unique(df_ranking_prev$team)) {
      prev_pos <- df_ranking_prev[df_ranking_prev$team == team_names[i], ]$position
      if (prev_pos<=6) {
          hyps[i,1] <- 1
          hyps[i,2] <- 1
          hyps[i,3] <- 1.5
      }
      else if (prev_pos<=12) {
        hyps[i,1] <- 1
        hyps[i,2] <- 1.5
        hyps[i,3] <- 1
      }
      else {
        hyps[i,1] <- 1.5
        hyps[i,2] <- 1
        hyps[i,3] <- 1
      }
    }
    else{
      hyps[i,1] <- 2
      hyps[i,2] <- 1
      hyps[i,3] <- 1
    }
    
  }
  
  return(hyps)
  
}

dirichlet_hyper_1991 <- assign_dir_hyp(year=1991)
dirichlet_hyper_2007 <- assign_dir_hyp(year=2007)


saveRDS(dirichlet_hyper_1991, "dirichlet_hyper1991.rds")
saveRDS(dirichlet_hyper_2007, "dirichlet_hyper2007.rds")