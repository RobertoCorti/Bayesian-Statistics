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

last_5_positions <- function(team, current_year, points_per_win){
  positions <- rep(0, 3)
  for (i in 1:5) {
    df_temp <- engsoccerdata::italy %>% 
                 filter(Season == current_year-i)
    df_temp_rank <- ranking(df_temp, point_per_win = points_per_win)
    
    if (team %in% unique(df_temp_rank$team)){
      position <- df_temp_rank[df_temp_rank$team == team, ]$position
      
      if(position <= 4){
        positions[3] <- positions[3] + 1
      }
      else if(position<=12){
        positions[2] <- positions[2] + 1
      }
      else {
        positions[1] <- positions[1] + 1
      }
    }
    
    else{
      positions[1] <- positions[1] + 1
    }
  }
  return(positions)
}


assign_dir_hyp <- function(year, points_per_win){
  
  df <- engsoccerdata::italy %>% 
              filter(Season == year)
  
  team_names <- sort(unique(df$home))
  
  hyps <- matrix(0, nrow = length(team_names), ncol = 3)
  
  for (i in 1:length(team_names)) {
    
    last_5_years <- last_5_positions(team = team_names[i], current_year = year, points_per_win)
    
    for (k in 1:3) {
      hyps[i,k] <- 1+last_5_years[k]
    }
    
  }
  
  return(hyps)
  
}

dirichlet_hyper_1991 <- assign_dir_hyp(year=1991, points_per_win = 2)
dirichlet_hyper_2007 <- assign_dir_hyp(year=2007, points_per_win = 3)


saveRDS(dirichlet_hyper_1991, "functions/dirichlet_hyper1991.rds")
saveRDS(dirichlet_hyper_2007, "functions/dirichlet_hyper2007.rds")