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


data_goals <- function(df){
  
  team_names <- sort(unique(df$home))
  
  df_home_goals <- df %>% 
    group_by(home_id) %>% 
    summarise(home_goals=sum(hgoal)) %>% 
    mutate(team_name = team_names) %>% 
    rename(team_id = home_id)
  
  df_away_goals <- df %>% 
    group_by(visitor_id) %>% 
    summarise(visitor_goals=sum(vgoal)) %>% 
    mutate(team_name = team_names) %>%
    rename(team_id = visitor_id)
  
  
  df_goals <-   merge(df_home_goals, df_away_goals, by=c("team_id", "team_name"), sort=FALSE) %>% 
    mutate(goals_made=home_goals + visitor_goals) %>% 
    select(team_id, team_name, goals_made, home_goals, visitor_goals) %>% 
    rename(goals_made_home = home_goals, goals_made_visitor = visitor_goals)
  
  remove(df_away_goals)
  remove(df_home_goals)
  
  df_home_goals <- df %>% 
    group_by(home_id) %>% 
    summarise(home_goals=sum(vgoal)) %>% 
    mutate(team_name = team_names) %>% 
    rename(team_id = home_id)
  
  df_away_goals <- df %>% 
    group_by(visitor_id) %>% 
    summarise(visitor_goals=sum(hgoal)) %>% 
    mutate(team_name = team_names) %>%
    rename(team_id = visitor_id)
  
  df_goals$goals_received <- merge(df_home_goals, df_away_goals, by=c("team_id", "team_name"), sort=FALSE) %>% 
    mutate(goals_received=home_goals + visitor_goals) %>%
    pull(goals_received)
  
  df_goals$goals_received_home <- df_home_goals %>% 
    select(home_goals) %>% 
    pull(home_goals)
  
  df_goals$goals_received_visitor <- df_away_goals %>% 
    select(visitor_goals) %>% 
    pull(visitor_goals)
  
  remove(df_away_goals)
  remove(df_home_goals)
  
  return(df_goals)
  
}



assign_rank_1991 <- function(name, df_ranking){
  if(name %in% c("AC Milan", "Juventus",
                 "Torino FC", "SSC Napoli",
                 "AS Roma", "Sampdoria") ){
    return("top")
  }
  else if (name %in% c("Parma AC", "Inter",
                       "US Foggia", "Lazio Roma",
                       "Atalanta", "ACF Fiorentina") ){
    return("medium")
  }
  else
    return("bottom")
}


### get data

### 1991-92
df <- engsoccerdata::italy

df <- df %>% 
        filter(Season == 1991)

df <- df %>%
  group_by(home) %>%
  mutate(home_id=cur_group_id())

df <- df %>%
  group_by(visitor) %>%
  mutate(visitor_id=cur_group_id())

df_goals <- data_goals(df)

df_ranking <- ranking(df, point_per_win = 2)

df_goals <- df_goals %>% 
            mutate(rank = sapply(team_name, assign_rank_1991))

df <- df %>% 
  mutate(home_rank = sapply(home, assign_rank_1991))

df <- df %>% 
  mutate(visitor_rank = sapply(visitor, assign_rank_1991))


cum_points_list <- cum_points(df)


saveRDS(df, file="data/season1991.rds")
saveRDS(df_goals, file = "data/season1991_goals.rds")
saveRDS(df_ranking, file="data/season1991_ranking.rds")
saveRDS(cum_points_list, file="data/season1991_cum_points.rds")

remove(df)
remove(df_goals)
remove(df_ranking)
remove(cum_points_list)

### 2007-08

assign_rank_2007 <- function(name, df_ranking){
  if(name %in% c("Inter", "AS Roma",
                 "Juventus", "ACF Fiorentina",
                 "AC Milan", "Sampdoria") ){
    return("top")
  }
  else if (name %in% c("Udinese Calcio", "SSC Napoli",
                       "Atalanta", "Genoa CFC",
                       "US Palermo", "Lazio Roma") ){
    return("medium")
  }
  else
    return("bottom")
}

df <- engsoccerdata::italy

df <- df %>% 
  filter(Season == 2007)

df <- df %>%
  group_by(home) %>%
  mutate(home_id=cur_group_id())

df <- df %>%
  group_by(visitor) %>%
  mutate(visitor_id=cur_group_id())

df_goals <- data_goals(df)


df_goals <- df_goals %>% 
  mutate(rank = sapply(team_name, assign_rank_2007))

df <- df %>% 
  mutate(home_rank = sapply(home, assign_rank_2007))

df <- df %>% 
  mutate(visitor_rank = sapply(visitor, assign_rank_2007))

df_ranking <- ranking(df, point_per_win = 3)

cum_points_list <- cum_points(df)


saveRDS(df, file="data/season2007.rds")
saveRDS(df_goals, file = "data/season2007_goals.rds")
saveRDS(df_ranking, file="data/season2007_ranking.rds")
saveRDS(cum_points_list, file="data/season2007_cum_points.rds")

remove(df)
remove(df_goals)
remove(df_ranking)
remove(cum_points_list)


saveRDS(assign_rank_1991, file="functions/assign_rank_1991.rds")
saveRDS(assign_rank_2007, file="functions/assign_rank_2007.rds")
saveRDS(cum_points, file="functions/cum_points.rds")
saveRDS(ranking, file="functions/ranking.rds")