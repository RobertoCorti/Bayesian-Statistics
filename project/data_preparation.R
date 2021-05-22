### libraries
library(engsoccerdata)
library(dplyr)
library(tidyr)

### get data
df <- engsoccerdata::italy

df <- df %>% 
        filter(Season == 1991)

df <- df %>%
  group_by(home) %>%
  mutate(home_id=cur_group_id())

df <- df %>%
  group_by(visitor) %>%
  mutate(visitor_id=cur_group_id())

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
              select(team_id, team_name, goals_made, home_goals, visitor_goals)
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

assign_rank <- function(name){
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

df_goals <- df_goals %>% 
            mutate(rank = sapply(team_name, assign_rank))

saveRDS(df, file="data/season1991.rds")
saveRDS(df_goals, file = "data/season1991_goals.rds")




