### libraries
library(engsoccerdata)
library(ggplot2)
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

ggplot(df_goals,  aes(x=goals_received, y=goals_made, colour=rank)) + 
  geom_point(size=3) +
  geom_text(aes(label=team_name),hjust=0, vjust=0) +
  ggtitle("Goals distrubution", subtitle="Serie A 1991-1992")+
  labs(x="Goals received", y="Goals made")+
  theme_bw()

  
df_goals_long <- df_goals %>% 
                 select(team_name, home_goals, visitor_goals) %>% 
                 gather("Stat", "Value", -team_name)

ggplot(df_goals_long, aes(x = team_name, y = Value, fill = Stat)) +
  geom_col(position = "stack")+
  theme(legend.title = element_blank()) +
  geom_text(aes(label = Value), size = 5, position = position_stack(vjust = 0.5))+
  ggtitle("Do teams score more at home?", subtitle="Distribution of goals in Serie A 1991-1992")+
  labs(x="Team", y="Number of goals")+
  theme(plot.title = element_text(size = 20, face = "bold"))

remove(df_goals_long)





# create dataframe for team # goals
# make plot and "color plot" for each part of the table




saveRDS(df, file="season1991.rds")