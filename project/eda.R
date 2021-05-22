library(ggplot2)

df <- readRDS("./data/season1991.rds")
df_goals <- readRDS("./data/season1991_goals.rds")


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

