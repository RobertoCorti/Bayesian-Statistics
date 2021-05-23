library(ggplot2)
library(dplyr)
library(cowplot)

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


# histogram for goals made distribution in top teams


## histogram for df$hgoals  and df$vgoals + hist divided for three clusters.

ggplot(df, aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Home goals distribution", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))
  
  
ggplot(df, aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Visitor goals distribution", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))



hist_top_home <- ggplot(df[df['home_rank']=='top',], aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Top teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


hist_medium_home <- ggplot(df[df['home_rank']=='medium',], aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Medium teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))

hist_bottom_home <- ggplot(df[df['home_rank']=='bottom',], aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Bottom teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


cowplot::plot_grid(hist_top_home, hist_medium_home, hist_bottom_home)


hist_top_visitor <- ggplot(df[df['home_rank']=='top',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Top teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


hist_medium_visitor <- ggplot(df[df['home_rank']=='medium',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Medium teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))

hist_bottom_visitor <- ggplot(df[df['home_rank']=='bottom',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Number of occurences")+
  ggtitle("Bottom teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


cowplot::plot_grid(hist_top_visitor, hist_medium_visitor, hist_bottom_visitor)

