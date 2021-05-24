library(ggplot2)
library(dplyr)
library(cowplot)
library(grid)

df <- readRDS("./data/season1991.rds")
df_goals <- readRDS("./data/season1991_goals.rds")


home <- sapply(df$hgoal, paste, collapse = " ")
away <- sapply(df$vgoal, paste, collapse = " ")
list_result <- vector(length = length(home))

for (i in 1:length(list_result)) {
  list_result[i] <- paste(home[i], away[i], sep="-")
}

df$result <- list_result

ggplot(df, aes(x = result)) + 
  geom_bar(position = "stack", color="black", fill="darkblue")+
  theme_classic()+
  ggtitle("Which was the most common result?", subtitle="Distribution of results in Serie A 1991-1992")+
  labs(x="Result", y="Count")+
  theme(plot.title = element_text(size = 20, face = "bold"))
  
grob <- grobTree(textGrob("Goals made = Goals received", x=0.63,  y=0.86, hjust=0,
                          gp=gpar(col="black", fontsize=13, alpha=0.4)))


ggplot(df_goals,  aes(x=goals_received, y=goals_made, colour=rank)) + 
  geom_point(size=3) +
  geom_text(aes(label=team_name),hjust=0, vjust=0) +
  annotation_custom(grob)+
  geom_abline(slope = 1, intercept = 0, color="black", linetype="dashed", size=1.5, alpha=0.2)+
  
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
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Top teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


hist_medium_home <- ggplot(df[df['home_rank']=='medium',], aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Medium teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))

hist_bottom_home <- ggplot(df[df['home_rank']=='bottom',], aes(x=hgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Bottom teams - Home goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


cowplot::plot_grid(hist_top_home, hist_medium_home, hist_bottom_home)


hist_top_visitor <- ggplot(df[df['home_rank']=='top',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Top teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


hist_medium_visitor <- ggplot(df[df['home_rank']=='medium',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Medium teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))

hist_bottom_visitor <- ggplot(df[df['home_rank']=='bottom',], aes(x=vgoal)) +
  geom_histogram(binwidth = 0.5, color="black", fill="darkblue")+
  labs(x="Number of goals in a match", y="Count")+
  ggtitle("Bottom teams - Visitor goals", subtitle="Serie A 1991-1992")+
  theme(plot.title = element_text(size = 20, face = "bold"))


cowplot::plot_grid(hist_top_visitor, hist_medium_visitor, hist_bottom_visitor)

