### libraries
library(engsoccerdata)
library(ggplot2)
library(dplyr)
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


# create dataframe for team # goals
# make plot and "color plot" for each part of the table




saveRDS(df, file="season1991.rds")