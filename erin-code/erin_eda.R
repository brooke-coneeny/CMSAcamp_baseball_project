# PURPOSE: EDA to explore the relationship between launch angle, exit velocity, and
# wOBA. Will explore with 2021 data. 

#load packages and data
library(baseballr)
library(data.table)
library(tidyverse)
library(ff)

batter_all_2021 <- read_rds("private_data/all2021data.rds")
---------------------------------------------------------------------------------

#create a table with the number of batted balls each player has hit to the categories
# of out, single, double, triple, homerun
batter_hits <- batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run")) %>%
  group_by(player_name) %>%
  count(events_group) %>%
  pivot_wider(id_cols = player_name, names_from = events_group, values_from = n)

batter_hits[is.na(batter_hits)] = 0

#update batter hits to include total number of batted balls and an in-play wOBA 
#using a calculation from online. **Eventually, we found there to be a `woba_value` 
#column in the dataset and did not end up using this. 
batter_hits <- batter_hits %>%
  mutate(total_balls_in_play = double + home_run + out + single + triple) %>%
  mutate(in_play_woba = (single*0.882 + double*1.242 + triple*1.586 + home_run*2.039)/total_balls_in_play) %>%
  filter(total_balls_in_play >=20)

# now find max EV and associated launch angle for each player 
max_exit_velos <- batter_all_2021 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2021, by = c("player_name", "max_EV" = "launch_speed")) %>%
  select(player_name, max_EV, launch_angle)

#join with batter hits in order to create woba graph, color by exit velocity.
# This graph ended up looking strange and not like we intended. We were able to figure
# out the proper code, which is located in the Final EDA script. 
batter_hits %>%
  left_join(max_exit_velos, by = "player_name") %>%
  select(player_name:max_EV, launch_angle) %>%
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), labels = c("<106", "106+", "112+"))) %>%
  ggplot(aes(x=launch_angle, y = in_play_woba, color = EV_group)) +
  geom_smooth(se = FALSE)+
  scale_x_continuous(limits = c(-40, 60))+
  scale_y_continuous(limits = c(0, 0.8))+
  theme_minimal()
  
  
