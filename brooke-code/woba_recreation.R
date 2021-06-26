#Recreating wOBA and Launch angle comparison plot 

#need to find each hitters max EV and the corresponding launch angle and their wOBA for the season and 
#each data point is a person

library(tidyverse)
library(Lahman)
library(baseballr)

batter_all_2021 <- bind_rows(april, may2021, june_2021)
View(batter_all_2021)


batter_hits <- batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  #categorize events by either an out or hit since balls in play which are outs have no weight 
  mutate(
    events_group = case_when(
      events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                    "fielders_choice_out", "force_out", "field_error", 
                    "fielders_choice", "triple_play") ~ "out",
      events %in% c("sac_fly_double_play", "sac_fly") ~ "SF",
      events == "single" ~ "single",
      events == "double" ~ "double", 
      events == "triple" ~ "triple", 
      events == "home_run" ~ "home_run", 
      TRUE ~ "other")
  ) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run", "SF")) %>%
  group_by(player_name) %>%
  count(events_group) %>%
  pivot_wider(id_cols = player_name, names_from = events_group, values_from = n)

#calculate woba for each batter
batter_woba <- batter_hits %>%
  mutate(total_at_bats = single + double + triple + home_run + out) %>%
  mutate(woba_balls_hit = (0.833*single + 1.253*double + 1.587*triple + 2.041*home_run)/(total_at_bats + SF)) %>%
  select(player_name, woba_balls_hit)



  
  