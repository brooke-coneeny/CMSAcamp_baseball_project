library(baseballr)
library(dplyr)
library(tidyverse)
library(patchwork)

batter_all_2021 <- bind_rows(april, may2021, june_2021)

#looking at launch angle and exit velocity categorized by pitch type 
launch_and_ev_by_pitch <- june_2021 %>%
  filter(
    pitch_type %in% c("FF", "FT", "FA", "FO", "SI", "FS", "CU", "SL", "KC", "CH", "KN")
  ) %>%
  ggplot(mapping = aes(x = launch_angle, y = launch_speed)) +
  facet_wrap(~pitch_type) +
  geom_point()

#are lefty batters more likely to swing at a higher angle than right handed batters

     #how many of our batters were lefty or righty 
     num_lefty <- batter_all_2021 %>%
        group_by(player_name) %>%
        filter(stand == "L") %>%
        nrow()
      
      num_righty <- batter_all_2021 %>%
        group_by(player_name) %>%
        filter(stand == "R") %>%
        nrow()

      #separate distributions with only lefty or righty batters 
      left_batters <- batter_all_2021 %>%
        filter(stand == "L")
      
      right_batters <- batter_all_2021 %>%
        filter(stand == "R")

      #additional statistics
      avg_launch_angle = mean(batter_all_2021$launch_angle, na.rm = TRUE)
      total_batters <- nrow(batter_all_2021)

      #given that someone is a lefty, what is the prob that their bat angle is above the avg
      num_lefty_above <- left_batters %>%
        filter(launch_angle > avg_launch_angle) %>%
        nrow()
      
      num_righty_above <- right_batters %>%
        filter(launch_angle > avg_launch_angle) %>%
        nrow()
      
      percent_lefty_above <- num_lefty_above / num_lefty
      percent_righty_above <- num_righty_above / num_righty

      #plotting distribution of most common launch angle, vertical line for avg lefty and righty 
      mean_angle_left = mean(left_batters$launch_angle, na.rm = TRUE)
      mean_angle_right = mean(right_batters$launch_angle, na.rm = TRUE)
      
      angle_dist <- batter_all_2021 %>%
        ggplot(mapping = aes(x = launch_angle)) +
        geom_freqpoly() +
        geom_vline(xintercept = mean_angle_right, color = "red") +
        geom_vline(xintercept = mean_angle_left, color = "blue")
      
      
#out of the lefties and righties who hit at an above average launch angle, where do their hits land? 
batter_all_2021 %>%
  filter(
    launch_angle > avg_launch_angle,
    events %in% c("single", "double", "triple", "home_run")
    ) %>%
  ggplot(mapping = aes(x = hc_x, y = hc_y)) +
  geom_point() +
  facet_wrap(~stand) +
  scale_y_reverse()


  