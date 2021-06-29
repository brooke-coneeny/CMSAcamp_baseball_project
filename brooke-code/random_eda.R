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
      
#-----------------------------------------------------------------------------------------------------------------
#out of the lefties and righties who hit below average launch angle, where do their hits land? 
below_avg_field_dist <- batter_all_2021 %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run",
    TRUE ~ "other")) %>%
  filter(
    launch_angle < avg_launch_angle,
    !is.na(hc_x),
    !is.na(hc_y),
    events_group %in% c("single", "double", "triple", "home_run", "out")
  ) %>%
  ggplot(mapping = aes(x = hc_x, y = hc_y, color = events_group)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~stand) +
  scale_y_reverse() +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
  ) +
  labs(
    title = "Distribution of Balls in Play",
    subtitle = "Filtered by players who hit below avg launch angle"
  ) +
  scale_color_discrete(name = "Outcome")
  
#------------------------------------------------------------------------------------------------------------------------
#what is the distribution of hit type for lefties and righties who hit below average launch angle?
below_avg_hit_dist <- batter_all_2021 %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run",
    TRUE ~ "other")) %>%
  filter(
    launch_angle < avg_launch_angle,
    events_group %in% c("out", "single", "double", "triple", "home_run")
  ) %>%
  ggplot(mapping = aes(x = events_group)) +
  geom_bar() +
  facet_wrap(~stand) +
  theme(
    axis.title.x = element_blank()
  ) +
  labs(
    title = "Distribution of Outcomes",
    subtitle = "Filtered by players who hit below avg launch angle",
    y = "Count"
  ) +
  scale_x_discrete(labels=c("double" = "Double", "home_run" = "Home Run",
                            "out" = "Out", "single" = "Single", "triple" = "Triple"))


#Examining the zones and launch angle
batter_all_2021 %>%
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
  ggplot(mapping = aes(x = zone, fill = events_group)) +
  geom_bar(position = "fill")
  

  