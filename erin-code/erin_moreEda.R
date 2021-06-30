library(baseballr)
library(tidyverse)
library(readr)

player_wobas <- read_csv("public_data/woba_2021data.csv")

### Loading data 
batter_all_2021 <- read_rds("data/all2021data.rds")

#distribution of event by launch angle and exit velocity -------------------
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run")) %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = events_group)) +
  geom_point(alpha = 0.5) + 
  theme_minimal()

# are there certain players that vary their launch angle more than others and does this impact woba?------------

player_abs <- batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name) %>%
  count() %>%
  filter(n > 100)

launch_variation <- batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name) %>%
  summarize(sd_launch_angle = sd(launch_angle, na.rm = TRUE)) 

launch_spread <- player_abs %>%
  left_join(launch_variation, by = "player_name")

launch_spread %>%
  inner_join(player_wobas, by = "player_name") %>%
  ggplot(aes(sd_launch_angle, woba)) +
    geom_point()+
  theme_minimal()

#facet outcome plot by pitch type -----------------------------------------------------

batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run")) %>%
  mutate(pitch_group = case_when(
    pitch_name %in% c("4-Seam Fastball", "Fastball", "Cutter", "Sinker") ~ "Fastball", 
    pitch_name %in% c("Changeup", "Split-Finger", "Screwball") ~ "Offspeed", 
    pitch_name %in% c("Slider", "Knuckle Curve", "Curveball", "Knuckle Curve", 
                      "Knuckleball", "Eephus") ~ "Breaking", 
    TRUE ~ pitch_name
  )) %>%
  filter(!is.na(pitch_group)) %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = events_group)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~pitch_group, ncol = 1)+
  theme_minimal()

#hows does hit distance correlate with launch angle?----------
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  ggplot(aes(x=launch_angle, y=hit_distance_sc, color = events_group))+
  geom_jitter()+
  theme_minimal()
  
#should launch angle change depending on fielder alignment?

#main graph (no alignment specifications)
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run")) %>%
    mutate(launch_angle_group = 
             cut(launch_angle, breaks = c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 
                                           0, 10, 20, 30, 40, 50, 60, 70, 80, 90))) %>%
    ggplot(aes(x = launch_angle_group, fill = events_group))+
      geom_bar(position = "fill")+
  theme_minimal()

#filter to make bar graph with launch angle between -20 and 50
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run"), 
         launch_angle >= -20 & launch_angle <=50) %>%
  mutate(launch_angle_group = 
           cut(launch_angle, breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 
                                        30, 35, 40, 45, 50))) %>%
  filter(!is.na(launch_angle_group)) %>%
  ggplot(aes(x = launch_angle_group, fill = events_group))+
  geom_bar(position = "fill")+
  theme_minimal()

#facet by lefty vs righty
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run"), 
         launch_angle >= -20 & launch_angle <=50) %>%
  mutate(launch_angle_group = 
           cut(launch_angle, breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 
                                        30, 35, 40, 45, 50))) %>%
  filter(!is.na(launch_angle_group)) %>%
  ggplot(aes(x = launch_angle_group, fill = events_group))+
  geom_bar(position = "fill")+
  facet_wrap(~p_throws + stand, ncol = 2)+
  theme_minimal()

#facet by alignment...doesn't appear so
batter_all_2021 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home_run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run")) %>%
  filter(if_fielding_alignment %in% c("Infield shift", "Standard", "Strategic")) %>%
  mutate(launch_angle_group = 
           cut(launch_angle, breaks = c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 
                                        0, 10, 20, 30, 40, 50, 60, 70, 80, 90))) %>%
  ggplot(aes(x = launch_angle_group, fill = events_group))+
  geom_bar(position = "fill")+
  facet_wrap(~if_fielding_alignment, ncol = 1)+
  theme_minimal()
