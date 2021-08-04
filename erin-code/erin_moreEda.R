## PURPOSE: to continue to explore the relationship between launch angle, exit velocity, 
# and weighted on base percentage. 

# Load libraries and data --------------------------------------------------
library(baseballr)
library(tidyverse)
library(readr)

player_wobas <- read_csv("public_data/woba_2021data.csv")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

# Create scatterplot of launch angle vs. exit velocity. Color by the outcome
# of the batted ball. This is where we first begin to see the J-shape of singles. 
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

#Start to investigate the relationship of whether or not players with a greater 
# standard deviation of their launch angle tend to have higher or lower wOBAs. 
# We see that there does not appear to be a relationship. 
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

# Does hit outcome change holding everything constant but pitch type? Break pitch 
# types into fastballs, offspeed pitches, and breaking balls. Then create the hit outcome
# scatterplot like above and facet by pitch type. We do not seem to see a difference.
# This is likely because we are conditioning on the ball being hit. 
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

#Do players hit the ball further at certain launch angles? Create a scatterplot 
# with launch angle on the x axis and hit distance on the y. Color by hit outcome. 
# Pretty much as expected, this is a parabola with balls being hit furthest at a 
# launch angle of around 25-30 (homeruns). 
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
  
# From the graph above we saw that homeruns really only tend to occur in 20ish degree 
# span of launch angle from around 20-40. Let's break launch angle into 10 degree 
# categories from -90 to 90 and have and show the distribution of hit outcomes in 
# each category. 
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

# We'd never recommend a launch angle on the ends of this distribution above, so lets 
# make the bound -20 and 50 degrees and break the launch angle into groups of 5 degrees. 
# Something to notice is how the 10-15 degree launch angle appears to be about 70% base 
# hits, far above the major league average in 2021 of around .250! The most homeruns 
# come with a launch angle around 30-35 degrees. 
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

#Is there any difference in this distribution when we facet by a lefty hitter vs 
#righty pitcher, lefty hitter versus lefty pitcher, etc? Again, there do not appear 
# to be any significant differences based on this graph. 
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

#Does hit outcome change holding everything constant but fielding alignment? 
# It does not appear so. 
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
  filter(if_fielding_alignment %in% c("Infield shift", "Standard", "Strategic")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home_run"), 
         launch_angle >= -20 & launch_angle <=50) %>%
  mutate(launch_angle_group = 
           cut(launch_angle, breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 
                                        30, 35, 40, 45, 50))) %>%
  filter(!is.na(launch_angle_group)) %>%
  ggplot(aes(x = launch_angle_group, fill = events_group))+
  geom_bar(position = "fill")+
  facet_wrap(~if_fielding_alignment, ncol = 1)+
  theme_minimal()
