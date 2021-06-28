library(baseballr)
library(tidyverse)
library(readr)

player_wobas <- read_csv("woba_2021data.csv")

### Loading data 
june_week01 <- scrape_statcast_savant_batter_all(start_date = "2021-06-01", 
                                                 end_date = "2021-06-07")
june_week02 <- scrape_statcast_savant_batter_all(start_date = "2021-06-08", 
                                                 end_date = "2021-06-14")
june_week03 <- scrape_statcast_savant_batter_all(start_date = "2021-06-15", 
                                                 end_date = "2021-06-21")
june_week04 <- scrape_statcast_savant_batter_all(start_date = "2021-06-22", 
                                                 end_date = "2021-06-23")
june_2021 <- bind_rows(june_week01, june_week02, june_week03, june_week04)
april_week1 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-01", 
  end_date = "2021-04-07")
april_week2 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-08", 
  end_date = "2021-04-14")
april_week3 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-15", 
  end_date = "2021-04-21")
april_week4 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-22", 
  end_date = "2021-04-28")
april_2930 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-29", 
  end_date = "2021-04-30")
april <- bind_rows(april_week1, april_week2, april_week3, april_week4, april_2930)
may_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-05-01",
                                               end_date = "2021-05-07")
may_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-05-08",
                                               end_date = "2021-05-14")
may_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-05-15",
                                               end_date = "2021-05-22")
may_week4 <- scrape_statcast_savant_batter_all(start_date = "2021-05-23",
                                               end_date = "2021-05-31")
may2021 <- bind_rows(may_week1, may_week2, may_week3, may_week4)
batter_all_2021 <- bind_rows(april, may2021, june_2021)

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
