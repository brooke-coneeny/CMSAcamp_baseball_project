library(tidyverse)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")

#Launch Angle vs Launch Speed colored by event groups
outcome_by_LA_EV <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  mutate(events_group = case_when(
    events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                  "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                  "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
    events == "single" ~ "single",
    events == "double" ~ "double", 
    events == "triple" ~ "triple", 
    events == "home_run" ~ "home run", 
    TRUE ~ "other")) %>%
  filter(events_group %in% c("out", "single", "double", "triple", "home run")) %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = events_group)) +
  geom_point(alpha = 0.5) + 
  scale_x_continuous(n.breaks = 15, limits = c(-60, 90))+
  scale_color_manual(values = rev(pal2))+
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "Hit outcome by launch angle and exit velocity", color = "")+
  theme_minimal()

#Launch Angle vs Launch Speed colored by wOBA
woba_by_LA_EV <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = woba_value)) +
  geom_point(alpha = 0.5) + 
  scale_x_continuous(n.breaks = 15, limits = c(-60, 90)) + 
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "wOBA by launch angle and exit velocity", color = "") +
  theme_minimal()

#add density later
swanson_data <- batter_all_2019 %>%
  filter(player_name == "Swanson, Dansby",
         description == "hit_into_play")

woba_by_LA_EV_Density <- woba_by_LA_EV + 
  stat_density_2d(data = swanson_data, color = "white")
  
