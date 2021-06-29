library(baseballr)
library(tidyverse)

#baseballr package info: http://billpetti.github.io/baseballr/
#variable meanings: https://app.box.com/v/statcast-pitchfx-glossary-pett


# Load data ---------------------------------------------------------------

batter_all_2019 <- read_rds("data/all2019data.rds")

#wOBA vs. launch angle by exit velos -------------------------------------------------------------------

#need to find each hitters max EV and the corresponding launch angle and their wOBA for the season and 
#each data point is a person

wOBA_angle_velo_graph <- batter_all_2019 %>%
#this gets us each player's max EV and the associated angle
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed")) %>%
#this brings in expected stats data
  left_join(expected_stats_2019, by = c("player_name" = "name")) %>%
  select(player_name, max_EV, launch_angle, woba) %>%
  filter(!is.na(woba)) %>%
#then we prepare the graph
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), labels = c("<106", "106+", "112+"))) %>%
  ggplot(aes(x=launch_angle, y = woba, color = EV_group)) +
  geom_smooth(se = FALSE)+
  #geom_line()+
  scale_x_continuous(limits = c(-40, 60))+
  scale_y_continuous(limits = c(0, 1))+
  theme_bw()+
  labs(x= "Launch Angle",
       y = "wOBA")
  
  

# Do righties vs. lefties have a distinct ev/angle combo?  --------------------------------------------

batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed")) %>%
  select(player_name, max_EV, launch_angle, stand)%>%
  ggplot(aes(x=launch_angle, y=max_EV, color = stand))+
  geom_point(alpha = .4)+
  theme_bw()+
  labs(x = "Launch Angle",
       y = "Max Exit Velocity")


# where the max ev hits landed -----------------------------------------------------------------------

batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed"))%>%
  select(player_name, hc_x, hc_y, stand, events)%>%
  filter(!is.na(stand), !is.na(events))%>%
  mutate(events = case_when(
#for the purposes of this, field error and fielders choice are outs because in normal play that's what 
    #they would have been
    events %in% c("double_play", "field_out", "grounded_into_double_play", "sac_bunt", 
                  "sac_fly", "field_error", "fielders_choice") ~ "out",
    events %in% c("single", "force_out") ~ "single",
    events == "double" ~ "double",
    events == "home_run" ~ "home_run",
    events == "triple" ~ "triple"
  ))%>%
#coloring by event might allow us to see how the shift affects lefties
  ggplot(mapping = aes(x = hc_x, y = hc_y, color = events)) +
  geom_point() +
  facet_wrap(~stand) +
  scale_y_reverse()
