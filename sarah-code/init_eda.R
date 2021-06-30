library(baseballr)
library(tidyverse)

#baseballr package info: http://billpetti.github.io/baseballr/
#variable meanings: https://app.box.com/v/statcast-pitchfx-glossary-pett


# Load data ---------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")

#wOBA vs. launch angle by exit velos -------------------------------------------------------------------

#get all balls in play  
in_play_2019 <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  select(launch_angle, woba_value, player_name, launch_speed) 

#max EV per player and combine with balls in play (so there is a column categorizing the player who 
  #hit the ball by their max EV in the season)
max_EV_2019_grouped2 <- batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 200 & max_EV>0) %>%
  right_join(in_play_2019, by = c("player_name")) %>%
  mutate(EV_group = cut(max_EV, breaks = c(0, 110, 150), 
                        labels = c("<110 EV Hitters", "110+ EV Hitters")))

#group by 3 EVs instead
max_EV_2019_grouped3 <- batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 200 & max_EV>0) %>%
  right_join(in_play_2019, by = c("player_name")) %>%
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), 
                        labels = c("<106 EV Hitters", "106+ EV Hitters", "112+ EV Hitters")))

#graphed divided into 2 EV groups
wOBA_angle_velo_graph_2 <- max_EV_2019_grouped2 %>%
  ggplot(aes(x=launch_angle, y = woba_value, color = EV_group)) +
  geom_smooth(method = "loess", span = .2, se = FALSE)+
  coord_cartesian(ylim=c(0,1), xlim=c(-40,60)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_color_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x= "Launch Angle",
       y = "wOBA",
       title = "wOBA by Launch Angle")

#grouped into 3 EV 
wOBA_angle_velo_graph_3 <- max_EV_2019_grouped3 %>%
  ggplot(aes(x=launch_angle, y = woba_value, color = EV_group)) +
  geom_smooth(method = "loess", span = .2, se = FALSE)+
  coord_cartesian(ylim=c(0,1), xlim=c(-40,60)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_color_manual(values = c("black", "orange", "blue"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x= "Launch Angle",
       y = "wOBA",
       title = "wOBA by Launch Angle")
  
  

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
