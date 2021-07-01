#can put graphs that we want to present in here
library(tidyverse)
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
pal <- park_palette("Saguaro")
pal2 <- park_palette("Everglades")

# Loading Data ------------------------------------------------------------

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

#Examine different launch angles and out comes --------------------------------

launch_angle_outcomes <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  mutate(
    events_group = case_when(
      events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                    "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                    "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
      events == "single" ~ "single",
      events == "double" ~ "double", 
      events == "triple" ~ "triple", 
      events == "home_run" ~ "home run", 
      TRUE ~ "other"),
    launch_angle_group = cut(
      launch_angle, breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 
                                        30, 35, 40, 45, 50))) %>%
  filter(
    events_group %in% c("out", "single", "double", "triple", "home run"),
    !is.na(launch_angle_group)
  ) %>%
  select(events_group, launch_angle_group) %>%
  ggplot(mapping = aes(x = launch_angle_group, fill = events_group)) +
  geom_bar(position = "dodge") +
  labs(
    fill = "Outcome",
    title = "Hit Outcome Grouped by Launch Angle"
  ) +
  scale_fill_manual(values = pal)+
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
  )

#above graph shown proportionally:
# we faceted by fielder alignment and by whether the match up was lefty-on-lefty, 
# lefty-righty, etc and did not see any noticeable differences 
proportional_launchAngles <- batter_all_2019 %>%
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
  filter(events_group %in% c("out", "single", "double", "triple", "home run"), 
         launch_angle >= -20 & launch_angle <=50) %>%
  mutate(launch_angle_group = 
           cut(launch_angle, breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 
                                        30, 35, 40, 45, 50))) %>%
  filter(!is.na(launch_angle_group)) %>%
  ggplot(aes(x = launch_angle_group, fill = events_group))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = pal)+
  labs(x = "launch angle", y= "", title = "Hit outcome by launch angle", fill = "")+
  theme_minimal()

faceted_by_alignment <- batter_all_2019 %>%
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
  scale_fill_manual(values = pal)+
  facet_wrap(~if_fielding_alignment, ncol = 1)+
  labs(x = "launch angle", y="", title = "Hit outcome faceted by fielding alignment", fill = "")+
  theme_minimal()


#graph showing hit outcome by exit velocity and launch angle ------------------
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

#we then faceted the plot by pitch type to see if there were any differences (and there weren't)
outcome_faceted <- batter_all_2019 %>%
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
  scale_x_continuous(n.breaks = 15, limits = c(-60, 90))+
  scale_color_manual(values = rev(pal2))+
  facet_wrap(~pitch_group, ncol = 1)+
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "Hit outcome by launch angle and exit velocity by pitch type", color = "")+
  theme_minimal()

#hows does hit distance correlate with launch angle?--------------------------
LA_versus_hitDistance <- batter_all_2019 %>%
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
  filter(events_group %in% c("single", "double", "triple", "home run", "out")) %>%
  ggplot(aes(x=launch_angle, y=hit_distance_sc, color = events_group))+
  scale_color_manual(values = rev(pal2))+
  scale_x_continuous(n.breaks = 14, limits = c(-40, 100))+
  labs(x = "launch angle", y="hit distance", title = "Hit distance by launch angle", color = "")+
  geom_jitter()+
  theme_minimal()

#where the max EV hits landed colored by outcome
left_righty_maxEV_outcome <- batter_all_2019 %>%
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
    events == "home_run" ~ "home run",
    events == "triple" ~ "triple"
  ))%>%
  #coloring by event might allow us to see how the shift affects lefties
  ggplot(mapping = aes(x = hc_x, y = hc_y, color = events)) +
  geom_point() +
  facet_wrap(~stand) +
  scale_y_reverse()+
  scale_color_manual(values = rev(pal2))+
  labs(x = "horizontal fielded location", y = "vertical fielded location", 
       title ="Players max exit velocity hit location", color = "")+
  theme_minimal()









