#Goals: to brainstorm issues with the modeling sample hits framework

#wOBA vs. ATTACK angle by exit velos -------------------------------------------------------------------
#modification of https://twitter.com/ckurcon/status/1362028697860063232

#get all balls in play  
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019 <- batter_all_2019 %>%
  mutate(year = "2019")
in_play_2019 <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(attack_angle >0 & attack_angle <=25) %>%
  mutate(attack_angle = round(attack_angle,0)) %>%
  select(attack_angle, woba_value, player_name, launch_speed) 

#max EV per player and combine with balls in play (so there is a column categorizing the player who 
#hit the ball by their max EV in the season)
#RESULT: All three groups had a local maximum at a lower launch angle but once you increased launch angle
#the players with higher exit velocities had much better wOBAs
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
#CONCLUSIONS: interesting how they connect around 15/16 (which is where they connected when we did this
#graph with launch angles)
wOBA_angle_velo_graph_2 <- max_EV_2019_grouped2 %>%
  ggplot(aes(x=attack_angle, y = woba_value, color = EV_group)) +
  geom_smooth(method = "loess", span = .2, se = FALSE)+
  coord_cartesian(ylim=c(0,.65), xlim=c(-5,30)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_color_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x= "Launch Angle",
       y = "wOBAcon",
       title = "wOBA by Attack Angle")

#grouped into 3 EV 
#CONCLUSIONS: low power hitters generally meet in the same spot as this graph with launch angle, seem 
#to drop off after except for that peak. HERE THOUGH we see these middle hitters still incresing a bit
#which might be related to the "low" power hitters we are investigating (they are actually in this middle
#range)
wOBA_angle_velo_graph_3 <- max_EV_2019_grouped3 %>%
  ggplot(aes(x=attack_angle, y = woba_value, color = EV_group)) +
  geom_smooth(method = "loess", span = .2, se = FALSE)+
  coord_cartesian(ylim=c(0,.65), xlim=c(-5,30)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_color_manual(values = c("black", "orange", "blue"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x= "Launch Angle",
       y = "wOBAcon",
       title = "wOBA by Attack Angle")


