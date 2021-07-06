library(tidyverse)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")

#Here is the graph we want to layer on top of colored by OUTCOME -----------------------------
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

#Here is the graph we want to layer on top of colored by WOBA ------------

wOBA_by_LA_EV <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = woba_value)) +
  geom_point(alpha = 0.5) + 
  scale_x_continuous(n.breaks = 15, limits = c(-60, 90))+
  scale_color_gradient(low = "darkblue", high = "darkorange")+
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "wOBA Value by launch angle and exit velocity", color = "wOBA Value")+
  theme_minimal()

# Adding KDE --------------------------------------------------------------

#Aaron Judge
judge_stat_density <- batter_all_2019 %>%
  filter(player_name == "Judge, Aaron", description == "hit_into_play") #hit into play so we don't include
                                                    #things like fouls that still have LA and EV

judge_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = judge_stat_density, color = "white") + 
  labs(title = "Judge's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Judge, Aaron") %>%        #wOBA for the season = .381
  select(woba)

#David Fletcher
fletcher_stat_density <- batter_all_2019 %>%
  filter(player_name == "Fletcher, David", description == "hit_into_play") 

fletcher_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = fletcher_stat_density, color = "white") + 
  labs(title = "Fletcher's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Fletcher, David") %>%        #wOBA for the season = .318
  select(woba)

#insights from these two: Judge can hit at a much higher velocity and is sometimes doing so at that 
#higher angle. If he used that higher angle more maybe he could shift into that HR clump a little more.
#Fletcher hits at a much lower velocity BUT is located at that sweet spot for the angle that allows him
#to get that band of singles. AND GUESS WHAT??? THEIR WOBAs ARE SUPER SIMILAR!!

#Michael Brantley : very ground ball hitter
brantley_stat_density <- batter_all_2019 %>%
  filter(player_name == "Brantley, Michael", description == "hit_into_play") 

brantley_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = brantley_stat_density, color = "white") + 
  labs(title = "Brantley's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Brantley, Michael") %>%        #wOBA for the season = .367
  select(woba)

#Brantley has some high EVs (not the higest but still up there); maybe would be better if upped the LA?

#Mike Trout
trout_stat_density <- batter_all_2019 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 

trout_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = trout_stat_density, color = "white") + 
  labs(title = "Trout's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Trout, Mike") %>%        #wOBA for the season = .436
  select(woba)

#does a FANTASTIC job centering around that HR clump; odd little clump in the high LAs

#Brandon Belt: supposedly very unlucky with the shift
belt_stat_density <- batter_all_2019 %>%
  filter(player_name == "Belt, Brandon", description == "hit_into_play") 

belt_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = belt_stat_density, color = "white") + 
  labs(title = "Belt's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Belt, Brandon") %>%        #wOBA for the season = .319
  select(woba)

#Joey Gallo: supposedly hits in the air because when he hits ground balls its always into the shift
gallo_stat_density <- batter_all_2019 %>%
  filter(player_name == "Gallo, Joey", description == "hit_into_play") 

gallo_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = gallo_stat_density, color = "white") + 
  labs(title = "Trout's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(name == "Gallo, Joey") %>%        #wOBA for the season = .401
  select(woba)

#should def keep hitting at that higher launch angle
