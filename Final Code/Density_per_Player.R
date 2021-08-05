####################################################################################################################################
#This file explores looking at a specific player's density plot for balls in play and seeing it overlays with a woba scatter plot
#for all batted balls
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)

#Loading Data
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")

####################################################################################################################################

#This plots visualizes what combinations of launch angle and exit velocity lead to specific types of hits
outcome_by_LA_EV <- batter_all_2019hp %>%
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
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "Hit outcome by launch angle and exit velocity", color = "")+
  theme_minimal()

#This plot visualizes the woba of all balls hit into play, shows us which combinations of launch angle and exit velocity lead
#to the greast woba values 

wOBA_by_LA_EV <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  ggplot(aes(x=launch_angle, y = launch_speed, color = woba_value)) +
  geom_point(alpha = 0.5) + 
  scale_x_continuous(n.breaks = 15, limits = c(-60, 90))+
  scale_color_gradient(low = "darkblue", high = "darkorange")+
  labs(x = "launch angle", y = "exit velocity (mph)", 
       title = "wOBA Value by launch angle and exit velocity", color = "wOBA Value")+
  theme_minimal()

#These are the same graphs but colored by different values (wOBA is just a different representation of outcome)

####################################################################################################################################

#The next step was to place the individual densities of specific players hits ontop of the wOBA-by_LA_EV plot to determine
#how we can shift their density plot in order to reach a higher wOBA

#Mike Trout's main density aligns almost perfectly with the homerun/high woba clump therefore we would make no recommendations
trout_stat_density <- batter_all_2019 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 

trout_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = trout_stat_density, color = "white") + 
  labs(title = "Trout's Density Over wOBA Values for LA and EV")

#Jason Heyward's main density aligns to the left of the homerun/high woba clump
#However because he is able to reach the high exit velocities, the visualization leads us to suggest that if he keeps 
#his EV constant but increases his LA, he can center  his density around the homerun clump
heyward_stat_density <- batter_all_2019 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 

heyward_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = heyward_stat_density, color = "white") + 
  labs(title = "Heyward's Density Over wOBA Values for LA and EV")



