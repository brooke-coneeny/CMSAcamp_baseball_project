library(tidyverse)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
expected_stats_2019 <- read_csv("public_data/expected_stats2019.csv")

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

# Player Density plots ----------------------------------------------------

#Cody Bellinger---------------
bellinger_stat_density <- batter_all_2019 %>%
  filter(player_name == "Bellinger, Cody", description == "hit_into_play") 

bellinger_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = bellinger_stat_density, color = "white") + 
  labs(title = "Bellinger's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(first_name == "Cody" & last_name == "Bellinger") %>%        #wOBA for the season = .381
  select(woba)

#Bellinger insights: Bellinger typically hit at a pretty high exit velocity of right around 100. This density
#plot shows his 2019 season in which he was NL MVP so we probably don't want him changing much about his swing :)
#His highest density area shows a launch angle and exit velocity right between the strip of singles and nearing the minimum 
#exit velocity typically required for a homerun, which is nice because it means he can be successful both 
#with singles and extra base hits. Bellinger hit the 4th most homeruns in 2019 at 47 and had the 24 highest batting average. 
# He had the third highest expected woba at 0.424

#Jason Heyward -------------------
heyward_stat_density <- batter_all_2019 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 

heyward_density_2019 <- wOBA_by_LA_EV + stat_density2d(data = heyward_stat_density, color = "white") + 
  labs(title = "Heyward's Density Over wOBA Values for LA and EV")

expected_stats_2019 %>%
  filter(first_name == "Jason" & last_name == "Heyward") %>%        #wOBA for the season = .381
  select(woba)

#From this graph we see that Jason Heyward may want to consider increasing his launch angle. At his highest
#density area, which is around 5 degrees, Heyward is just below of the launch angle where we see a very high
#likelihood of singles. By increasing his launch angle to around 10-15 degrees Heyward could potentially raise 
#his batting average a considerable amount. If he can adopt this launch angle more often and also steers away from another 
#fairly common -20 degree launch angle that he has, he may see a lot more singles and a lot less groundouts to second base. 

