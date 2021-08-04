# PURPOSE: begin to explore the relationship between launch angle and strikeout rate. 
# We may recommmend a player increase their launch angle, but that means their bat
# is in the heart of the zone for less time. We would like to start to see what 
# this means for strikeout rate. 

# load data and libraries ---------------------------------------------------

library(tidyverse)
library(baseballr)

batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
'%!in%' <- Negate('%in%')

batter_all_2016 <- batter_all_2016 %>%
  mutate(year = "2016")
batter_all_2017 <- batter_all_2017 %>%
  mutate(year = "2017")
batter_all_2018 <- batter_all_2018 %>%
  mutate(year = "2018")
batter_all_2019 <- batter_all_2019 %>%
  mutate(year = "2019")
batter_all_2020 <- batter_all_2020 %>%
  mutate(year = "2020")
batter_all_2021 <- batter_all_2021 %>%
  mutate(year = "2021")

batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)

#create the main dataset -----------------------------------------------------
# I want a dataset that shows a player's attack angle (their median launch angle
# in their top 10% of exit velocities) for each season where they have at least 50
# batted balls, their average launch angle in each season, their number of plate 
# appearances and number of batted balls, number of strikeouts/strikeout percentage, 
# and wOBA. 

#find each player's attack angle in each season
attack_angles <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#find each player's launch angle in each season
launch_angles <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120)%>%
  summarize(avg_launch_angle = mean(launch_angle, na.rm = TRUE))

#find each player's number of plate appearances
plate_appearances <- batter_all_1621 %>%
  mutate(PA_id = paste(game_pk, at_bat_number, sep = "-")) %>%
  group_by(player_name, year) %>%
    summarise(n_pa = length(unique(PA_id)))

#find number of batted balls for each player
batted_balls <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  filter(description == "hit_into_play") %>%
  count() %>%
  rename(balls_in_play = n)

#find each player's number of strikeouts
strikeouts <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  filter(events %in% c("strikeout", "strikeout_double_play")) %>%
  count() %>%
  rename(K=n)

#find each player's wOBA each season
wOBAs <-  batter_all_1621 %>%
  group_by(player_name, year) %>%
  summarize(woba = mean(woba_value, na.rm = TRUE))

#create joined data set
strikeout_eda <- plate_appearances %>%
  left_join(strikeouts, by = c("player_name", "year")) %>%
  left_join(attack_angles, by=c("player_name", "year")) %>%
  left_join(launch_angles, by=c("player_name", "year")) %>%
  left_join(wOBAs, by = c("player_name", "year")) %>%
  left_join(batted_balls, by = c("player_name", "year")) %>%
  mutate(k_percent = K/n_pa) %>%
  filter(balls_in_play >=50)

# Begin by creating a scatterplot of average launch angle versus strikeout percentage and 
# attack angle versus strikeout percentage. Each point represents one player's season 
# where they had at least 50 batted balls. We see a stronger relationship between 
#attack angle and strikeout percentage (correlation ~0.313). This emphasizes why 
# we should be using attack angle - it better correlates with what a player is doing 
# when they don't make contact. 
strikeout_eda %>%
  ggplot(aes(x=avg_launch_angle, y=k_percent)) +
  geom_point()+
  geom_smooth()+
  labs(x = "launch angle", y="K%")+
  theme_minimal()

strikeout_eda %>%
  ggplot(aes(x=attack_angle, y=k_percent)) +
  geom_point()+
  geom_smooth()+
  labs(x = "attack angle", y="K%")+
  theme_minimal()

# Find the average launch angle in the MLB each year since 2016. Notice that the 
# average launch angle increased the most between 2016 and 2017, indicating the 
# beginning of the launch angle revolution. 
batter_all_1621 %>%
  group_by(year) %>%
  summarize(mean(launch_angle, na.rm = TRUE))

#Start to investigate a little bit about what is going on between 2016 and 2017. 
#Create dataset called that shows how much players changed their attack angle 
#between 2016 and 2017. 
AA_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = attack_angle) %>%
  summarize(attack_angle_change = `2017`-`2016`) %>%
  filter(!is.na(attack_angle_change))

#Next, find how the change in each player's wOBA between 2016 and 2017. 
woba_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = woba) %>%
  summarize(woba_change = `2017`-`2016`) %>%
  filter(!is.na(woba_change))

#Find the change in their strikeout percentage between 2016 and 2017. 
k_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = k_percent) %>%
  summarize(k_change = `2017`-`2016`) %>%
  filter(!is.na(k_change))

# Summarize each player's attack angle in 2017 specifically. 
attack_angles_2017 <- strikeout_eda %>%
  filter(year == "2017") %>%
  group_by(player_name) %>%
  summarize(attack_angle)

#Finally, create the overall change dataset which filters for players with at least
# 50 batted balls and displays each player's strikeout percentage change, attack 
# angle change, wOBA change, and attack angle in 2017. Create groups of attack angle - 
#0-10, 10-15, 15-20, and 20-35. 
overall_change_1617 <- AA_change_1617 %>%
  full_join(woba_change_1617, by = "player_name") %>%
  full_join(k_change_1617, by="player_name") %>%
  left_join(attack_angles_2017, by = "player_name") %>%
  filter(attack_angle >=0) %>%
  mutate(AA_group = cut(attack_angle, breaks = c(0, 10, 15, 20, 35), 
                        labels = c("0-10", "10-15", "15-20", "20-35")))

#Plot the change in strikeout percentage, with change in attack angle on the x 
#axis. Facet into players that ended up hitting with an attack angle of 0-10, 10-15, 
# 15-20, and 20-35 in 2017. We can see that for players that finished with a 15-20 
# degree attack angle in 2017 that increased their attack angle significantly 
# from the prior season that their strikeout percentage tended to increase. 
overall_change_1617 %>%
  filter(!is.na(AA_group) & attack_angle_change <=15) %>%
  ggplot(aes(x=attack_angle_change, y=k_change))+
  geom_point()+
  geom_smooth()+
  labs(x="2017-2016 attack angle", y="2017 K% - 2016%")+
  facet_wrap(~AA_group)+
  theme_minimal()

#Additionally, create a plot simply of attack angle in 2016 versus 2017. 
# From this, we see that players with a low attack angle in 2016 (<5-10 degrees) 
# generally tended to increase that angle in 2017. 
strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = attack_angle) %>%
  ggplot(aes(x=`2016`, y=`2017`))+
  geom_point()+
  labs(x="2016 attack angle", y="2017 attack angle")+
  geom_abline(a=0, b=1)+
  theme_minimal()

# New graph topic -------------------------------------------------------
#Now, let's look at how changing launch angle tends to be correlated with K%

#Filter for players that have had at least 50 batted balls in at least 4 out of the 
# past 6 seasons. 
regular_players <- strikeout_eda %>%
  group_by(player_name) %>%
  count() %>%
  filter(n >= 4) 

#Find the difference between each player's min and max avg launch angle in those 6 years
LA_range_players <- strikeout_eda %>%
  group_by(player_name) %>%
  summarize(launch_range = max(avg_launch_angle) - min(avg_launch_angle))

LA_range_regular_players <- regular_players %>%
  left_join(LA_range_players, by = "player_name")

#Find difference between each player's min and max attack angle in those 6 years 
AA_range_players <- strikeout_eda %>%
  group_by(player_name) %>%
  summarize(attack_range = max(attack_angle) - min(attack_angle))

AA_range_regular_players <- regular_players %>%
  left_join(AA_range_players, by = "player_name")

#Create plot that shows the change in maximum attack angle - min attack angle on
# the x axis and the change in strikeout percentage at that max - min attack angle 
# on the y axis. We see the line tends to average around +2-3%. This indicates that on
# average players tended to have a 2-3% higher strikeout percentage in their season with
# their maximum attack angle versus the season with their minimum. One thing this graph 
# does not account for is the number of years between their minimum and maximum attack 
#angle season. For example, a player may have increased their attack angle over a period of 
# 4 years and had very high strikeout rates during that transition period but once 
# they finally settled down into their maximum attack angle they were able to calm that 
# down. 
strikeout_eda %>%
  group_by(player_name) %>%
  mutate(n_seasons = n()) %>%
  ungroup() %>%
  filter(n_seasons >= 4) %>%
  group_by(player_name) %>%
  summarize(attack_range = max(attack_angle) - min(attack_angle), 
            k_range = k_percent[which.max(attack_angle)] - k_percent[which.min(attack_angle)]) %>%
  ggplot(aes(x=attack_range, y=k_range))+
  labs(x="Max - min attack angle", y="K% at max - min attack angle")+
  geom_point()+
  geom_smooth()+
  theme_minimal()

# Additional future notes/ideas  -----------------------------------------

#Create a glm for strikeout probability
k_probability <- glm(cbind(K, n_pa-K)~attack_angle, family="binomial", data=strikeout_eda)
summary(k_probability)



