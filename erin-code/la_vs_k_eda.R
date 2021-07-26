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

#find each player's number of strikeouts per 10 plate appearances
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
  #rename(PA = n.x, K = n.y) %>%
  mutate(k_percent = K/n_pa) %>%
  filter(balls_in_play >=50)

#glm for strikeout probability
k_probability <- glm(cbind(K, n_pa-K)~attack_angle, family="binomial", data=strikeout_eda)
summary(k_probability)

#change in attack angle with change in K rate and woba relationship

# create plot of launch angle versus strikeout percentage and plot of attack angle versus 
#strikeout percentage
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

# find difference in strikeout percentage and wOBA in 2016 versus 2017 for hitters 
#that changed their launch angle ---> 2016 avg launch angle was 16.2 and 2017 
batter_all_1621 %>%
  group_by(year) %>%
  summarize(mean(launch_angle, na.rm = TRUE))

AA_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = attack_angle) %>%
  summarize(attack_angle_change = `2017`-`2016`) %>%
  filter(!is.na(attack_angle_change))

woba_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = woba) %>%
  summarize(woba_change = `2017`-`2016`) %>%
  filter(!is.na(woba_change))

k_change_1617 <- strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = k_percent) %>%
  summarize(k_change = `2017`-`2016`) %>%
  filter(!is.na(k_change))

attack_angles_2017 <- strikeout_eda %>%
  filter(year == "2017") %>%
  group_by(player_name) %>%
  summarize(attack_angle)

overall_change_1617 <- AA_change_1617 %>%
  full_join(woba_change_1617, by = "player_name") %>%
  full_join(k_change_1617, by="player_name") %>%
  left_join(attack_angles_2017, by = "player_name") %>%
  filter(attack_angle >=0) %>%
  mutate(AA_group = cut(attack_angle, breaks = c(0, 10, 15, 20, 35), 
                        labels = c("0-10", "10-15", "15-20", "20-35")))
  

overall_change_1617 %>%
  filter(!is.na(AA_group) & attack_angle_change <=15) %>%
  ggplot(aes(x=attack_angle_change, y=k_change))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~AA_group)+
  theme_minimal()

strikeout_eda %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% c(2016,2017)) %>%
  pivot_wider(id_cols = player_name, names_from =year, values_from = attack_angle) %>%
  ggplot(aes(x=`2016`, y=`2017`))+
  geom_point()+
  geom_abline(a=0, b=1)
  

  



#finding players that have changed their average launch angle --------------

#find players that have at least 100 PAs in at least 4 of the last 5 seasons
regular_players <- strikeout_eda %>%
  group_by(player_name) %>%
  count() %>%
  filter(n >= 4) 

#find the difference between each player's min and max avg launch angle in those 5 years
LA_range_players <- strikeout_eda %>%
  group_by(player_name) %>%
  summarize(launch_range = max(avg_launch_angle) - min(avg_launch_angle))

LA_range_regular_players <- regular_players %>%
  left_join(LA_range_players, by = "player_name")

#find difference between each player's min and max attack angle in those 5 years 
AA_range_players <- strikeout_eda %>%
  group_by(player_name) %>%
  summarize(attack_range = max(attack_angle) - min(attack_angle))

AA_range_regular_players <- regular_players %>%
  left_join(AA_range_players, by = "player_name")
  
strikeout_eda %>%
  group_by(player_name) %>%
  mutate(n_seasons = n()) %>%
  ungroup() %>%
  filter(n_seasons >= 4) %>%
  group_by(player_name) %>%
  summarize(attack_range = max(attack_angle) - min(attack_angle), 
            k_range = k_percent[which.max(attack_angle)] - k_percent[which.min(attack_angle)]) %>%
  ggplot(aes(x=attack_range, y=k_range))+
  geom_point()+
  geom_smooth()
#could there still be relationship of k rate in back to back seasons?
#condition on players that increased their launch angle

