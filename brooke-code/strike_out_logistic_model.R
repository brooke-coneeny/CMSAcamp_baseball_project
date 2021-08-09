####################################################################################################################################
#This file intends to model strikeout probability based on different launch angles. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)

#Loading Data 
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

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

batter_all <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)


#Creating a column which describes the result of the pitch 
batter_all <- batter_all %>%
  mutate(description2 = case_when(
           description %in% c("automatic_ball", "ball", "blocked_ball", 
                              "called_strike", "hit_by_pitch", "pitchout") ~ "no_swing", 
           description %in% c("foul", "foul_tip") ~ "foul",
           description %in% c("foul_bunt", "missed_bunt", "bunt_foul_tip") ~ "bunting",
           description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score") ~ "fair_contact",
           description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swing_and_miss",
           TRUE ~ description)
  )

####################################################################################################################################

#Find each player's attack angle in each season
attack_angles <- batter_all %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#Find each player's number of plate appearances
plate_appearances <- batter_all %>%
  mutate(PA_id = paste(game_pk, at_bat_number, sep = "-")) %>%
  group_by(player_name) %>%
  summarise(n_pa = length(unique(PA_id)))

#Find number of balls hit into play by each batter 
balls_in_play <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name) %>%
  summarise(n_in_play = n())

#Find how many times they made fair contact, fouled, did not swing, swing and miss, and attempted bunting 
pitch_result <- batter_all_2019 %>%
  group_by(player_name, description2) %>%
  count() %>%
  pivot_wider(id_cols = player_name:description2, names_from = description2, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0))

#Find the rates at which people swing and miss at their given attack angle for the season 
swing_and_miss_rate <- pitch_result %>%
  left_join(plate_appearances, by = c("player_name")) %>%
  left_join(strikeouts, by = c("player_name")) %>%
  left_join(attack_angles, by = c("player_name")) %>%
  filter(n_pa >= 50) %>%
  mutate(
    swing_miss_rate = swing_and_miss / (fair_contact + foul + swing_and_miss)
  )

#What is the relationship between attack angle and swing & miss rate 
swing_and_miss_rate %>%
  ggplot(aes(x = attack_angle, y = swing_miss_rate)) +
  geom_point() +
  theme_bw()
  




