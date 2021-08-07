#Creating a model to determine if a ball will be hit into play based on the swing plane and how the ball
#comes in

library(tidyverse)


# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")

batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
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

#create the main dataset  (ERIN)-----------------------------------------------------
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

#Creating a column with corresponding attack angle per player then filtering so that variables we (ME) ----
#use in the model do not have NA 
batter_all_2019 <- batter_all_2019 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type)) #MIGHT NEED TO ADD/CHANGE VARS DEPENDING ON WHAT WE USE IN THE MODEL

batter_all_1621 <- batter_all_1621 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_1621, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type))

#Adding a yes/no column for if a ball was hit into play
batter_all_1621 <- batter_all_1621 %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))


# Creating initial glm ----------------------------------------------------

init_logit <- glm(is_hit_into_play ~ attack_angle, #+ plate_z + release_speed, 
                  data = batter_all_1621, family = "binomial")
summary(init_logit)

pred_hit_outcomes <- ifelse(init_logit$fitted.values > 0.5, "hit", "no hit")

table("Predictions" = pred_hit_outcomes, "Observed" = batter_all_1621$is_hit_into_play)
max(init_logit$fitted.values)

#I think there are so few hit into play that when you have a a similar attack angle/plate height 
#combination the not hit into play lowers the prob of a hit

#Erin's logit model--------------------------------------------------------
#Create a glm for strikeOUT probability
k_probability <- glm(cbind(K, n_pa-K)~attack_angle, family="binomial", data=strikeout_eda)
summary(k_probability)

#Try a random forrest or XGBoost -----------------------------------------




