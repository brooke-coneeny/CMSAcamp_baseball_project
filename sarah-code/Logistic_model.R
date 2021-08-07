#Creating a model to determine if a ball will be hit into play based on the swing plane and how the ball
#comes in

library(tidyverse)
library(mgcv)


# Loading Data ------------------------------------------------------------

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

#GET THE STRIKE PERCENTAGE FOR EACH PLAYER FOR EACH YEAR

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

#find each player's number of strikes
strikes <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  filter(description %!in% c("hit_into_play", "ball", "hit_by_pitch")) %>%
  count() %>%
  rename(strikes=n)     
  

#find each player's wOBA each season
wOBAs <-  batter_all_1621 %>%
  group_by(player_name, year) %>%
  summarize(woba = mean(woba_value, na.rm = TRUE))

#Get total number of pitches seen by a player during a year
total_pitches <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  count() %>%
  rename(total_pitches = n)

#create joined data set
strike_modeling <- plate_appearances %>%
  left_join(strikes, by = c("player_name", "year")) %>%
  left_join(attack_angles, by=c("player_name", "year")) %>%
  left_join(launch_angles, by=c("player_name", "year")) %>%
  left_join(wOBAs, by = c("player_name", "year")) %>%
  left_join(batted_balls, by = c("player_name", "year")) %>%
  left_join(total_pitches, by = c("player_name", "year")) %>%
  mutate(strike_percent = strikes/total_pitches) %>%
  filter(attack_angle <= 30 & attack_angle >=0)

#Creating a column with corresponding attack angle per player then filtering so that the variables we 
#use in the model do not have NA 
batter_all_2019_logit <- batter_all_2019 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type)) #MIGHT NEED TO ADD/CHANGE VARS DEPENDING ON WHAT WE USE IN THE MODEL

batter_all_1621_logit <- batter_all_1621 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_1621, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type))

#Adding a yes/no column for if a ball was hit into play
batter_all_2019_logit <- batter_all_2019_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))
batter_all_1621_logit <- batter_all_1621_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))

#read in the stats and add the player's attack angle
stats_2019 <- read_csv("public_data/expected_stats2019.csv")
stats_2019 <- stats_2019 %>% mutate(player_name = paste(last_name, first_name, sep=", "))
name_and_attack <- batter_all_2019_logit %>% select(player_name, attack_angle)

avg_and_attack_modeling <- stats_2019 %>% left_join(name_and_attack, by = c("player_name")) %>% distinct()

# Creating initial glm ----------------------------------------------------

init_logit <- glm(is_hit_into_play ~ attack_angle, #+ plate_z + release_speed, 
                  data = batter_all_2019_logit, family = "binomial")
summary(init_logit)

#pred_hit_outcomes <- ifelse(init_logit$fitted.values > 0.5, "hit", "no hit")

#table("Predictions" = pred_hit_outcomes, "Observed" = batter_all_2019_logit$is_hit_into_play)
max(init_logit$fitted.values)

#I think there are so few hit into play that when you have a a similar attack angle/plate height 
#combination the not hit into play lowers the prob of a hit

#could we use the low probabilities as the prob of selection and then select the number of balls hit into
#play based on a modeled batting average for that launch angle?????

avg_and_attack_modeling %>%
  ggplot(aes(x=attack_angle, y=ba))+
  geom_line() + geom_smooth() 

init_avg_model <- gam(ba ~ s(attack_angle, k=125), data=avg_and_attack_modeling)
summary(init_avg_model)
gam.check(init_avg_model, k.sample = 50000, k.rep = 250)

# Model strike percentage  ---------------------------------------------
strike_modeling %>%
  ggplot(aes(x=attack_angle, y=strike_percent))+
  geom_line() + geom_smooth() #practically no relationship...so the model is useless

strike_percent_init_model <- lm(strike_percent ~ attack_angle, data=strike_modeling)
summary(strike_percent_init_model)



