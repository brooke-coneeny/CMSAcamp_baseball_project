####################################################################################################################################
#This file intends to model strikeout probability based on different launch angles. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
'%!in%' <- Negate('%in%')

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

batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1621 <- batter_all_1621 %>%
  mutate(description2 = case_when(description %in% c("ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout") ~ "other", 
                                  description %in% c("foul", "foul_tip") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swinging_strike",
                                  TRUE ~ description))
####################################################################################################################################

#CREATE THE MAIN DATASET 

# I want a dataset that shows a player's attack angle (their median launch angle) in their top 10% of 
#exit velocities) for each season where they have at least 50 batted balls, their average launch angle 
#in each season, their number of plate appearances and number of batted balls, number of 
#strikeouts/strikeout percentage, and wOBA. 

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

#find each player's rate of swing and misses out of all balls they swing at
swing_and_miss <- batter_all_1621 %>%
  group_by(player_name, year, description2) %>%
  count() %>%
  pivot_wider(id_cols = player_name:year, names_from = description2, values_from = n) %>%
  mutate(contact = hit_into_play + foul) %>%
  select(player_name, year, swinging_strike, contact, swinging_strike)

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
  left_join(swing_and_miss, by = c("player_name", "year")) %>%
  mutate(k_percent = K/n_pa) %>%
  filter(balls_in_play >=50)

####################################################################################################################################

#LOGISTIC MODEL (GENERAL) FOR ATTACK ANGLE VS SWING & MISS RATE

#Create a scatterplot to show the general relationship. This uses data where 
#each row represents one season for player's with at least 50 batted balls and their number of swing and misses and 
#number of swings where they make some kind of contact (either foul, foul tip, or hit into play) are listed. 
strikeout_eda <- strikeout_eda %>%
  mutate(swing_miss_percent = swinging_strike / (swinging_strike + contact))
strikeout_eda %>%
  ggplot(aes(x=attack_angle, y = swing_miss_percent))+
  geom_point()+
  theme_minimal()

#correlation coefficient 0.3199
cor(strikeout_eda$attack_angle, strikeout_eda$swing_miss_percent)

# Build a logistic model for swing and miss rate based on a player's attack angle.
swing_miss_prob <- glm(cbind(swinging_strike, contact)~attack_angle, family="binomial", data=strikeout_eda)
summary(swing_miss_prob)

# create a mock data frame of attack angles from 0 to 30 spaced by 0.5. Predict the swing and miss rate
#at each attack angle using the model above
attack_angles_mock <- data.frame("attack_angle" = seq(0, 30, 0.5))
preds <- predict(swing_miss_prob,attack_angles_mock, type = "response")
attack_preds <- cbind(attack_angles_mock, preds)

#plot the swing and miss prediction on the y axis for a given attack angle. We see
#a pretty linear relationship. 
attack_preds %>%
  ggplot(aes(x=attack_angle, y=preds))+
  geom_line()+
  theme_minimal()

####################################################################################################################################

# ANOTHER MODEL: trying to model probability of a swing and miss given attack angle and pitch height

# Filter for balls that were swung at in the dataset with all pitches between 2016 and 2021. Denote pitches that 
#were missed with a 1 (the "succcess" in this logistic model is a swing and miss) and denote pitches that some kind 
#of contact was made with a 0 (fouls, hit into play). 
contact_batted_balls <- batter_all_1621 %>%
  filter(description2 %in% c("swinging_strike", "foul", "hit_into_play")) %>%
  mutate(contact = case_when(description2 == "swinging_strike" ~ 1, 
                             description2 %in% c("foul", "hit_into_play") ~ 0))
# Create the contact dataset. This takes all pitches that were swung at from above, and assigns it the player's attack 
#angle in that appropriate season. By joining with batted_balls, we filter for players that had at least 50 batted 
#balls in a given season. Additionally, make contact a factor (0 or 1) and filter for pitches with a height less than 
#or equal to 5 ft and greater than -2.5 feet (I assume this just means they bounce far in front of the plate). 
contact_dataset <- batted_balls %>%
  left_join(contact_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, contact) %>%
  mutate(contact = as.factor(contact)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)
# Scatterplot of attack angle versus pitch height colored by if contact was made. 
contact_dataset %>%
  ggplot(aes(x=attack_angle, y=plate_z, color = contact))+
  geom_point()

# Create training and test datasets. 
set.seed(88)

nrow(contact_dataset)*0.75
sample_rows <- sample(nrow(contact_dataset), 1208368)

contact_train <- contact_dataset[sample_rows,]
contact_test <- contact_dataset[-sample_rows,]

# Create the logistic model. Predict whether contact will be made given a player's attack angle and 
#the height of the pitch. 
k_mod2 <- glm(contact ~ attack_angle + plate_z, data = contact_train, family = "binomial")
summary(k_mod2)
exp(coef(k_mod2))

# Test the model on the test dataset. I played around with the threshold to split at and found that 
#0.36 seemed to maximize the overall accuracy of the model (0.794). The average rate of contact 
#is 0.7686 in the contact_test dataset. 
contact_test$prob <- predict(k_mod2, contact_test, type = "response")
contact_test$pred[contact_test$prob >= .36] = 1
contact_test$pred[contact_test$prob < .36] = 0
contact_test$pred[is.na(contact_test$prob)] = 0

# Compute the overall accuracy of the simpler tree
mean(contact_test$pred == contact_test$contact) 

# Create side by side boxplots for the predicted probability. 
k_mod2 %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = contact)) + 
  geom_boxplot() + 
  ylab("Predicted probability of swing and miss") + 
  xlab("Acutal contact (1 = missed, 0 = fouled/hit into play") + 
  theme_classic()

# Create the confusion matrix and compute the accuracy of both predicting swings and misses 
#and also hit into play. With the threshold that maximized of the overall accuracy (0.36), the accuracy
#of predicting the "rare" event of swing and miss is very low at 0.205. 
threshold <- 0.36
k_mod2 %>%
  augment(type.predict = "response") %>%
  mutate(predict_swing_miss = as.numeric(.fitted >= threshold)) %>%
  count(contact, predict_swing_miss)

# Therefore, we should further lower the threshold in order to increase the probability of predicting the 
#rare event correctly. With a threshold of 0.29, the model predicts 84.64% of contact correctly and 
#41.22% of swing and misses correctly. The overall accuracy of the model is a bit lower at 74.6%. 
threshold <- 0.29
k_mod2 %>%
  augment(type.predict = "response") %>%
  mutate(predict_swing_miss = as.numeric(.fitted >= threshold)) %>%
  count(contact, predict_swing_miss)

#Ron's model
k_probability <- glm(cbind(K, n_pa-K)~attack_angle, family="binomial", data=strikeout_eda)
summary(k_probability)