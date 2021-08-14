####################################################################################################################################
# This file intends to model swing and miss probability based on different launch angles. In this file we begin by 
#creating a general model to predict swing and miss probability based on attack angle. We then build two models - one that 
#predicts the probability of contact being made, and one that predicts the probability of a ball being hit into play. 
#These are logistic models. Later on, we explore these same types of models with GAMs. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(plotly)
library(broom)
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

#Write files to the public data folder
write_rds(strikeout_eda, "public_data/strikeout_eda.rds")
write_rds(batted_balls, "public_data/batted_balls.rds")

####################################################################################################################################

#LOGISTIC MODEL (GENERAL) FOR ATTACK ANGLE VS SWING & MISS RATE

#Create a scatterplot to show the general relationship. This uses data where 
#each row represents one season for players with at least 50 batted balls and their number of swing and misses and 
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
# ANOTHER MODEL: trying to model probability of a swing and miss given attack angle, 
#height of pitch, pitch x coordinate, release speed, and pitch type. 

# Filter for balls that were swung at in the dataset with all pitches between 2016 and 2021. Denote pitches that 
#were missed with a 1 (the "success" in this logistic model is a swing and miss) and denote pitches that some kind 
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
  filter(balls_in_play >= 50) %>%
  left_join(contact_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, contact, plate_x, release_speed, pfx_z) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type), 
         plate_x = abs(plate_x)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

# Scatterplot of attack angle versus pitch height colored by if contact was made. 
contact_dataset %>%
  ggplot(aes(x=attack_angle, y=plate_z, color = contact))+
  geom_point()

# Create another training and test dataset from the contact_dataset. This time, group by player and year so that all the pitches
#that a player swung at in a season are in either the test or train dataset. 
player_year <- contact_dataset %>%
  group_by(player_name, year) %>%
  count()

set.seed(211)

nrow(player_year)*0.75
sample_rows2 <- sample_rows <- sample(nrow(player_year), 1972)

player_year_train <- player_year[sample_rows2,]
player_year_test <- player_year[-sample_rows2,]

contact_py_train <- contact_dataset %>%
  right_join(player_year_train, by = c("player_name", "year"))
contact_py_test <- contact_dataset %>%
  right_join(player_year_test, by = c("player_name", "year"))

# Create the logistic model. Predict whether contact will be made given a player's attack angle and 
#the height of the pitch. 
k_mod2 <- glm(contact ~ attack_angle + plate_z + pitch_type + plate_x + release_speed, 
              data = contact_py_train, family = "binomial")
summary(k_mod2)
exp(coef(k_mod2))

# Test the model on the test dataset. I played around with the threshold to split at and found that 
#0.41 seemed to maximize the overall accuracy of the model (0.798). The average rate of contact 
#is 0.765 in the contact_test dataset. 
contact_py_test$prob <- predict(k_mod2, contact_py_test, type = "response")
contact_py_test$pred[contact_py_test$prob >= .41] = 1
contact_py_test$pred[contact_py_test$prob < .41] = 0
contact_py_test$pred[is.na(contact_py_test$prob)] = 0

# Compute the overall accuracy
mean(contact_py_test$pred == contact_py_test$contact) 

# Create side by side boxplots for the predicted probability. 
k_mod2 %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = contact, group = contact)) + 
  geom_boxplot() + 
  ylab("Predicted probability of swing and miss") + 
  xlab("Acutal contact (1 = missed, 0 = fouled/hit into play") + 
  theme_classic()

# Create the confusion matrix and compute the accuracy of both predicting swings and misses 
#and also hit into play. With the threshold that maximized of the overall accuracy (0.41), the accuracy
#of predicting the "rare" event of swing and miss is very low at 18.8%. The accuracy of predicting contact 
#is high at 97.5%. 
threshold <- 0.41
k_mod2 %>%
  augment(type.predict = "response") %>%
  mutate(predict_swing_miss = as.numeric(.fitted >= threshold)) %>%
  count(contact, predict_swing_miss)

# Therefore, we should further lower the threshold in order to increase the probability of predicting the 
#rare event correctly. With a threshold of 0.28, the model predicts 80.52% of contact correctly and 
#46.87% of swing and misses correctly. The overall accuracy of the model is a bit lower at 73.22%. 
threshold <- 0.28
k_mod2 %>%
  augment(type.predict = "response") %>%
  mutate(predict_swing_miss = as.numeric(.fitted >= threshold)) %>%
  count(contact, predict_swing_miss)

####################################################################################################################################
# BASED ON THE PREVIOUS MODEL: VISUALIZATIONS AND ACCURACY 

#Visualization of attack angle versus pitch height and color by probability of swinging and missing 
contact_py_test %>%
  ggplot(aes(x=attack_angle, y=plate_z, color = prob))+
  geom_point()

#Let's group by player/year and look at their expected swing-miss average and what they actually got. 
player_exp_swing_miss <- contact_py_test %>%
  group_by(player_name, year) %>%
  summarize(player_name, year, attack_angle, exp_swing_miss = mean(prob)) %>%
  distinct() %>%
  left_join(strikeout_eda, by=c("player_name", "year", "attack_angle")) %>%
  select(player_name, year, attack_angle, exp_swing_miss, swing_miss_percent) %>%
  pivot_longer(cols = exp_swing_miss:swing_miss_percent, names_to = "predicted", 
               values_to = "percent") %>%
  mutate(predicted = case_when(predicted == "exp_swing_miss" ~ "expected", 
                               predicted == "swing_miss_percent" ~ "actual"))

# Create visualization of predicted versus actual swing and miss percentage. It seems like model might not 
#allow for enough variability in swing-miss percentage. 
player_exp_swing_miss %>%
  #head(20) %>%
  ggplot(aes(x=attack_angle, y=percent, color = predicted))+
  geom_point(alpha = 0.7)+
  theme_minimal()+
  labs(x="attack angle", y="swing and miss percent", color = "")

####################################################################################################################################
# MODEL 3: WHETHER OR NOT THE BALL WAS HIT INTO PLAY
# Create a logistic model in a similar process, this time just whether the ball was hit into play (1) or
#fouled/missed (0). 

# create the contact_batted_balls2 dataset. Assign 1 if the ball was hit into play and 0 if it swung at but not 
#hit into play. 
contact_batted_balls2 <- batter_all_1621 %>%
  filter(description2 %in% c("swinging_strike", "foul", "hit_into_play")) %>%
  mutate(hit_into_play = case_when(description2 == "hit_into_play" ~ 1, 
                             description2 %in% c("foul", "swinging_strike") ~ 0))

# create the contact_dataset2. This time, create a variable for pitch type that denotes whether a pitch is breaking, 
#offspeed, or a fastball. Additionally, take the absolute value of plate_x so that it we aren't dealing with 
#inside versus outside for lefties versus righties, etc. 
contact_dataset2 <- batted_balls %>%
  filter(balls_in_play >= 50) %>%
  left_join(contact_batted_balls2, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, hit_into_play, plate_x, release_speed) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type), 
         plate_x = abs(plate_x)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

# Create training and test dataset from the contact_dataset2. Group by player and year so that all the pitches
#that a player swung at in a season are in either the test or train dataset. 
player_year2 <- contact_dataset2 %>%
  group_by(player_name, year) %>%
  count()

set.seed(23)

nrow(player_year2)*0.75
row_nums <- sample(nrow(player_year2), 1972)

hit_into_play_train <- player_year2[row_nums,]
hit_into_play_test <- player_year2[-row_nums,]

hit_into_play_train_data <- contact_dataset2 %>%
  right_join(hit_into_play_train, by = c("player_name", "year"))
hit_into_play_test_data <- contact_dataset2 %>%
  right_join(hit_into_play_test, by = c("player_name", "year"))

# Create the logistic model. Predict whether the ball will be hit into play based on the attack angle, height of pitch, 
#how far from the middle of the plate the pitch was, the pitch speed, and pitch type. 
hp_log_mod <- glm(hit_into_play ~ attack_angle + plate_z + plate_x + release_speed + pitch_type, 
                  data = hit_into_play_train_data, family = "binomial")
summary(hp_log_mod)
exp(coef(hp_log_mod))
#confint(hp_log_mod) %>%
#  exp()

# Test the model on the test dataset. The threshold I chose (explained below) has an overall accuracy for the model 
# of 54.7%. This is likely because its really hard to differentiate between foul and fair balls. 
hit_into_play_test_data$prob <- predict(hp_log_mod, hit_into_play_test_data, type = "response")
hit_into_play_test_data$pred[hit_into_play_test_data$prob >= .385] = 1
hit_into_play_test_data$pred[hit_into_play_test_data$prob < .385] = 0
hit_into_play_test_data$pred[is.na(hit_into_play_test_data$prob)] = 0

# Compute the overall accuracy
mean(hit_into_play_test_data$pred == hit_into_play_test_data$hit_into_play)

# Create side by side boxplots for the predicted probability. 
hp_log_mod %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = hit_into_play, group = hit_into_play)) + 
  geom_boxplot() + 
  ylab("Predicted probability of hit into play") + 
  xlab("Acutal contact (1 = hit into play, 0 = foul/miss") + 
  theme_classic()

# Create the confusion matrix and compute the accuracy predictions when predicting the ball was hit into play
#and not hit into play. It seems like the threshold that best balances the accuracy is 0.385. It correctly predicted 
#54.9% of the balance fouled/missed correctly and 53.8% of the balls hit into play correctly. 
threshold <- 0.385
hp_log_mod %>%
  augment(type.predict = "response") %>%
  mutate(predict_hit_into_play = as.numeric(.fitted >= threshold)) %>%
  count(hit_into_play, predict_hit_into_play)
####################################################################################################################################
#Ron's model
k_probability <- glm(cbind(K, n_pa-K)~attack_angle, family="binomial", data=strikeout_eda)
summary(k_probability)