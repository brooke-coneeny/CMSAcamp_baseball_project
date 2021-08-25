####################################################################################################################################
#This file intends to model swing and miss probability based on different launch angles (in other words, predicting
#whether or not contact will be made on a given swing). We start this file by looking at a little EDA between vertical 
#pitch movement and swing and miss rate at different attack angles. We then build a GAM to model the probability of contact 
#based on attack angle, vertical pitch movement and horizontal pitch location, release speed, and pitch height. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(plotly)
library(broom)
library(mgcv)
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
                                  description %in% c("foul") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip") ~ "swinging_strike",
                                  TRUE ~ description), 
         #Using Adam's recommended physics equations to calculate the approach angle of the pitch
         #Negative because of the direction of v and a vectors
         approach_angle = -(atan((vz0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * az) / 
                                   (vy0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * ay)) * 180.0/pi))

batted_balls <- read_rds("public_data/batted_balls.rds")
strikeout_eda <- read_rds("public_data/strikeout_eda.rds")
attack_angles <- read_rds("public_data/attack_angles_1621.rds")
####################################################################################################################################
# A little bit of EDA on vertical movement of the pitch between the release point and home plate.
#Comparing attack angle versus strikeout rate and the relationship with this pitch movement.  

# Filter for balls that were swung at in the dataset with all pitches between 2016 and 2021. Denote pitches that 
#were missed with a 1 (the "success" in this GAM model is a swing and miss) and denote pitches that some kind 
#of contact was made with a 0 (fouls, hit into play). 
contact_batted_balls <- batter_all_1621 %>%
  filter(description2 %in% c("swinging_strike", "foul", "hit_into_play")) %>%
  mutate(contact = case_when(description2 == "swinging_strike" ~ 1, 
                             description2 %in% c("foul", "hit_into_play") ~ 0))

# Create the contact dataset. This takes all pitches that were swung at from above, and assigns it the player's attack 
#angle in that appropriate season. By joining with batted_balls, we filter for players that had at least 50 batted 
#balls in a given season. Additionally, make contact a factor (0 or 1) and filter for pitches with a height less than 
#or equal to 5 ft and greater than -2.5 feet (I assume this just means they bounce far in front of the plate). Also break 
#the pitch type down into broader categories. 
contact_dataset <- batted_balls %>%
  filter(balls_in_play >= 50) %>%
  left_join(contact_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, contact, plate_x, 
         release_speed, pfx_z, stand, approach_angle) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

# Create density plot showing the distribution of vertical pitch movement (ft) for all pitches 
#in the MLB 2016-2021. 
contact_dataset %>%
  filter(pfx_z < 2.5, pfx_z > -3) %>%
  ggplot(aes(x=pfx_z)) +
  geom_density(fill = "honeydew1")+
  #geom_vline(xintercept=0.5, color = "red", linetype="dashed")+
  #geom_vline(xintercept=1.75, color = "red", linetype="dashed")+
  theme_minimal()+
  labs(y="", x="", title = "Density of vertical pitch movement (inches)", 
       subtitle = "On pitches swung at in the MLB 2016-2021") +
  theme(plot.title.position = "plot", 
        plot.title = element_text(size=10), 
        plot.subtitle = element_text(size=8))

# Create plot showing the relationship between vertical pitch movement and swing and miss percentage
#for different attack angles. Break attack into down into four groups. 
contact_dataset %>%
  filter(attack_angle >0 & attack_angle < 30) %>%
  mutate(attack_group = cut(attack_angle, breaks = seq(0, 30, 7.5)),
         contact = as.factor(contact)) %>%
  group_by(attack_group, pfx_z) %>%
  count(contact) %>%
  pivot_wider(id_cols = c(attack_group, pfx_z), names_from = contact, values_from = n) %>%
  mutate(swing_miss_percent = `1`/(`1`+`0`)) %>%
  ggplot(aes(x=pfx_z, y = swing_miss_percent, color = attack_group)) +
  geom_smooth()+
  scale_color_manual(values = c("navy", "darkgoldenrod3", "firebrick4", "forestgreen", "deeppink4", "paleturquoise4"))+
  theme_minimal()+
  labs(y = "swing and miss percentage", 
       title = "At higher attack angles, swing and miss percentage tends to be higher on pitches with more vertical movement", 
       color = "Attack angle group", 
       x= "vertical movement (inches)")+
  theme(plot.title.position = "plot", 
        plot.title = element_text(size=9), 
        axis.title = element_text(size=8), 
        legend.title = element_text(size=8))

####################################################################################################################################
# First attempt with trying to model contact probability with a GAM. This uses 
#pitch horizontal and height coordinates, release speed, attack angle, and vertical movement. 

#Reflect the lefties plate x values to match the righties.  
contact_dataset_lefty <- contact_dataset %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
contact_dataset_rest <- contact_dataset %>%
  filter(stand != "L")
contact_dataset <- bind_rows(contact_dataset_rest, contact_dataset_lefty)

# Create training and test dataset from the contact_dataset. Group by player 
#and year so that all the pitches that a player swung at in a season are in either 
#the test or train dataset. 
player_year <- contact_dataset %>%
  group_by(player_name, year) %>%
  count()

set.seed(214)

nrow(player_year)*0.75
sample_rows2 <- sample(nrow(player_year), 1972)

player_year_train <- player_year[sample_rows2,]
player_year_test <- player_year[-sample_rows2,]

contact_py_train <- contact_dataset %>%
  right_join(player_year_train, by = c("player_name", "year"))
contact_py_test <- contact_dataset %>%
  right_join(player_year_test, by = c("player_name", "year"))

# Create the GAM. Predict whether contact will be made given a player's attack angle and 
#the height of the pitch. 

#contact_gam <- gam(contact ~ s(plate_x, plate_z, k=28) + s(release_speed, k=10) 
#                   + s(attack_angle, pfx_z, k=28), 
#                   data = contact_py_train, family = "binomial", method = "REML")
summary(contact_gam)
write_rds(contact_gam, "private_data/contact_gam_model.rds")

contact_gam <- read_rds("private_data/contact_gam_model.rds")

# Test the model on the test dataset. First, find the average rate of contact in the test dataset. 
#It is  0.7545902. 
contact_py_test %>%
  group_by(contact) %>%
  count()

# Determine an optimal threshold at which to predict contact. If the probability of contact is above this threshold, 
#predict the batter will make contact (0). If not, predict they will swing and miss (1). Because contact is so common at 
#over 3/4 of the time, it will be harder to predict accurately swings and misses than it will to predict contact. 
#It seems like a good threshold that might balance the accuracy of both categories (even if it sacrifices the overall 
#accuracy a bit) is 0.22. 
contact_py_test$prob <- predict(contact_gam, type = "response", newdata = contact_py_test)
contact_py_test$pred[contact_py_test$prob >= .22] = 1
contact_py_test$pred[contact_py_test$prob < .22] = 0
contact_py_test$pred[is.na(contact_py_test$prob)] = 0

# Compute the overall accuracy, break down the accuracy into categories for correctly predicting 
#swing and misses as well as contact. 
#Overall model accuracy on the test dataset is 67.7%. 68.3% of contact is predicted correctly 
#and 65.9% of swing and misses are predicted correctly. 
mean(contact_py_test$pred == contact_py_test$contact) 
contact_py_test %>%
  group_by(contact) %>%
  count(pred) %>%
  mutate(freq = n/sum(n))

# Create side by side boxplots for the predicted probability.
contact_gam %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = contact, group = contact)) + 
  geom_boxplot() + 
  ylab("Predicted probability of swing and miss") + 
  xlab("Acutal contact (1 = missed, 0 = fouled/hit into play") + 
  theme_classic()

# Group by player/year and look at their expected swing-miss average and what they actually got.
#Create visualization of predicted versus actual swing and miss percentage.
#This seems to be looking a lot better than the logistic model did. 
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

player_exp_swing_miss %>%
  ggplot(aes(x=attack_angle, y=percent, color = predicted))+
  geom_point(alpha = 0.7)+
  theme_minimal()+
  labs(x="attack angle", y="swing and miss percent", color = "")

# Create a residual plot for the model using the test data. This looks okay - it tends to overestimate the 
#swing and miss percentage when it predicts swing and miss percentage in general and especially at high predicted
#swing and miss percentages. 
player_exp_swing_miss %>%
  pivot_wider(id_cols = player_name:attack_angle, names_from = predicted, values_from = percent) %>%
  mutate(resid = actual - expected) %>%
  ggplot(aes(x=expected, y = resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x="Predicted swing and miss percentage", y ="Residuals", title = "Residual Plot for Predicted Swing-Miss Test Data")+
  theme_minimal()

####################################################################################################################################
# GAM 2: Exact same as contact gam 1, except this one includes pitch approach angle instead of vertical break. 

#contact_gam2 <- gam(contact ~ s(plate_x, plate_z, k=28) + s(release_speed, k=10) 
#                    + s(attack_angle, approach_angle, k=28), 
#                    data = contact_py_train, family = "binomial", method = "REML")
#summary(contact_gam2)

#write_rds(contact_gam2, "private_data/contact_gam2.rds")
contact_gam2 <- read_rds("private_data/contact_gam2.rds")

#Using the same dataset as GAM 1, test the new model. 
contact_py_test$prob2 <- predict(contact_gam2, type = "response", newdata = contact_py_test)
contact_py_test$pred2[contact_py_test$prob2 >= .22] = 1
contact_py_test$pred2[contact_py_test$prob2 < .22] = 0
contact_py_test$pred2[is.na(contact_py_test$prob2)] = 0

# Compute the overall accuracy, break down the accuracy into catogories for correctly predicting swing and misses as well 
#as contact. 
#At the same threshold as before, the model overall accuracy is slightly higher at 68.1017%.  
#68.7% of contact is predicted correctly and 66.4% of swing and misses are predicted correctly. 
mean(contact_py_test$pred2 == contact_py_test$contact) 
contact_py_test %>%
  group_by(contact) %>%
  count(pred2) %>%
  mutate(freq = n/sum(n))

# Create side by side boxplots for the predicted probability.
contact_gam2 %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = contact, group = contact)) + 
  geom_boxplot() + 
  ylab("Predicted probability of swing and miss") + 
  xlab("Acutal contact (1 = missed, 0 = fouled/hit into play") + 
  theme_classic()

# Group by player/year and look at their expected swing-miss average and what they actually got.
#Create visualization of predicted versus actual swing and miss percentage. This looks pretty much the same 
#as for the other GAM. 
player_exp_swing_miss <- contact_py_test %>%
  group_by(player_name, year) %>%
  summarize(player_name, year, attack_angle, exp_swing_miss = mean(prob2)) %>%
  distinct() %>%
  left_join(strikeout_eda, by=c("player_name", "year", "attack_angle")) %>%
  select(player_name, year, attack_angle, exp_swing_miss, swing_miss_percent) %>%
  pivot_longer(cols = exp_swing_miss:swing_miss_percent, names_to = "predicted", 
               values_to = "percent") %>%
  mutate(predicted = case_when(predicted == "exp_swing_miss" ~ "expected", 
                               predicted == "swing_miss_percent" ~ "actual"))

player_exp_swing_miss %>%
  ggplot(aes(x=attack_angle, y=percent, color = predicted))+
  geom_point(alpha = 0.7)+
  theme_minimal()+
  labs(x="attack angle", y="swing and miss percent", color = "")

# Create a residual plot for the model using the test data. This again looks okay, 
#but it tends to overestimate the swing and miss percentage especially at values over 28%.  
player_exp_swing_miss %>%
  pivot_wider(id_cols = player_name:attack_angle, names_from = predicted, values_from = percent) %>%
  mutate(resid = actual - expected) %>%
  ggplot(aes(x=expected, y = resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x="Predicted swing and miss percentage", y ="Residuals", title = "Residual Plot for Predicted Swing-Miss Test Data for GAM 2")+
  theme_minimal()