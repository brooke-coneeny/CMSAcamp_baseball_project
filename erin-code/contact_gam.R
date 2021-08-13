####################################################################################################################################
#This file intends to model swing and miss probability based on different launch angles. 
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
# A little bit of EDA on approach angle and attack angle versus strikeout rate

# Filter for balls that were swung at in the dataset with all pitches between 2016 and 2021. Denote pitches that 
#were missed with a 1 (the "succcess" in this GAM model is a swing and miss) and denote pitches that some kind 
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
         woba_value, description, description2, events, balls, strikes, plate_z, contact, plate_x, 
         release_speed, pfx_z, stand) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type), 
         plate_x = abs(plate_x)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

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
# Trying to model probability of contact with a GAM. 

#Reflect the lefties plate x values to match the righty. 
contact_dataset_lefty <- contact_dataset %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
contact_dataset_rest <- contact_dataset %>%
  filter(stand != "L")
contact_dataset <- bind_rows(contact_dataset_rest, contact_dataset_lefty)

# Create another training and test dataset from the contact_dataset. Group by player 
#and year so that all the pitches that a player swung at in a season are in either 
#the test or train dataset. 
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

# contact_gam <- gam(contact ~ s(plate_x, plate_z, k=28) + s(release_speed, k=10) 
#                    + s(attack_angle, pfx_z, k=28), 
#               data = contact_py_train, family = "binomial", method = "REML")
# summary(contact_gam)

write_rds(contact_gam, "public_data/contact_gam_model.rds")
contact_gam <- read_rds("public_data/contact_gam_model.rds")

# Test the model on the test dataset. I played around with the threshold to split at and found that 
#0.48 seemed to maximize the overall accuracy of the model (0.80696). The average rate of contact 
#is 0.765 in the contact_test dataset. 
contact_py_test$prob <- predict(contact_gam, type = "response", newdata = contact_py_test)
contact_py_test$pred[contact_py_test$prob >= .28] = 1
contact_py_test$pred[contact_py_test$prob < .28] = 0
contact_py_test$pred[is.na(contact_py_test$prob)] = 0

# Compute the overall accuracy
mean(contact_py_test$pred == contact_py_test$contact) 

# Computationally less expensive way to break down model accuracy. Using the threshold from the log-model 
#of 0.28, we get an accuracy of 85% for correctly predicting contact and 49.4% for correctly predicting 
#swing and misses. If we use a threshold of 0.2, accuracy of predicting contact falls to 70.7% but we can 
#predict 64.7% of swing and misses correctly. 
contact_py_test %>%
  group_by(contact) %>%
  count(pred)

# Create side by side boxplots for the predicted probability.
contact_gam %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = contact, group = contact)) + 
  geom_boxplot() + 
  ylab("Predicted probability of swing and miss") + 
  xlab("Acutal contact (1 = missed, 0 = fouled/hit into play") + 
  theme_classic()

#Group by player/year and look at their expected swing-miss average and what they actually got. 
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

# Create visualization of predicted versus actual swing and miss percentage.
#This seems to be looking a lot better than the logistic model. 
player_exp_swing_miss %>%
  #head(20) %>%
  ggplot(aes(x=attack_angle, y=percent, color = predicted))+
  geom_point(alpha = 0.7)+
  theme_minimal()+
  labs(x="attack angle", y="swing and miss percent", color = "")

# Plotting the gam...not super sure what the interaction plots are showing but 
#the release speed plot is showing the estimated probability of swinging and missing 
#as release speed changes. 
plot(contact_gam, pages = 1, trans = plogis, 
     shift = coef(contact_gam)[1])


