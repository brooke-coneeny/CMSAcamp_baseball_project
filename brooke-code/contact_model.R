###################################################################################################################################
#This file intends to model contact prob from different variables 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(caret)
library(e1071)
library(ROCR)
library(ROCit)
library(mgcv)

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
  mutate(description2 = case_when(description %in% c("automatic_ball", "ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout", "called_strike") ~ "other", 
                                  description %in% c("foul") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked","foul_tip") ~ "swinging_strike",
                                  description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score") ~ "in_play",
                                  TRUE ~ description)) %>%
  #Using Adam's recommended physics equations to calculate the approach angle of the pitch
  #Negative because of the direction of v and a vectors
  mutate(approach_angle = -(atan((vz0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * az) / 
                                   (vy0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * ay)) * 180.0/pi))

#Finding each player's attack angle in each season
attack_angles <- batter_all %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#Finding how many balls each player put in play 
hit_in_play <- batter_all %>%
  group_by(player_name, year) %>%
  filter(description == "hit_into_play") %>%
  count() %>%
  rename(balls_in_play = n)

####################################################################################################################################

#Looking at data pitch by pitch, did they make contact with the attack angle or did they miss? How is this attack angle effecting the
#amount of pitches they can hit? 

#Lets only look at pitches they fouled, hit in play, or missed. This means were ignoring pitches they did not swing at 
#If they swing and miss, there is no contact (1) if they foul or hit in play there is contact (0)
contact_all <- batter_all %>%
  filter(description2 %in% c("swinging_strike", "foul", "in_play")) %>%
  mutate(contact = case_when(description2 == "in_play" ~ 1, 
                             description2 %in% c("foul", "swinging_strike") ~ 0))

#Combining with the attack angles of each player in each year 
#Combining with number of balls in play by player and year 
contact_batter_all <- contactr_all %>%
  left_join(hit_in_play, by=c("player_name", "year")) %>%
  left_join(attack_angles, by = c("player_name", "year"))

contact_batter_all <- contact_batter_all %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) %>%
  filter(!is.na(pitch_type)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

# Reflect the lefties plate x values to match the righties.  
contact_dataset_lefty <- contact_batter_all %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)

contact_dataset_rest <- contact_batter_all %>%
  filter(stand != "L")

contact_batter_all <- bind_rows(contact_dataset_rest, contact_dataset_lefty)
####################################################################################################################################

#Selecting only the variables we are interested in 
contact_data <- contact_batter_all %>%
  select("player_name", "year", "attack_angle", "plate_z", "launch_angle", "launch_speed") %>%
  filter(!is.na(launch_angle), !is.na(launch_speed)) %>%
  mutate(row_ID = row_number())

#Creating training and testing data sets for contact model
#Group by player and year so that all the pitches that a player swung at in a season are in either the test or train data set. 
player_num_pitches <- contact_data%>%
  group_by(player_name, year) %>%
  count()

#75 percent of the sample size
smp_size <- floor(0.75 * nrow(player_num_pitches))

#Set the seed to make partition reproducible
set.seed(315)
sample_rows <- sample(nrow(player_num_pitches), smp_size)

player_year_train <- player_num_pitches[sample_rows,]
player_year_test <- player_num_pitches[-sample_rows,]

contact_train <- contact_batter_all %>%
  right_join(player_year_train, by = c("player_name", "year")) 

contact_test <- contact_batter_all %>%
  right_join(player_year_test, by = c("player_name", "year"))

#Creating a GAM model which predicts probability of contact for any given hit given attack angle and pitch height
#using only pitches they swung at 
contact_model <- gam(contact ~ s(plate_z) + s(release_speed)
                     + s(attack_angle) + s(approach_angle),
                     data = contact_train, family = "binomial", method = "REML")

write_rds(contact_model, "private_data/contact_model.rds")
contact_model <- read_rds("private_data/contact_gam_model.rds")

#Testing accuracy of the model 
contact_test$prob <- predict(contact_model, type = "response", newdata = contact_test)

contact_threshold <- median(contact_test$prob)

contact_test$pred[contact_test$prob >= contact_threshold] = 1
contact_test$pred[contact_test$prob < contact_threshold] = 0
contact_test$pred[is.na(contact_test$prob)] = 0

#Compute the overall accuracy
mean(contact_test$pred == contact_test$contact) 

####################################################################################################################################

#Finding predicted values, assigning an ID so we can add column to contact_data 
contact_prob <- data.frame(predict(contact_model, contact_data, type = "response"))
names(contact_prob)[1] <- "contact_prob"

contact_prob <- contact_prob %>%
  mutate(row_ID = row_number())

contact_data <- contact_data %>% 
  left_join(contact_prob, by = "row_ID") 

####################################################################################################################################

#create a GAM that predicts probability of foul or in play for any of the contact pitches above?
#need to sample pitches that might be hit into play (likely using the batting average model to predict
#total balls in play for an attack angle) !!this one might need to account for great/poor players

#creating the median probability of contact as the threshold for the moment 
contact_threshold <- median(contact_data$contact_prob)

#pred_contact determines if they made contact or not based off the threshold 
#if it was less than median chance of contact than 0 for no contact, otherwise 1 for contact 
contact_batter_all <- contact_batter_all %>%
  mutate(pred_contact = case_when(contact_prob < contact_threshold ~ 0, 
                                  contact_prob >= contact_threshold ~ 1)) 



  










