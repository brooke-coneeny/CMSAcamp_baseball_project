########################################################################################################
#Overall goals: find a way to get a different set of pitches that the player might have hit at each 
#attack angle (things to consider - might swing at different pitches at different attack angles, 
#different total number of pitches hit at each attack angle, likelihood of hit different at different 
#attack angle)
########################################################################################################

########################################################################################################
#Step by step goals:
  #--> potentially create a model that predicts swingability of a pitch (because some you just so 
    #obviously don't swing at)
  #create a GAM that predicts probability of contact for any pitch given attack angle (and other params)
  #based on only the pitches they swung at
  #create a GAM that predicts probability of foul or in play for any of the contact pitches above?
  #need to sample pitches that might be hit into play (likely using the batting average model to predict
    #total balls in play for an attack angle) !!this one might need to account for great/poor players
  #once we FINALLY have a set of pitches that they hit over the season for that attack angle, we get 
    #the launch angles for these pitches and pass it all into the wOBA calculation to get the graph!
########################################################################################################

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
                                  TRUE ~ description),
         #Using Adam's recommended physics equations to calculate the approach angle of the pitch
         #These should be negative???
         approach_angle = atan((vz0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * az) / 
                                 (vy0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * ay)) * 180.0/pi)

batted_balls <- read_rds("public_data/batted_balls.rds")
strikeout_eda <- read_rds("public_data/strikeout_eda.rds")

####################################################################################################################################
#This GAM intends to model swing and miss probability based on different launch angles (in other words, predicting
#whether or not contact will be made on a given swing). It will model the probability of contact 
#based on attack angle, vertical pitch movement and horizontal pitch location, release speed, 
#pitch height, and approach angle. 

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

contact_gam <- gam(contact ~ s(plate_x, plate_z, k=28) + s(release_speed, k=10)
                   + s(attack_angle, approach_angle, k=28),
              data = contact_py_train, family = "binomial", method = "REML")
summary(contact_gam)

write_rds(contact_gam, "private_data/contact_gam_model.rds")
contact_gam <- read_rds("private_data/contact_gam_model.rds")
