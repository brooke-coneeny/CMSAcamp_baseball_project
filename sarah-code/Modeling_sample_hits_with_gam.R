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
         release_speed, pfx_z, stand, approach_angle) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) %>%
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

set.seed(214)

nrow(player_year)*0.75
sample_rows2 <- sample(nrow(player_year), 1972)

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
gam.check(contact_gam)
#.22 is the threshold that maximized accuracy

write_rds(contact_gam, "private_data/contact_gam_model.rds")
contact_gam <- read_rds("private_data/contact_gam_model.rds")

########################################################################################################
# FAIR or FOUL GAM
fair_foul_batted_balls <- batter_all_1621 %>%
  filter(description %in% c("hit_into_play", "foul")) %>%
  mutate(fair_ball = case_when(description2 == "hit_into_play" ~ 1, 
                               description2 == "foul" ~0))

# Create the fair/foul dataset. This takes all pitches that were either hit foul or fair, and assigns it the player's attack 
#angle in that appropriate season. By joining with batted_balls, we filter for players that had at least 50 batted 
#balls in a given season. Filter for pitches with a height less than or equal to 5 ft and greater than -2.5 feet 
#(I assume this just means they bounce far in front of the plate). Also break the pitch type down into broader categories. 
fair_foul_dataset <- batted_balls %>%
  filter(balls_in_play >= 50) %>%
  left_join(fair_foul_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, fair_ball, plate_x, 
         release_speed, release_spin_rate, stand, approach_angle) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

#Reflect the lefties plate x values to match the rightys.  
fair_foul_dataset_lefty <- fair_foul_dataset %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
fair_foul_dataset_rest <- fair_foul_dataset %>%
  filter(stand != "L")
fair_foul_dataset <- bind_rows(fair_foul_dataset_rest, fair_foul_dataset_lefty)

# Create training and test dataset from the contact_dataset. Group by player 
#and year so that all the pitches that a player swung at in a season are in either 
#the test or train dataset. 
player_year_ff <- fair_foul_dataset %>%
  group_by(player_name, year) %>%
  count()

set.seed(216)

nrow(player_year_ff)*0.75
sample_rows3 <- sample(nrow(player_year_ff), 1972)

player_year_train_ff <- player_year_ff[sample_rows3,]
player_year_test_ff <- player_year_ff[-sample_rows3,]

ff_py_train <- fair_foul_dataset %>%
  right_join(player_year_train_ff, by = c("player_name", "year"))
ff_py_test <- fair_foul_dataset %>%
  right_join(player_year_test_ff, by = c("player_name", "year"))

# Create the GAM. Predict whether contact will be made given a player's attack angle and 
#the height of the pitch. 

fair_foul_gam <- gam(fair_ball ~ s(plate_x, plate_z, k=20) + s(release_speed, release_spin_rate, k=20) 
                     + s(attack_angle, approach_angle, k=20), 
                     data = ff_py_train, family = "binomial", method = "REML")
summary(fair_foul_gam)
gam.check(fair_foul_gam)
#.5 is the threshold that maximized accuracy

write_rds(fair_foul_gam, "private_data/fair_foul_gam_model.rds")

fair_foul_gam <- read_rds("private_data/fair_foul_gam_model.rds")

########################################################################################################
#Creating functions so we can go from their full season pitches to a 
#subset that they might have hit at a different attack angle

clean_edges <- function (data){
  for(i in 1:length(data$launch_angle)){
    if(!is.na(data$launch_angle[i])){
      if(data$launch_angle[i] < (mean(data$launch_angle, na.rm = TRUE)-2*sd(data$launch_angle, na.rm = TRUE))){
        data$cleaned_launch_angle[i] <- (mean(data$launch_angle, na.rm = TRUE)-2*sd(data$launch_angle, na.rm = TRUE))
      }
      else if(data$launch_angle[i] > (mean(data$launch_angle, na.rm = TRUE)+2*sd(data$launch_angle, na.rm = TRUE))){
        data$cleaned_launch_angle[i] <- (mean(data$launch_angle, na.rm = TRUE)+2*sd(data$launch_angle, na.rm = TRUE))
      }
      else{
        data$cleaned_launch_angle[i]<-data$launch_angle[i]
      }
    }
    else{
      data$cleaned_launch_angle[i]<-data$launch_angle[i]
    }
  }
  return (data)
}


#Function that will get the sample of balls they could have hit into play for each attack angle
get_sample_hits <- function(contact_model, fair_foul_model, player_data){
  #Create an empty starting tibble
  sample_hits <- player_data
  sample_hits <- sample_hits[0,]
  #Repeat this process for all 31 attack angles we are looking at
  for(possible_attack in 0:30){
    player_data$attack_angle <- possible_attack
    #Get the whether there is contact using threshold of .22
    prob_contact <- predict(contact_model, newdata = player_data, type = "response")
    player_data <- player_data %>% mutate(pitch_prob_contact = prob_contact, 
                                          pred_contact = ifelse(pitch_prob_contact >= .22, 1, 0))

    #Get the subset of player_data that predicted contact
    player_contact <- player_data %>% filter(pred_contact == 1)
    
    #Get whether it is fair using threshold of .5
    prob_fair <- predict(fair_foul_model, newdata = player_contact, type = "response")
    player_contact <- player_contact %>% mutate(prob_fair = prob_fair,
                              pred_fair = ifelse(prob_fair >= .5, 1, 0))
    
    #Get the subset of player_contact that predicted fair
    player_fair <- player_contact %>% filter(pred_fair == 1)
    sample_hits <- rbind(sample_hits, player_fair)
  }
  return (sample_hits)
}

########################################################################################################

#Modified function from previous presentation to get predicted wobas for each attack angle
test_all_attack_sample <- function(woba_model, LA_model, player_data, year_data, orig_attack, orig_woba){
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack, times=31))
  original_woba <- c(rep(orig_woba, times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  
  for(possible_attack in 0:30){
    current_attack <- player_data %>% filter(attack_angle == possible_attack)
    # Repeat 10 times
    for(n in 1:10){
      EV_vector4 <- vector()    # To hold launch speeds for this function
      
      # Find the possible launch angle for this attack angle
      #current_attack$attack_angle <- possible_attack
      pred_angles <- tibble(lm.preds = predict(LA_model, newdata = current_attack))
      pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
                                                          sd = sigma(LA_model)), 
                                            launch_angle = lm.preds + noise)

      for(i in 1:length(pred_angles$launch_angle)){
        # Sample a launch speed around their actual attack angle
        hits_at_angle <- year_data %>%     #we want to sample exit velocities from his actual data
          #not just the ones we sampled as potential hit into play
          filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
                   orig_attack-3 & !is.na(launch_speed))
        # Randomly sample 1 exit velocity form similar hits
        EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
        pred_EV <- hits_at_angle[EV_sample_index,] 
        # Add that launch speed to vector as the predicted launch speed 
        EV_vector4 <- c(EV_vector4, pred_EV$launch_speed)
      }
      # Create modeled data for this attack angle
      modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector4)
      preds <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
      xwOBA <- mean(preds$gam.preds, na.rm = TRUE)
      
      predicted_woba <- c(predicted_woba, xwOBA)
    }
    avg_predicted_woba <- c(avg_predicted_woba, mean(predicted_woba))
  }
  return (tibble(original_attack = original_attack, possible_attack = possible_attack_vec, 
                 original_woba = original_woba, predicted_woba = avg_predicted_woba))
  
}

########################################################################################################
#Load in the models from the first section of the research
woba_model<- read_rds("public_data/woba_model.rds")
predicted_LA <- read_rds("private_data/LA_model.rds")


#Test for Trout
mtrout <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Trout, Mike") %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()

mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)

mtrout_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, mtrout) 
mtrout_woba_values <- test_all_attack_sample(woba_model, predicted_LA, mtrout_sample_hits, mtrout,
                                             mtrout$attack_angle[1], mtrout_woba)

mtrout_attack_angles_plot <- mtrout_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = mtrout$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Mike Trout")

#average woba is so high because of the hits it sampled as being 
#successfully fair if hit at 30 degree attack angle, they were WELL
#hit balls
max_attack <- mtrout_sample_hits %>% filter(attack_angle == 30) %>% 
  summarize(mean(woba_value, na.rm = TRUE))

#Test for Heyward
jhey <- batter_all_1621 %>%
  filter(player_name == "Heyward, Jason") %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)

jhey_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, jhey) 
jhey_woba_values <- test_all_attack_sample(woba_model, predicted_LA, jhey_sample_hits, jhey,
                                           jhey$attack_angle[1], jhey_woba)
jhey_attack_angles_plot <- jhey_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+                                #Didn't plateau like Trout for some reason
  theme_bw()+
  geom_vline(xintercept = jhey$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Jason Heyward")

#Test for Kemp
tkemp <- batter_all_1621 %>%
  filter(player_name == "Kemp, Tony") %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
tkemp_woba <- mean(tkemp$woba_value, na.rm = TRUE)

tkemp_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, tkemp)
tkemp_woba_values <- test_all_attack_sample(woba_model, predicted_LA, tkemp_sample_hits, tkemp,
                                            tkemp$attack_angle[1], tkemp_woba)

tkemp_attack_angles_plot <- tkemp_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = tkemp$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Tony Kemp")



