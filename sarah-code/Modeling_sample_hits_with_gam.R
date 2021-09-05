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

#add attack angles to the set
batter_all_1621 <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(attack_angle >0 & attack_angle <=25) %>%
  mutate(attack_angle = round(attack_angle,0))

#calculate sacs per hit into play
sacs_per_hit_into_play <- batter_all_1621 %>%
  filter(events %!in% c("catcher_interf", "game_advisory")) %>%
  mutate(event_sac_fly = case_when(events == "sac_fly" ~ 1, 
                                   TRUE ~0)) %>%
  group_by(attack_angle, event_sac_fly) %>%
  count() %>%
  pivot_wider(id_cols = attack_angle, names_from = event_sac_fly, values_from = n) %>%
  rename(`sac`=`1`, `other`=`0`) %>%
  mutate(sacs_per_hip = sac/(sac+other)) %>%
  select(attack_angle, sacs_per_hip)

#add this column to batter_all_1621
batter_all_1621 <- batter_all_1621 %>%
  left_join(sacs_per_hit_into_play, by=c("attack_angle"))

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

#Modification of the function from part 1 of research to get final woba values
test_all_attack_sample <- function(woba_model, LA_model, player_data, year_data, orig_attack, orig_woba){
  #Get some values we need to calculate real woba
  walk <- nrow(year_data %>%
                        filter(events == "walk"))
  hbp <- nrow(year_data %>%
                       filter(events == "hit_by_pitch"))
  ibb <- nrow(year_data %>%
                       filter(events == "intent_walk"))
  ab <- nrow(year_data %>%
                      #By setting pitch number equal to 1 we get all of the first pitches for all of his
                      #plate appearances (thus counting the number of plate appearances)
                      filter(pitch_number == 1))
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack, times=31))
  original_woba <- c(rep(orig_woba, times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  #For printing actual modeled data when needed
  # pred_launch <- c()
  # pred_velo <- c()
  # attack <- c()
  # pred_woba <- c()
  
  for(possible_attack in 0:30){
    current_attack <- player_data %>% filter(attack_angle == possible_attack)
    #This ensures we end up with the correct value for sacs per hit in play at the possible attack angle
    sf <- (batter_all_1621 %>% filter(attack_angle == possible_attack))$sacs_per_hip[1]
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
    #xwOBA <- mean(preds$gam.preds, na.rm = TRUE)
    xwOBAcon <- sum(preds$gam.preds, na.rm = TRUE)
    xwOBA <- ((.69*walk + .72*hbp +xwOBAcon)/(ab + walk + ibb + sf + hbp))
    
    predicted_woba <- c(predicted_woba, xwOBA)
    
    #For printing modeled data
    # pred_launch <- c(pred_launch, modeled_data$launch_angle)
    # pred_velo <- c(pred_velo, modeled_data$launch_speed)
    # attack <- c(attack, rep(possible_attack, times = nrow(modeled_data)))
    # pred_woba <- c(pred_woba, preds$gam.preds)
    }
    avg_predicted_woba <- c(avg_predicted_woba, mean(predicted_woba))
    
  }
  return (tibble(original_attack = original_attack, possible_attack = possible_attack_vec,
                 original_woba = original_woba, predicted_woba = avg_predicted_woba))
  
  # return (tibble(predicted_launch = pred_launch, predicted_speed = pred_velo, attack_angle = attack,
  #                wOBAcon = pred_woba))
  
}

########################################################################################################
#Load in the models from the first section of the research
woba_model<- read_rds("public_data/woba_model.rds")
predicted_LA <- read_rds("private_data/LA_model.rds")


#Test for Trout
mtrout <- batter_all_1621 %>%
  #Need to get all they pitches he swung at in 2019
  filter(year == 2019, player_name == "Trout, Mike", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()

mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)

#After about 40 Trout didn't have any fair balls
mtrout_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, mtrout) 
#Pushing so Brooke can help without being able to run the models associated with this function
write_rds(mtrout_sample_hits, "public_data/mtrout_sample_hits.rds")
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

#Test for Heyward
jhey <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Heyward, Jason", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)

jhey_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, jhey) 
write_rds(jhey_sample_hits, "public_data/jhey_sample_hits.rds")
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
  filter(year == 2019, player_name == "Kemp, Tony", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
tkemp_woba <- mean(tkemp$woba_value, na.rm = TRUE)

#Kemp doesn't have any fairs after about 35 (so make sure the function that calculates woba stops there)
tkemp_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, tkemp)
write_rds(tkemp_sample_hits, "public_data/tkemp_sample_hits.rds")
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

#David Fletcher
dfletcher <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Fletcher, David", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
dfletcher_woba <- mean(dfletcher$woba_value, na.rm = TRUE)

dfletcher_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, dfletcher)
write_rds(dfletcher_sample_hits, "public_data/dfletcher_sample_hits.rds")
dfletcher_woba_values <- test_all_attack_sample(woba_model, predicted_LA, dfletcher_sample_hits, dfletcher,
                                            dfletcher$attack_angle[1], dfletcher_woba)

dfletcher_attack_angles_plot <- dfletcher_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = dfletcher$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "David Fletcher")

#Whit Merrifield
wmerrifield <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Merrifield, Whit", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
wmerrifield_woba <- mean(wmerrifield$woba_value, na.rm = TRUE)

wmerrifield_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, wmerrifield)
write_rds(wmerrifield_sample_hits, "public_data/wmerrifield_sample_hits.rds")
wmerrifield_woba_values <- test_all_attack_sample(woba_model, predicted_LA, wmerrifield_sample_hits, wmerrifield,
                                            wmerrifield$attack_angle[1], wmerrifield_woba)

wmerrifield_attack_angles_plot <- wmerrifield_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = wmerrifield$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Whit Merrifield")

#Freddy Galvis
fgalvis <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Galvis, Freddy", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
fgalvis_woba <- mean(fgalvis$woba_value, na.rm = TRUE)

fgalvis_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, fgalvis)
write_rds(fgalvis_sample_hits, "public_data/fgalvis_sample_hits.rds")
fgalvis_woba_values <- test_all_attack_sample(woba_model, predicted_LA, fgalvis_sample_hits, fgalvis,
                                            fgalvis$attack_angle[1], fgalvis_woba)

fgalvis_attack_angles_plot <- fgalvis_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = fgalvis$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Freddy Galvis")

#Dee Strange-Gordon
dsgordon <- batter_all_1621 %>%
  filter(year == 2019, player_name == "Strange-Gordon, Dee", description2 %in% c("foul", "hit_into_play", "foul_pitchout", "swinging_pitchout", "swinging_strike")) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()
dsgordon_woba <- mean(dsgordon$woba_value, na.rm = TRUE)

dsgordon_sample_hits <- get_sample_hits(contact_gam, fair_foul_gam, dsgordon)
write_rds(dsgordon_sample_hits, "public_data/dsgordon_sample_hits.rds")
dsgordon_woba_values <- test_all_attack_sample(woba_model, predicted_LA, dsgordon_sample_hits, dsgordon,
                                            dsgordon$attack_angle[1], dsgordon_woba)

dsgordon_attack_angles_plot <- dsgordon_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = dsgordon$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Dee Strange-Gordon")

