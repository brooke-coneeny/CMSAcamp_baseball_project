###################################################################################################################################
#This file intends to go through our original process using only predicted fair balls
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)

#Loading Data 
batter_all_2019 <- read_rds("private_data/all2019data.rds")

####################################################################################################################################

batter_all <- batter_all_2019 %>%
  filter(player_name == current_player) %>%
  filter(!is.na(launch_speed), !is.na(launch_angle), !is.na(plate_z)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

# Find the attack angle for each pitch 
batter_all <- batter_all %>%
  filter(description == "hit_into_play") %>%
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

# Categorize the result of the pitch, also find the approach angles 
batter_all <- batter_all %>%
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

# Categorize the different types of pitches 
batter_all <- batter_all %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) 

# Reflect the lefties plate x values to match the righties.  
contact_dataset_lefty <- batter_all %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)

contact_dataset_rest <- batter_all %>%
  filter(stand != "L")

batter_all <- bind_rows(contact_dataset_rest, contact_dataset_lefty)

####################################################################################################################################

# Load in contact and fair/foul models 

contact_model <- read_rds("private_data/contact_gam_model.rds")
fair_foul_model <- read_rds 

####################################################################################################################################

#Function that will get the sample of balls they could have hit into play for each attack angle
get_sample_hits <- function(contact_model, fair_foul_model, player_data){
  #Create an empty starting tibble
  sample_hits <- player_data
  sample_hits <- sample_hits[0,]
  #Repeat this process for all 31 attack angles we are looking at
  for(possible_attack in 0:30){
    player_data$attack_angle <- possible_attack
    #Predict whether the batter makes contact
    prob_contact <- predict(contact_model, newdata = player_data, type = "response")
    #Using a threshold of 0.22, decide if there was contact or not based on predicted contact probabilities 
    player_data <- player_data %>%
      mutate(pitch_prob_contact = prob_contact, 
             pred_contact = ifelse(pitch_prob_contact >= .22, 1, 0))
    
    #Get the subset of player_data when the batter made contact 
    player_contact <- player_data %>% filter(pred_contact == 1)
    
    #Predict whether the batter hit the ball fair or foul 
    prob_fair <- predict(fair_foul_model, newdata = player_contact, type = "response")
    #Using the threshold of 0.5, decide if the hit was fair or foul 
    player_contact <- player_contact %>% 
      mutate(prob_fair = prob_fair,
             pred_fair = ifelse(prob_fair >= .5, 1, 0))
    
    #Get the subset of player_contact that predicted fair
    player_fair <- player_contact %>% 
      filter(pred_fair == 1)
    sample_hits <- rbind(sample_hits, player_fair)
  }
  return (sample_hits)
}

####################################################################################################################################

# Load in launch angle model and woba model 

woba_model<- read_rds("public_data/woba_model.rds")
predicted_LA <- read_rds("private_data/LA_model.rds")

####################################################################################################################################

#Modified function from previous presentation to get predicted wobas for each attack angle
test_all_attack_sample <- function(woba_model, LA_model, player_data, year_data, orig_attack, orig_woba){
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack, times=31))
  original_woba <- c(rep(orig_woba, times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  
  for(possible_attack in 0:30){
    current_attack <- player_data %>% 
      filter(attack_angle == possible_attack)
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

####################################################################################################################################

# Pick the player and filter the data 
current_player <- "Trout,Mike"

player_data <- batter_all %>%
  filter(year == 2019, player_name == current_player) %>% 
  left_join(attack_angles, by = c("player_name", "year")) %>%
  clean_edges()

player_woba <- mean(player_data$woba_value, na.rm = TRUE)

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




