# Purpose: using the equation we found in the blog, holding walks and hit by pitch constant
# The equation we want to use
# expected_woba = woba_con / at_bats + sac_flies

# Library
library(tidyverse)
library(glmnet)
library(mgcv)


# Load in data 
trout_data <- read_rds("public_data/mtrout_sample_hits.rds")
heyward_data <- read_rds("public_data/jhey_sample_hits.rds")

# Load in the models
woba_model <- read_rds("public_data/woba_model.rds")
linear_model <- read_rds("public_data/LA_model.rds")

####################################################################################################################################

# Find the number of sac flies at each attack angle 
sac_fly_data <- heyward_data %>%
  mutate(event_sac_fly = case_when(events == "sac_fly" ~ 1, 
                                   TRUE ~0)) %>%
  group_by(attack_angle, event_sac_fly) %>%
  count() %>%
  pivot_wider(id_cols = attack_angle, names_from = event_sac_fly, values_from = n) %>%
  rename(`sac`=`1`, `other`=`0`) %>%
  mutate(sacs_per_hip = sac/(sac+other)) %>%
  select(attack_angle, sacs_per_hip)

# Add this column to data set 
batter_data <- heyward_data %>%
  left_join(sac_fly_data, by=c("attack_angle")) %>%
  filter(!is.na(events))

####################################################################################################################################

clean_edges <- function (data){
  for(i in 1:length(data$launch_angle)){
    if(data$launch_angle[i] < (mean(data$launch_angle)-2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)-2*sd(data$launch_angle))
    }
    else if(data$launch_angle[i] > (mean(data$launch_angle)+2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)+2*sd(data$launch_angle))
    }
    else{
      data$cleaned_launch_angle[i]<-data$launch_angle[i]
    }
  }
  return (data)
}

####################################################################################################################################

sampling <- function(pred_angles, player_data, EV_vector, orig_attack){
  # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with the cap (plus or minus)
  for(i in 1:length(pred_angles$launch_angle)){
    if(pred_angles$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
    }
    else if(pred_angles$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
    }
    else{
      pred_angles$launch_angle[i] <- pred_angles$launch_angle[i]
    }
    
    # Need to sample the data for each predicted angle to find what exit velocity we would give it 
    # Filter for the player's launch angles plus or minus 3 degrees around the attack angle 
    hits_at_angle <- player_data %>% 
      filter(cleaned_launch_angle <= orig_attack +3 & cleaned_launch_angle >= orig_attack -3 & !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector <- c(EV_vector, pred_EV$launch_speed)
  }
  return(EV_vector)
}

####################################################################################################################################

set.seed(2021)

# Create empty vectors of exit velocities 
EV_vector1 <- vector()    #for no change in attack angle
EV_vector2 <- vector()    #for plus one attack angle
EV_vector3 <- vector()    #for minus one attack angle

# global variable to monitor change in attack angle 
change_in_attack <- 0

predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack, change_in_attack){
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, sd = sigma(LA_model)),
                                        launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles 
  EV_vector1 <- sampling(pred_angles, player_data, EV_vector1, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector1)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds1 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
  
  # Find the mean expected woba value for the batter 
  xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)
  
  ##################################################
  # Repeat with a one degree increase in attack angle
  plus_one_attack <- player_data
  plus_one_attack$attack_angle <- plus_one_attack$attack_angle + 1
  
  # Model the predicted launch angles given the new increased attack angle
  pred_angles2 <- tibble(lm.preds = predict(LA_model, newdata = plus_one_attack))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles2 <- pred_angles2 %>% mutate(noise = rnorm(n = length(pred_angles2$lm.preds), mean = 0, sd = sigma(LA_model)),
                                          launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles 
  EV_vector2 <- sampling(pred_angles2, player_data, EV_vector2, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data_plus_one <- tibble(launch_angle = pred_angles2$launch_angle, launch_speed = EV_vector2)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds2 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_plus_one))  
  
  # Find the mean expected woba value for the batter 
  xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)
  
  ##################################################
  # Repeat with a -1 attack angle
  minus_one_attack <- player_data
  minus_one_attack$attack_angle <- minus_one_attack$attack_angle - 1
  
  # Model the predicted angles given the new decreased attack angle
  pred_angles3 <- tibble(lm.preds = predict(LA_model, newdata = minus_one_attack))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles3 <- pred_angles3 %>% mutate(noise = rnorm(n = length(pred_angles3$lm.preds), 
                                                        mean = 0, sd = sigma(LA_model)),launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles
  EV_vector3 <- sampling(pred_angles3, player_data, EV_vector3, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data_minus_one <- tibble(launch_angle = pred_angles3$launch_angle, launch_speed = EV_vector3)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds3 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_minus_one)) 
  
  # Find the mean expected woba value for the batter 
  xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
  
  ##################################################
  
  # Comparing the different expected woba values 
  # If original < +1 and the +! woba is greater than the -1 woba
  if(xwOBA1 < xwOBA2 & xwOBA2 > xwOBA3){
    # Recursively call with +1 attack angle data
    change_in_attack <- change_in_attack + 1
    predicted_LA_adjust_attack(woba_model, LA_model, plus_one_attack, orig_woba, orig_attack, attack+1, change_in_attack)
  }
  # Else if original < -1
  else if (xwOBA1 < xwOBA3){
    change_in_attack <- change_in_attack - 1
    # Recursively call with -1 attack angle data
    predicted_LA_adjust_attack(woba_model, LA_model, minus_one_attack, orig_woba, orig_attack, attack-1, change_in_attack)
  }
  # Else
  else{
    change_in_attack <- change_in_attack
    # Return the orig_woba, xwOBA, orig_attack, and attack angles
    return (tibble(original_woba = orig_woba, predicted_woba = xwOBA1, original_attack = orig_attack, 
                   reccomended_attack = attack, change_in_attack = change_in_attack)[1,])
  }
}

####################################################################################################################################

# Getting predicted woba values 
batter_data <- batter_data %>%
  filter(!is.na(plate_z), !is.na(launch_angle), !is.na(launch_speed)) %>%
  clean_edges()

batter_woba <- mean(batter_data$woba_value, na.rm = TRUE)

predicted_values <- as.data.frame(predicted_LA_adjust_attack(woba_model, linear_model, batter_data, batter_woba, batter_data$attack_angle, 
                           batter_data$attack_angle, 0))

predicted_woba <- predicted_values$predicted_woba

####################################################################################################################################





