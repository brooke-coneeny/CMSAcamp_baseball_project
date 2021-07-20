library(tidyverse)
library(glmnet)
library(mgcv)

### Load in data 
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019 <- batter_all_2019 %>%
  filter(description == "hit_into_play")

### Our GAM Model for reference:
woba_model_interaction <- gam(woba_value ~ s(launch_speed, launch_angle, k = 200), 
                              data = batter_all_2019, method = "REML")

# create a linear model of launch angle ~ pitch height + attack angle 

# Create a column in data with the corresponding attack angle for each player
batter_all_2019 <- batter_all_2019 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name")) 

# Create linear model with the variables 
predicted_LA <- glm(launch_angle ~ attack_angle + plate_z, data = batter_all_2019)


# Update attack angle and compare woba values 

set.seed(2021)
# Create empty vectors of exit velocities 
EV_vector1 <- vector()    #for no change in attack angle
EV_vector2 <- vector()    #for plus one attack angle
EV_vector3 <- vector()    #for minus one attack angle

#global variable
change_in_attack <- 0

# Pass in the woba model, the LA model, the player's data for the year (the attack angle will be changed 
#in the recursive calls), the players original woba for output purposes, the original attack angle for 
#output purposes, and the attack angle which will be modified in the recursive call
predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack, change_in_attack){
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles$lm.preds)){
    # Filter for the player's launch angles plus or minus 3 degrees above the predicted LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= pred_angles$lm.preds[i]+3 & launch_angle >= pred_angles$lm.preds[i]-3)
    # Sample those hits, 10 for each predicted angle and take mean launch speed of those
    EV_sample_index <- sample(1:nrow(hits_at_angle), 10, replace = TRUE)
    pred_EV <- player_data[EV_sample_index,] %>% summarize(the_EV = mean(launch_speed))
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector1 <- c(EV_vector1, pred_EV$the_EV[1])
  }
  
  modeled_data <- tibble(launch_angle = pred_angles$lm.preds, launch_speed = EV_vector1)
  preds1 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
  xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)
  
  # Repeat with a +1 attack angle
  plus_one_attack <- player_data
  plus_one_attack$attack_angle <- plus_one_attack$attack_angle + 1
  
  pred_angles2 <- tibble(lm.preds = predict(LA_model, newdata = plus_one_attack))
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles2$lm.preds)){
    # Filter for the player's launch angles plus or minus 3 degrees above the predicted LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= pred_angles2$lm.preds[i]+3 & launch_angle >= pred_angles2$lm.preds[i]-3)
    # Sample those hits, 10 for each predicted angle and take mean launch speed of those
    EV_sample_index <- sample(1:nrow(hits_at_angle), 10, replace = TRUE)
    pred_EV <- player_data[EV_sample_index,] %>% summarize(the_EV = mean(launch_speed))
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector2 <- c(EV_vector2, pred_EV$the_EV[1])
  }
  
  modeled_data_plus_one <- tibble(launch_angle = pred_angles2$lm.preds, launch_speed = EV_vector2)
  preds2 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_plus_one))  
  xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)
  
  # Repeat with a -1 attack angle
  minus_one_attack <- player_data
  minus_one_attack$attack_angle <- minus_one_attack$attack_angle - 1
  
  pred_angles3 <- tibble(lm.preds = predict(LA_model, newdata = minus_one_attack))
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles3$lm.preds)){
    # Filter for the player's launch angles plus or minus 3 degrees above the predicted LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= pred_angles3$lm.preds[i]+3 & launch_angle >= pred_angles3$lm.preds[i]-3)
    # Sample those hits, 10 for each predicted angle and take mean launch speed of those
    EV_sample_index <- sample(1:nrow(hits_at_angle), 10, replace = TRUE)
    pred_EV <- player_data[EV_sample_index,] %>% summarize(the_EV = mean(launch_speed))
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector3 <- c(EV_vector3, pred_EV$the_EV[1])
  }
  
  modeled_data_minus_one <- tibble(launch_angle = pred_angles3$lm.preds, launch_speed = EV_vector3)
  preds3 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_minus_one))  
  xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
  
  # If original < +1
  if(xwOBA1 < xwOBA2){
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
    # Return the orig_woba, xwOBA, orig_attack, and attack angles
    return (tibble(original_woba = orig_woba, woba = xwOBA1, original_attack = orig_attack, 
                   reccomended_attack = attack, change_in_attack = change_in_attack)[1,])
  }
  
}

# Using Mike Trout to test b/c it should be short
mtrout <- batter_all_2019 %>%
  filter(player_name == "Trout, Mike")
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(woba_model_interaction, predicted_LA, mtrout, mtrout_woba, mtrout$attack_angle, 
                           mtrout$attack_angle, 0)
