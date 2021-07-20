#Attack angle and height of pitch —> model launch angle
#Get predicted launch angle, figure out what to use as exit velocity by sampling from player’s actual 
  #batted balls

library(tidyverse)

# Data --------------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")


# Create model for attack angle and height predicting LA ------------------

# Need to calculate the attack angle for every at bat (needs to be the same for each individual player)
batter_all_2019hp <- batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019hp, by = c("player_name"))

# Graph relationships between variables and LA
batter_all_2019hp %>%
  ggplot(aes(x=attack_angle, y=launch_angle))+
  geom_smooth()

batter_all_2019hp %>%
  ggplot(aes(x=plate_z, y=launch_angle))+
  geom_smooth()
hist(batter_all_2019hp$plate_z)

predicted_LA <- lm(launch_angle ~ attack_angle + plate_z, data=batter_all_2019hp)
summary(predicted_LA)

predicted_LA2 <- gam(launch_angle ~ s(attack_angle) + s(plate_z), data = batter_all_2019hp)
gam.check(predicted_LA2)

predicted_LA3 <- gam(launch_angle ~ s(attack_angle, plate_z, k=75), data = batter_all_2019hp)
gam.check(predicted_LA3) #worse but might need a higher k?

# Final Model -------------------------------------------------------------

final_woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")


# Update attack angle -----------------------------------------------------

set.seed(2021)
# Create empty vectors of exit velocities 
EV_vector1 <- vector()    #for no change in attack angle
EV_vector2 <- vector()    #for plus one attack angle
EV_vector3 <- vector()    #for minus one attack angle

# Pass in the woba model, the LA model, the player's data for the year (the attack angle will be changed 
#in the recursive calls), the players original woba for output purposes, the original attack angle for 
#output purposes, and the attack angle which will be modified in the recursive call
predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack)
  {
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
    EV_sample_index <- sample(1:nrow(hits_at_angle), 50, replace = TRUE)
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
    predicted_LA_adjust_attack(woba_model, LA_model, plus_one_attack, orig_woba, orig_attack, attack+1)
  }
  # Else if original < -1
  else if (xwOBA1 < xwOBA3){
    # Recursively call with -1 attack angle data
    predicted_LA_adjust_attack(woba_model, LA_model, minus_one_attack, orig_woba, orig_attack, attack-1)
  }
  # Else
  else{
    # Return the orig_woba, xwOBA, orig_attack, and attack angles
    return (tibble(original_woba = orig_woba, predicted_woba = xwOBA1, original_attack = orig_attack, 
                   reccomended_attack = attack)[1,])
  }
  
}

# Using Jason Heyward to test
jhey <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason")
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, jhey, jhey_woba, jhey$attack_angle, 
                           jhey$attack_angle)

# Using Mike Trout to test b/c it should be short
mtrout <- batter_all_2019hp %>%
  filter(player_name == "Trout, Mike")
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, mtrout, mtrout_woba, mtrout$attack_angle, 
                           mtrout$attack_angle)

# Using Joey Gallo to test 
jgallo <- batter_all_2019hp %>%
  filter(player_name == "Gallo, Joey")
jgallo_woba <- mean(jgallo$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, jgallo, jgallo_woba, jgallo$attack_angle, 
                           jgallo$attack_angle)

# Using Davis, Khris to test - only increases by 1 degree but MASSIVE difference in wOBA (making us think
#the glm for predicting launch angle isn't doing that well so maybe switch to GAM?) DIDN'T HELP
kdavis <- batter_all_2019hp %>%
  filter(player_name == "Davis, Khris")
kdavis_woba <- mean(kdavis$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, kdavis, kdavis_woba, kdavis$attack_angle, 
                           kdavis$attack_angle)
# This is telling us the woba model isn't terrible at predicting Kris Davis so the problems we are having
  #is with using the woba model on the mock data made though the LA model and sampling of EVs
woba_kdavis <- tibble(gam.preds = predict(final_woba_model2, newdata = kdavis))
kdavis_pred_woba <- mean(woba_kdavis$gam.preds, na.rm=TRUE)

# Using Ramos, Wilson to test 
wramos <- batter_all_2019hp %>%
  filter(player_name == "Ramos, Wilson")
wramos_woba <- mean(wramos$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, wramos, wramos_woba, wramos$attack_angle, 
                           wramos$attack_angle)

#ISSUE - IT SEEMS TO BE GIVING A .6 WOBA TO ATTACK ANGLES IN THE DOUBLE DIGITS SO IT CUTS OFF

